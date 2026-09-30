//! Runtime type membership for executable values
//!
//! `sub` tests whether a value inhabits a type,
//! unfolding definitions through a lookup closure;
//! `check` runs a `Subcheck` that static subtyping left for runtime.
//! Both need a function lookup to type function values.

use num_traits::Signed;
use thiserror::Error;

use crate::{
    lang::{
        common::prim::num::{Number, Typ as NumTyp},
        common::source::Span,
        data::value::{Value, ValueArena, ValueKind},
        il::ast::{DefTypKind, FuncTyp, Id, Iter, Subcheck, Typ, TypCase, TypField, TypKind},
    },
    runtime::{
        ops::typ::{Theta, TypeError, equiv_func_typ, subst_not_typ, subst_typ},
        typdef::TypeDef,
    },
};

// == Errors

/// A failure while testing type membership.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum MatchError {
    /// The type name has no definition.
    #[error("undefined type {name} at {span}")]
    TypeUndefined { name: String, span: Span },

    /// A parameter or unfinished type, which no value inhabits.
    #[error("unexpected type variable at {span}")]
    TypeVariableUnexpected { span: Span },

    /// Type arguments do not match the definition's parameters.
    #[error("expected {expected} type arguments, got {actual} at {span}")]
    TypeArgumentCountMismatch { expected: usize, actual: usize, span: Span },

    /// A function value names an unknown function.
    #[error("undefined function {name} at {span}")]
    FunctionUndefined { name: String, span: Span },

    /// A type operation failed.
    #[error(transparent)]
    Type(#[from] TypeError),
}

// == Type membership

/// Tests whether `value` inhabits `typ`.
pub fn sub<'env, F>(
    arena: &ValueArena,
    find_typdef_opt: &impl Fn(&Id) -> Option<&'env TypeDef>,
    find_func: &F,
    typ: &Typ,
    value: &Value,
) -> Result<bool, MatchError>
where
    F: Fn(&str) -> Option<FuncTyp>,
{
    match &typ.node {
        // Booleans
        TypKind::Bool => Ok(matches!(arena.kind(value), ValueKind::Bool(_))),
        // Naturals: a natural, or a non-negative integer
        TypKind::Num(NumTyp::Nat) => Ok(match arena.kind(value) {
            // Naturals as such
            ValueKind::Num(Number::Nat(_)) => true,
            // Integers when non-negative
            ValueKind::Num(Number::Int(int)) => !int.is_negative(),
            // Anything else
            _ => false,
        }),
        // Integers: any number
        TypKind::Num(NumTyp::Int) => Ok(matches!(arena.kind(value), ValueKind::Num(_))),
        // Text
        TypKind::Text => Ok(matches!(arena.kind(value), ValueKind::Text(_))),
        // A named type: unfold its definition
        TypKind::Var(id, targs) => {
            let typdef = find_typdef_opt(id).ok_or_else(|| MatchError::TypeUndefined {
                name: id.node.clone(),
                span: typ.span.clone(),
            })?;
            match typdef {
                // Nothing inhabits a parameter or an unfinished type
                TypeDef::Parameter | TypeDef::Defining(_) => {
                    Err(MatchError::TypeVariableUnexpected { span: typ.span.clone() })
                }
                // Extern types hold extern values
                TypeDef::Extern => Ok(matches!(arena.kind(value), ValueKind::Extern(_))),
                // A defined type: instantiate, then match the body
                TypeDef::Defined(tparams, def_typ) => {
                    // Type arguments must match the parameters
                    let theta = Theta::from_lists(tparams, targs);
                    let theta =
                        theta.map_err(|mismatch| MatchError::TypeArgumentCountMismatch {
                            expected: mismatch.expected,
                            actual: mismatch.actual,
                            span: typ.span.clone(),
                        })?;
                    match (&def_typ.node, arena.kind(value)) {
                        // An alias: test against the aliased type
                        (DefTypKind::Plain(typ), _) => {
                            let typ = subst_typ(&|id| theta.get(id), typ)?;
                            sub(arena, find_typdef_opt, find_func, &typ, value)
                        }
                        // A struct: same fields, each in its field type
                        (DefTypKind::Struct(typ_fields), ValueKind::Struct(value_fields)) => {
                            if typ_fields.len() != value_fields.len() {
                                return Ok(false);
                            }
                            for (TypField { atom: atom_typ, typ }, (atom_value, value)) in
                                typ_fields.iter().zip(value_fields)
                            {
                                if atom_typ.node != atom_value.node {
                                    return Ok(false);
                                }
                                let typ = subst_typ(&|id| theta.get(id), typ)?;
                                if !sub(arena, find_typdef_opt, find_func, &typ, value)? {
                                    return Ok(false);
                                }
                            }
                            Ok(true)
                        }
                        // A variant: a same-shaped case accepts the arguments
                        (DefTypKind::Variant(typ_cases), ValueKind::Case(value_case)) => {
                            for TypCase { not_typ, .. } in typ_cases {
                                // Skip cases of a different shape
                                if !not_typ.node.eq_shape(value_case) {
                                    continue;
                                }
                                let not_typ = subst_not_typ(&|id| theta.get(id), not_typ)?;
                                let typs = not_typ.node.args();
                                let values = value_case.args();
                                if subs_inner(
                                    arena,
                                    find_typdef_opt,
                                    find_func,
                                    typs.into_iter(),
                                    values.into_iter(),
                                )? {
                                    return Ok(true);
                                }
                            }
                            Ok(false)
                        }
                        // Body and value shapes disagree
                        _ => Ok(false),
                    }
                }
            }
        }
        // Tuples: componentwise
        TypKind::Tuple(typs) => match arena.kind(value) {
            ValueKind::Tuple(values) => {
                subs_inner(arena, find_typdef_opt, find_func, typs.iter(), values.iter())
            }
            // Not a tuple
            _ => Ok(false),
        },
        // Options: absent, or present with the element type
        TypKind::Iter(typ_inner, Iter::Opt) => match arena.kind(value) {
            // A present option checks its payload
            ValueKind::Opt(Some(value)) => sub(arena, find_typdef_opt, find_func, typ_inner, value),
            // An absent option has no payload to check
            ValueKind::Opt(None) => Ok(true),
            // Other value kinds are not options
            _ => Ok(false),
        },
        // Lists: every element in the element type
        TypKind::Iter(typ_inner, Iter::List) => match arena.kind(value) {
            ValueKind::List(values) => {
                for value in values {
                    if !sub(arena, find_typdef_opt, find_func, typ_inner, value)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            // Not a list
            _ => Ok(false),
        },
        // Function values: the named function's type must be equivalent
        TypKind::Func(func_typ) => match arena.kind(value) {
            ValueKind::Func(id) => {
                let func_typ_actual = find_func(&id.node).ok_or_else(|| {
                    MatchError::FunctionUndefined { name: id.node.clone(), span: id.span.clone() }
                })?;
                let equivalent =
                    equiv_func_typ(find_typdef_opt, &typ.span, func_typ, &func_typ_actual)?;
                Ok(equivalent)
            }
            // Not a function value
            _ => Ok(false),
        },
    }
}

/// Tests values against types pairwise.
pub fn subs<'env, F>(
    arena: &ValueArena,
    find_typdef_opt: &impl Fn(&Id) -> Option<&'env TypeDef>,
    find_func: &F,
    typs: &[Typ],
    values: &[Value],
) -> Result<bool, MatchError>
where
    F: Fn(&str) -> Option<FuncTyp>,
{
    subs_inner(arena, find_typdef_opt, find_func, typs.iter(), values.iter())
}

/// Pairwise membership; differing counts fail.
fn subs_inner<'env, 'typ, 'value, F, T, V>(
    arena: &ValueArena,
    find_typdef_opt: &impl Fn(&Id) -> Option<&'env TypeDef>,
    find_func: &F,
    typs: T,
    values: V,
) -> Result<bool, MatchError>
where
    F: Fn(&str) -> Option<FuncTyp>,
    T: ExactSizeIterator<Item = &'typ Typ>,
    V: ExactSizeIterator<Item = &'value Value>,
{
    if typs.len() != values.len() {
        return Ok(false);
    }
    for (typ, value) in typs.zip(values) {
        if !sub(arena, find_typdef_opt, find_func, typ, value)? {
            return Ok(false);
        }
    }
    Ok(true)
}

// == Subtype-check execution

/// Runs a precomputed subtype check on a value.
pub fn check<'env, F>(
    arena: &ValueArena,
    find_typdef_opt: &impl Fn(&Id) -> Option<&'env TypeDef>,
    find_func: &F,
    subcheck: &Subcheck,
    value: &Value,
) -> Result<bool, MatchError>
where
    F: Fn(&str) -> Option<FuncTyp>,
{
    match (subcheck, arena.kind(value)) {
        // Statically known to hold
        (Subcheck::Skip, _) => Ok(true),
        // Variant case: the tag must be one of the accepted
        (Subcheck::Mixop(mixops), ValueKind::Case(value_case)) => {
            Ok(mixops.iter().any(|mixop| mixop.eq_shape(value_case)))
        }
        // Componentwise
        (Subcheck::Tuple(subchecks), ValueKind::Tuple(values)) => {
            if subchecks.len() != values.len() {
                return Ok(false);
            }
            for (subcheck, value) in subchecks.iter().zip(values) {
                if !check(arena, find_typdef_opt, find_func, subcheck, value)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        // An absent option holds
        (Subcheck::Iter(Iter::Opt, _), ValueKind::Opt(None)) => Ok(true),
        // A present option checks its element
        (Subcheck::Iter(Iter::Opt, subcheck), ValueKind::Opt(Some(value))) => {
            check(arena, find_typdef_opt, find_func, subcheck, value)
        }
        // Every element
        (Subcheck::Iter(Iter::List, subcheck), ValueKind::List(values)) => {
            for value in values {
                if !check(arena, find_typdef_opt, find_func, subcheck, value)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        // Full membership test
        (Subcheck::Recurse(typ), _) => sub(arena, find_typdef_opt, find_func, typ, value),
        // Check and value shapes disagree
        _ => Ok(false),
    }
}
