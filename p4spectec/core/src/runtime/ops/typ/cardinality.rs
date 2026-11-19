//! Inhabitant cardinality queries over internal-language types
//!
//! `cardinality` classifies values, including constructor payloads
//! With `syntax unit = UNIT` and `syntax box<T> = BOX T`,
//! `unit` and `box<unit>` are `One`, while `box<nat>` is `Many`;
//! both box types have exactly one constructor
//!
//! `unit?` and `unit*` are `Many`: empty and nonempty values differ
//! For `syntax void = VOID void`, recursion yields `Unsure`
//! Opaque types and unsure components also yield `Unsure`

use crate::lang::{
    common::ds::{
        map::{ArityMismatch, IdMap},
        set::IdSet,
    },
    il::ast,
};

use crate::runtime::{envs::elab::TDEnv, typdef::TypeDef};

use super::{TypeArityMismatch, TypeError, TypeErrorKind};

// == Cardinality

/// A conservative classification of the number of values inhabiting a type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cardinality {
    /// Exactly one value inhabits the type.
    One,
    /// At least two values inhabit the type.
    Many,
    /// Neither classification is established; the type may also be empty.
    Unsure,
}

// == Type cardinality

/// Classifies inhabitants, treating recursive and opaque components as unsure.
///
/// Unlike `has_single_constructor`, this inspects constructor payloads.
/// An unsure component propagates through products, variants, and iterations.
pub fn cardinality(tdenv: &TDEnv, typ: &ast::Typ) -> Result<Cardinality, TypeError> {
    cardinality_inner(tdenv, &IdMap::new(), &IdSet::new(), typ)
}

/// Classifies a type with parameter cardinalities and the active definition path.
fn cardinality_inner(
    tdenv: &TDEnv,
    cenv: &IdMap<Cardinality>,
    ids_active: &IdSet,
    typ: &ast::Typ,
) -> Result<Cardinality, TypeError> {
    match &typ.node {
        ast::TypKind::Bool | ast::TypKind::Num(_) | ast::TypKind::Text => Ok(Cardinality::Many),
        ast::TypKind::Var(id, targs) => {
            cardinality_var_typ(tdenv, cenv, ids_active, typ, id, targs)
        }
        ast::TypKind::Tuple(typs) => cardinality_tuple_typ(tdenv, cenv, ids_active, typs),
        ast::TypKind::Iter(typ_inner, _) => {
            cardinality_iter_typ(tdenv, cenv, ids_active, typ_inner)
        }
        ast::TypKind::Func(_) => Ok(Cardinality::Unsure),
    }
}

// - Type variables

/// Resolves a named type without unfolding a recursive definition twice.
fn cardinality_var_typ(
    tdenv: &TDEnv,
    cenv: &IdMap<Cardinality>,
    ids_active: &IdSet,
    typ: &ast::Typ,
    id: &ast::Id,
    targs: &[ast::Typ],
) -> Result<Cardinality, TypeError> {
    // Instantiated parameters shadow global type names
    if let Some(cardinality_param) = cenv.get(id) {
        if !targs.is_empty() {
            return Err(TypeError::new(
                TypeErrorKind::HigherOrderSubstitutionUnsupported,
                typ.span,
            ));
        }
        return Ok(*cardinality_param);
    }
    // Externs, parameters, and incomplete definitions leave cardinality unsure
    let typdef = tdenv.get(id).ok_or_else(|| {
        TypeError::new(TypeErrorKind::TypeUndefined(id.node.to_string()), typ.span)
    })?;
    let TypeDef::Defined(tparams, def_typ) = typdef else {
        return Ok(Cardinality::Unsure);
    };
    if tparams.len() != targs.len() {
        let arity_mismatch = ArityMismatch::new(tparams.len(), targs.len());
        let arity_mismatch = TypeArityMismatch::TypeArgument(arity_mismatch);
        return Err(TypeError::new(TypeErrorKind::ArityMismatch(arity_mismatch), typ.span));
    }
    // Track names so recursion also terminates when type arguments grow
    if ids_active.contains(id) {
        return Ok(Cardinality::Unsure);
    }
    // Evaluate arguments in the caller scope before entering this definition
    let cardinalities = targs
        .iter()
        .map(|typ| cardinality_inner(tdenv, cenv, ids_active, typ))
        .collect::<Result<Vec<_>, _>>()?;
    let cenv = tparams.iter().cloned().zip(cardinalities).collect();
    let mut ids_active = ids_active.clone();
    ids_active.insert(id.clone());
    cardinality_def_typ(tdenv, &cenv, &ids_active, def_typ)
}

// - Tuples

/// Classifies tuple components, also used for fields and constructor payloads.
fn cardinality_tuple_typ<'a>(
    tdenv: &TDEnv,
    cenv: &IdMap<Cardinality>,
    ids_active: &IdSet,
    typs: impl IntoIterator<Item = &'a ast::Typ>,
) -> Result<Cardinality, TypeError> {
    let mut cardinality_all = Cardinality::One;
    // A product is inhabited only when every component is known to be inhabited
    for typ in typs {
        match cardinality_inner(tdenv, cenv, ids_active, typ)? {
            // An unsure component determines the whole product
            Cardinality::Unsure => return Ok(Cardinality::Unsure),
            // Multiple values in any inhabited component make a product non-singleton
            Cardinality::Many => cardinality_all = Cardinality::Many,
            // A singleton component does not change the count
            Cardinality::One => {}
        }
    }
    Ok(cardinality_all)
}

// - Iterations

/// Classifies an iteration from its element type's inhabitants.
fn cardinality_iter_typ(
    tdenv: &TDEnv,
    cenv: &IdMap<Cardinality>,
    ids_active: &IdSet,
    typ_inner: &ast::Typ,
) -> Result<Cardinality, TypeError> {
    // An inhabited element type admits both empty and nonempty iterations
    Ok(match cardinality_inner(tdenv, cenv, ids_active, typ_inner)? {
        Cardinality::One | Cardinality::Many => Cardinality::Many,
        Cardinality::Unsure => Cardinality::Unsure,
    })
}

// == Definition cardinality

/// Classifies a definition under its instantiated parameter cardinalities.
fn cardinality_def_typ(
    tdenv: &TDEnv,
    cenv: &IdMap<Cardinality>,
    ids_active: &IdSet,
    def_typ: &ast::DefTyp,
) -> Result<Cardinality, TypeError> {
    match &def_typ.node {
        ast::DefTypKind::Plain(typ) => cardinality_inner(tdenv, cenv, ids_active, typ),
        ast::DefTypKind::Struct(fields) => cardinality_struct_typ(tdenv, cenv, ids_active, fields),
        ast::DefTypKind::Variant(cases) => cardinality_variant_typ(tdenv, cenv, ids_active, cases),
    }
}

// - Structs

/// Classifies a struct as the product of its field types.
fn cardinality_struct_typ(
    tdenv: &TDEnv,
    cenv: &IdMap<Cardinality>,
    ids_active: &IdSet,
    fields: &[ast::TypField],
) -> Result<Cardinality, TypeError> {
    cardinality_tuple_typ(tdenv, cenv, ids_active, fields.iter().map(|field| &field.typ))
}

// - Variants

/// Classifies a variant by combining the inhabitants of its cases.
fn cardinality_variant_typ(
    tdenv: &TDEnv,
    cenv: &IdMap<Cardinality>,
    ids_active: &IdSet,
    cases: &[ast::TypCase],
) -> Result<Cardinality, TypeError> {
    // Distinct cases add inhabitants, but unsure payloads remain conservative
    let mut cardinality_cases = None;
    for case in cases {
        let cardinality_case =
            cardinality_tuple_typ(tdenv, cenv, ids_active, case.not_typ.node.args())?;
        cardinality_cases = Some(match (cardinality_cases, cardinality_case) {
            (_, Cardinality::Unsure) => return Ok(Cardinality::Unsure),
            (None, cardinality_case) => cardinality_case,
            (Some(_), _) => Cardinality::Many,
        });
    }
    // Direct IL may contain an empty variant, which has no represented count
    Ok(cardinality_cases.unwrap_or(Cardinality::Unsure))
}
