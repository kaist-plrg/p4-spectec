//! Runtime subtyping for internal-language types
//!
//! Subtyping expands aliases, tries equivalence, then structural rules:
//! numbers by `num::sub`, variants by case inclusion, tuples componentwise,
//! an option into a list, and a plain value into an option or list.
//! `optimize_sub_typ` reduces a static subtype relation
//! to the runtime check still needed.

use std::rc::Rc;

use crate::lang::common::prim::num;

use crate::lang::il::ast::{self, DefTypKind, Iter, Subcheck, TypKind};

use crate::runtime::{envs::elab::TDEnv, typdef::TypeDef};

use super::{
    Fresh, Theta, TypeArityMismatch, TypeError, TypeErrorKind, equiv_not_typ, equiv_typ_expanded,
    expand_typ, subst_not_typ_inner,
};

// == Subtyping

/// Tests whether the source type is a subtype of the target type.
pub fn sub_typ(
    tdenv: &TDEnv,
    typ_source: &ast::Typ,
    typ_target: &ast::Typ,
) -> Result<bool, TypeError> {
    let typ_source = expand_typ(tdenv, typ_source)?;
    let typ_target = expand_typ(tdenv, typ_target)?;
    sub_typ_expanded(tdenv, &typ_source, &typ_target)
}

/// Subtyping of expanded types: equivalence first, then the structural rules.
fn sub_typ_expanded(
    tdenv: &TDEnv,
    typ_source: &ast::Typ,
    typ_target: &ast::Typ,
) -> Result<bool, TypeError> {
    if equiv_typ_expanded(tdenv, typ_source, typ_target)? {
        return Ok(true);
    }
    sub_typ_inner(tdenv, typ_source, typ_target)
}

/// The structural subtyping rules.
fn sub_typ_inner(
    tdenv: &TDEnv,
    typ_source: &ast::Typ,
    typ_target: &ast::Typ,
) -> Result<bool, TypeError> {
    match (&typ_source.node, &typ_target.node) {
        // Numeric types by their own subtyping
        (TypKind::Num(num_typ_source), TypKind::Num(num_typ_target)) => {
            let is_sub = num::sub(*num_typ_source, *num_typ_target);
            Ok(is_sub)
        }
        // Two variants: every source case must have an equivalent target case
        (TypKind::Var(id_source, targs_source), TypKind::Var(id_target, targs_target)) => {
            // Both must be defined types
            let Some(TypeDef::Defined(tparams_source, deftyp_source)) = tdenv.get(id_source) else {
                return Ok(false);
            };
            let Some(TypeDef::Defined(tparams_target, deftyp_target)) = tdenv.get(id_target) else {
                return Ok(false);
            };
            // Both must be variants
            let DefTypKind::Variant(typ_cases_source) = &deftyp_source.node else {
                return Ok(false);
            };
            let DefTypKind::Variant(typ_cases_target) = &deftyp_target.node else {
                return Ok(false);
            };
            // Type arguments must match on each side
            let theta_source = match Theta::from_lists(tparams_source, targs_source) {
                Ok(theta) => theta,
                Err(arity_mismatch) => {
                    let arity_mismatch = TypeArityMismatch::TypeArgument(arity_mismatch);
                    let error_kind = TypeErrorKind::ArityMismatch(arity_mismatch);
                    let error = TypeError::new(error_kind, typ_source.span.clone());
                    return Err(error);
                }
            };
            let theta_target = match Theta::from_lists(tparams_target, targs_target) {
                Ok(theta) => theta,
                Err(arity_mismatch) => {
                    let arity_mismatch = TypeArityMismatch::TypeArgument(arity_mismatch);
                    let error_kind = TypeErrorKind::ArityMismatch(arity_mismatch);
                    let error = TypeError::new(error_kind, typ_target.span.clone());
                    return Err(error);
                }
            };

            // Instantiate the cases of both sides
            let mut fresh = Fresh::default();
            let mut not_typs_source = Vec::with_capacity(typ_cases_source.len());
            for ast::TypCase { not_typ, .. } in typ_cases_source {
                let not_typ_subst =
                    subst_not_typ_inner(&mut fresh, &|id| theta_source.get(id), not_typ)?;
                not_typs_source.push(not_typ_subst);
            }
            let mut not_typs_target = Vec::with_capacity(typ_cases_target.len());
            for ast::TypCase { not_typ, .. } in typ_cases_target {
                let not_typ_subst =
                    subst_not_typ_inner(&mut fresh, &|id| theta_target.get(id), not_typ)?;
                not_typs_target.push(not_typ_subst);
            }

            // Each source case needs an equivalent target case
            for not_typ_source in not_typs_source {
                let mut has_equiv = false;
                for not_typ_target in &not_typs_target {
                    if equiv_not_typ(tdenv, &not_typ_source, not_typ_target)? {
                        has_equiv = true;
                        break;
                    }
                }
                if !has_equiv {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        // Tuples: same length, componentwise
        (TypKind::Tuple(typs_source), TypKind::Tuple(typs_target)) => {
            if typs_source.len() != typs_target.len() {
                return Ok(false);
            }
            for (typ_source, typ_target) in typs_source.iter().zip(typs_target) {
                if !sub_typ(tdenv, typ_source, typ_target)? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        // Same iteration, or an option into a list, on subtype elements
        (TypKind::Iter(typ_source, iter_source), TypKind::Iter(typ_target, iter_target))
            if iter_source == iter_target
                || (*iter_source == Iter::Opt && *iter_target == Iter::List) =>
        {
            sub_typ(tdenv, typ_source, typ_target)
        }
        // A plain value lifts into an option or list
        (_, TypKind::Iter(typ_target, Iter::Opt | Iter::List)) => {
            sub_typ(tdenv, typ_source, typ_target)
        }
        // No rule applies
        _ => Ok(false),
    }
}

// == Subtype checks

/// Builds the least runtime subtype check needed after static subtyping.
pub fn optimize_sub_typ(
    tdenv: &TDEnv,
    typ_source: &ast::Typ,
    typ_target: &ast::Typ,
) -> Result<Subcheck, TypeError> {
    let typ_source_expanded = expand_typ(tdenv, typ_source)?;
    let typ_target_expanded = expand_typ(tdenv, typ_target)?;
    // A static subtype needs no runtime check
    if sub_typ_expanded(tdenv, &typ_source_expanded, &typ_target_expanded)? {
        let subcheck = Subcheck::Skip;
        return Ok(subcheck);
    }

    match (&typ_source_expanded.node, &typ_target_expanded.node) {
        // Tuples: check each component
        (TypKind::Tuple(typs_source), TypKind::Tuple(typs_target))
            if typs_source.len() == typs_target.len() =>
        {
            let mut subchecks = Vec::with_capacity(typs_source.len());
            for (typ_source, typ_target) in typs_source.iter().zip(typs_target) {
                let subcheck = optimize_sub_typ(tdenv, typ_source, typ_target)?;
                subchecks.push(subcheck);
            }
            let subcheck = Subcheck::Tuple(subchecks);
            Ok(subcheck)
        }
        // Same iteration: check each element
        (TypKind::Iter(typ_source, iter_source), TypKind::Iter(typ_target, iter_target))
            if iter_source == iter_target =>
        {
            let subcheck = optimize_sub_typ(tdenv, typ_source, typ_target)?;
            let subcheck = Box::new(subcheck);
            let subcheck = Subcheck::Iter(*iter_source, subcheck);
            Ok(subcheck)
        }
        // Target is a subvariant of the source: check the case tag only
        (TypKind::Var(id_source, _), TypKind::Var(id_target, _))
            if sub_typ_expanded(tdenv, &typ_target_expanded, &typ_source_expanded)? =>
        {
            // Anything but two variants falls back to a full check
            let Some(TypeDef::Defined(_, deftyp_source)) = tdenv.get(id_source) else {
                let subcheck = Subcheck::Recurse(typ_target.clone());
                return Ok(subcheck);
            };
            let Some(TypeDef::Defined(_, deftyp_target)) = tdenv.get(id_target) else {
                let subcheck = Subcheck::Recurse(typ_target.clone());
                return Ok(subcheck);
            };
            let DefTypKind::Variant(_) = &deftyp_source.node else {
                let subcheck = Subcheck::Recurse(typ_target.clone());
                return Ok(subcheck);
            };
            let DefTypKind::Variant(typ_cases_target) = &deftyp_target.node else {
                let subcheck = Subcheck::Recurse(typ_target.clone());
                return Ok(subcheck);
            };

            // The target's case tags are the accepted set
            let mut mixops_target = Vec::with_capacity(typ_cases_target.len());
            for ast::TypCase { not_typ, .. } in typ_cases_target {
                mixops_target.push(Rc::clone(not_typ.node.mixop()));
            }
            let subcheck = Subcheck::Mixop(mixops_target);
            Ok(subcheck)
        }
        // Otherwise a full membership test against the target
        _ => {
            let subcheck = Subcheck::Recurse(typ_target.clone());
            Ok(subcheck)
        }
    }
}
