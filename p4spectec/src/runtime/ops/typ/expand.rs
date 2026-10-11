//! Runtime expansion of internal-language type aliases
//!
//! Only plain aliases expand;
//! struct and variant definitions are types of their own.
//! Expansion recurses, so a chain of aliases resolves to its last type.

use std::borrow::Cow;

use crate::lang::il::ast::{self, DefTypKind, TypKind};

use crate::runtime::{envs::elab::TDEnv, typdef::TypeDef};

use super::{Theta, TypeArityMismatch, TypeError, TypeErrorKind, subst_typ};

/// Expands plain type aliases until a non-alias type is reached.
pub fn expand_typ<'a>(tdenv: &TDEnv, typ: &'a ast::Typ) -> Result<Cow<'a, ast::Typ>, TypeError> {
    let find_typdef_opt = |id: &ast::Id| tdenv.get(id);
    expand_typ_with(&find_typdef_opt, typ)
}

/// Expands through a lookup closure, so callers can layer local environments.
pub(super) fn expand_typ_with<'a, 'env, P: ast::Stage>(
    find_typdef_opt: &impl Fn(&ast::Id) -> Option<&'env TypeDef<P>>,
    typ: &'a ast::Typ,
) -> Result<Cow<'a, ast::Typ>, TypeError> {
    // Only type variables can be aliases
    let TypKind::Var(id, targs) = &typ.node else {
        return Ok(Cow::Borrowed(typ));
    };
    // An unknown type name is an error, not a non-alias
    let Some(typdef) = find_typdef_opt(id) else {
        let error_kind = TypeErrorKind::TypeUndefined(id.node.clone());
        let error = TypeError::new(error_kind, typ.span.clone());
        return Err(error);
    };
    // Parameters and externs are opaque
    let TypeDef::Defined(tparams, deftyp) = typdef else {
        return Ok(Cow::Borrowed(typ));
    };
    // Structs and variants are nominal
    let DefTypKind::Plain(typ_alias) = &deftyp.node else {
        return Ok(Cow::Borrowed(typ));
    };
    // Type arguments must match the alias parameters
    let theta = match Theta::from_lists(tparams, targs) {
        Ok(theta) => theta,
        Err(arity_mismatch) => {
            let arity_mismatch = TypeArityMismatch::TypeArgument(arity_mismatch);
            let error_kind = TypeErrorKind::ArityMismatch(arity_mismatch);
            let error = TypeError::new(error_kind, typ.span.clone());
            return Err(error);
        }
    };
    // Substitute, then keep expanding the result
    let typ_expanded = subst_typ(&|id| theta.get(id), typ_alias)?;
    // Borrowed means the substituted type was already final
    let typ_expanded = match expand_typ_with(find_typdef_opt, &typ_expanded)? {
        Cow::Borrowed(_) => typ_expanded,
        Cow::Owned(typ_expanded) => typ_expanded,
    };
    Ok(Cow::Owned(typ_expanded))
}
