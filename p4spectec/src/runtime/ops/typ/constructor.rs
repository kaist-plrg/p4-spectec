//! Constructor queries over internal-language types

use crate::lang::{common::ds::map::ArityMismatch, il::ast};
use crate::runtime::{envs::elab::TDEnv, typdef::TypeDef};

use super::{TypeArityMismatch, TypeError, TypeErrorKind, expand_typ};

/// Checks whether a type has exactly one constructor, through aliases.
pub fn has_single_constructor(tdenv: &TDEnv, typ: &ast::Typ) -> Result<bool, TypeError> {
    // Instantiate alias parameters before inspecting the nominal type
    let typ = expand_typ(tdenv, typ)?;
    let ast::TypKind::Var(id, targs) = &typ.node else {
        return Ok(false);
    };
    // Parameters and extern types do not expose constructors
    let typdef = tdenv.get(id).ok_or_else(|| {
        TypeError::new(TypeErrorKind::TypeUndefined(id.node.to_string()), typ.span)
    })?;
    let TypeDef::Defined(tparams, def_typ) = typdef else {
        return Ok(false);
    };
    // Direct IL callers must supply the declared type arguments
    if tparams.len() != targs.len() {
        let arity_mismatch = ArityMismatch::new(tparams.len(), targs.len());
        let arity_mismatch = TypeArityMismatch::TypeArgument(arity_mismatch);
        return Err(TypeError::new(TypeErrorKind::ArityMismatch(arity_mismatch), typ.span));
    }
    // Only a variant with one case is a singleton constructor type
    let ast::DefTypKind::Variant(cases) = &def_typ.node else {
        return Ok(false);
    };
    Ok(cases.len() == 1)
}
