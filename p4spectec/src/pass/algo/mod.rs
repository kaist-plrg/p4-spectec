//! Internal-to-algorithmic language conversion
//!
//! `convert` runs binding normalization and guard insertion.
//! `binding::transform::lower_spec` rewrites patterns into let and match premises.
//! `sidecondition::guard::insert_spec` adds conditions for partial operations.
//! Both passes use `otherwise` to check purity with the IL type annotations.

mod error;
mod otherwise;
mod sidecondition;

mod binding;

use crate::lang::il;

use crate::lang::al;

use crate::phrase;

pub use error::AlgoError;

// == Entry point

/// Converts an IL specification to AL.
pub fn convert(spec_il: il::ast::Spec) -> Result<al::ast::Spec, AlgoError> {
    // Retain type definitions for checks after guard insertion
    let mut ctx = binding::context::Context::new();
    ctx.load(&spec_il);
    // Check normalized bindings before their diagnostic provenance is discarded
    let spec_al = binding::transform::lower_spec(spec_il)?;
    // Check the final premises after redundant guards have been filtered
    sidecondition::guard::insert_spec(&ctx, spec_al)
}

/// Converts only the type definitions and builtin declarations of an IL specification.
///
/// Both are shared by IL and AL, so no rule or clause is converted.
pub fn convert_builtins(spec_il: &il::ast::Spec) -> al::ast::Spec {
    spec_il
        .iter()
        .filter_map(|def_il| {
            let def_kind_al = match &def_il.node {
                il::ast::DefKind::Typ(typ_def) => al::ast::DefKind::Typ(typ_def.clone()),
                il::ast::DefKind::MetaFunc(il::ast::MetaFuncDef::Builtin(builtin_func)) => {
                    al::ast::DefKind::MetaFunc(al::ast::MetaFuncDef::Builtin(builtin_func.clone()))
                }
                _ => return None,
            };
            Some(phrase!(node: def_kind_al, span: def_il.span))
        })
        .collect()
}
