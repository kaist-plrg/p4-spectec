//! Internal-to-algorithmic language conversion
//!
//! `convert` runs binding normalization, guard insertion, and admission.
//! `binding::transform::lower_spec` rewrites patterns into let and match premises.
//! `sidecondition::guard::insert_spec` adds conditions for partial operations.
//! `otherwise::check_spec` then checks complete otherwise branches,
//! using the original IL type annotations and type environment.

mod error;
mod otherwise;
mod sidecondition;

mod binding;

use crate::lang::il;

use crate::lang::al;

pub use error::AlgoError;

// == Entry point

/// Converts an IL specification to AL.
pub fn convert(spec_il: il::ast::Spec) -> Result<al::ast::Spec, AlgoError> {
    // Retain type definitions for the final admission check
    let mut ctx = binding::context::Context::new();
    ctx.load(&spec_il);
    // Finish both lowering passes before checking any otherwise branch
    let spec_al = binding::transform::lower_spec(spec_il)?;
    let spec_al = sidecondition::guard::insert_spec(spec_al);
    // Source and generated premises now have the same representation
    otherwise::check_spec(&ctx, &spec_al)?;
    Ok(spec_al)
}
