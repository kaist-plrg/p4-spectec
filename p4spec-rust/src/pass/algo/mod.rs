//! Internal-to-algorithmic language conversion
//!
//! `convert` runs two passes over an IL specification.
//!
//! Binding lowering (`binding::transform::lower_spec`)
//! decides which variables each premise binds
//! and rewrites binder patterns into explicit let and match premises.
//!
//! Guard insertion (`sidecondition::guard::insert_spec`)
//! then adds side conditions that make partial operations such as `a[n]` safe.

mod error;
mod sidecondition;

mod binding;

use crate::lang::il;

use crate::lang::al;

pub use error::AlgoError;

// == Entry point

/// Converts an IL specification to AL.
pub fn convert(spec_il: il::ast::Spec) -> Result<al::ast::Spec, AlgoError> {
    let spec_al = binding::transform::lower_spec(spec_il)?;
    Ok(sidecondition::guard::insert_spec(spec_al))
}
