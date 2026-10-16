//! Convert structured-language definitions to prose-language definitions
//!
//! `convert` builds prose from an SL specification:
//! `Context::load` collects `prose*` hints and meta-variable types,
//! `expand` lifts nested calls into let instructions,
//! `transform` converts each definition and splits instructions into tiers,
//! `control` normalizes singleton cases and classifies conditions
//! by their remaining continuations,
//! and `shorthand` recognizes hinted destructuring.

mod context;
mod control;
mod error;
mod expand;
mod shorthand;
mod transform;

use crate::lang::sl::ast as sl;

use crate::lang::pl::ast as pl;

use context::Context;

pub use error::ProseError;

/// Converts a rule-group-preserving SL specification to PL.
///
/// Requires validated SL produced by `structure::convert(spec_al, false)`.
/// Invalid user hints return reports; violated producer invariants panic.
pub fn convert(spec_sl: sl::Spec) -> Result<pl::Spec, ProseError> {
    transform::prosify_spec(spec_sl)
}
