//! Convert structured-language definitions to prose-language definitions
//!
//! `convert` runs five steps over an SL specification:
//! `Context::load` collects `prose*` hints and meta-variable types,
//! `expand` lifts nested calls into let instructions,
//! `transform` converts each definition and splits instructions into tiers,
//! `shorthand` folds common instruction shapes into one prose step,
//! and `stamp` marks each fallible instruction with its failure destination.

mod context;
mod error;
mod expand;
mod shorthand;
mod stamp;
mod transform;

pub use error::ProseError;

use context::Context;

use crate::lang::{pl::ast as pl, sl::ast as sl};

/// Converts a rule-group-preserving SL specification to PL.
///
/// Requires validated SL produced by `structure::convert(spec_al, false)`.
/// Invalid user hints return reports; violated producer invariants panic.
pub fn convert(spec_sl: sl::Spec) -> Result<pl::Spec, ProseError> {
    transform::prosify_spec(spec_sl)
}
