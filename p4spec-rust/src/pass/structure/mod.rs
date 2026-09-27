//! Structure AL definitions into SL via the pass-local optimization language
//!
//! `convert` turns each AL relation or function into SL blocks.
//!
//! Rule and clause premises become nested OL instructions (`transform`),
//! shared inputs are anti-unified into one signature (`antiunify`),
//! the blocks are optimized (`opt`), totalized (`totalize`),
//! and prettified (`pretty`),
//! and `dangle` marks where SL execution falls through to the next alternative.

mod antiunify;
mod context;
mod dangle;
mod error;
mod ol;
mod opt;
mod pretty;
mod re;
mod totalize;
mod transform;

pub use error::StructureError;

use crate::lang::{al::ast as al, sl::ast as sl};

// == Entry point

/// Converts validated algorithmic definitions, removing rule groups if requested.
///
/// The input must come from elaboration followed by algorithmic conversion.
/// Those passes establish declaration uniqueness, argument shapes, input hints,
/// and premise bindings; violating that contract may panic.
/// Reports describe input unification and optimization limitations that remain
/// reachable after validation. Executable IR admission belongs to the runner.
pub fn convert(spec_al: al::Spec, without_rule_groups: bool) -> Result<sl::Spec, StructureError> {
    transform::struct_spec(spec_al, without_rule_groups)
}
