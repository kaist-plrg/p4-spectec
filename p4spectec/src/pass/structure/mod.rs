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

use crate::lang::al::ast as al;

use crate::lang::sl::ast as sl;

pub use error::StructureError;

// == Entry point

/// Converts AL definitions into SL, removing rule groups if requested.
///
/// Requires input from elaboration followed by algorithmic conversion.
pub fn convert(spec_al: al::Spec, without_rule_groups: bool) -> Result<sl::Spec, StructureError> {
    transform::struct_spec(spec_al, without_rule_groups)
}
