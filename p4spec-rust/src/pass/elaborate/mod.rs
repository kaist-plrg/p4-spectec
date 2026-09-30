//! Surface-language validation and conversion to the internal language
//!
//! `convert` elaborates an EL specification into a typed IL specification.
//!
//! Definitions are checked in source order,
//! rule groups and clauses are attached to their relation or function,
//! and iterations are annotated with their source variables.
//!
//! For example, `-- if (n_x = n_y)*` becomes `-- if (n_x = n_y)*{n_x <- n_x*}`
//! once the dimensions of `n_x` and `n_y` are known.

#![allow(clippy::result_large_err)]

mod backtrack;
mod context;
mod dimension;
mod error;
mod expect;
mod transform;

use crate::lang::el;

use crate::lang::il;

use crate::diagnostic::Report;

pub use error::ElabError;

// == Entry point

/// Validates and converts EL to IL, discarding nonfatal warnings.
pub fn convert(spec_el: el::ast::Spec) -> Result<il::ast::Spec, ElabError> {
    convert_with_warnings(spec_el).0
}

/// Converts EL to IL and retains committed warnings even when a later check fails.
pub fn convert_with_warnings(
    spec_el: el::ast::Spec,
) -> (Result<il::ast::Spec, ElabError>, Vec<Report>) {
    let mut warnings = Vec::new();
    let result = transform::elab_spec(&mut warnings, spec_el);
    (result, warnings)
}
