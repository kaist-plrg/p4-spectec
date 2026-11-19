//! Structured diagnostics authored by algorithmic conversion
//!
//! Constructors retain the spans at binding and table checks.
//! Recoverable anti-unification mismatches stay local to overlap analysis;
//! only terminal failures become reports at their owning boundary.

use crate::diagnostic::{Diagnostic, Label, Report, Severity};

pub(super) mod binding;
pub(super) mod otherwise;
pub(super) mod rule;
pub(super) mod table;
pub(super) mod typ;

/// Names an algorithmic conversion report without adding a wrapper.
pub type AlgoError = Box<Report>;

/// Creates an algorithmic error without reading source files.
fn cause(
    code: &str,
    message: impl Into<String>,
    labels: Vec<Label>,
    notes: Vec<String>,
) -> AlgoError {
    Box::new(
        Diagnostic::new("algo", Severity::Error, Some(code.to_owned()), message, labels, notes)
            .into(),
    )
}
