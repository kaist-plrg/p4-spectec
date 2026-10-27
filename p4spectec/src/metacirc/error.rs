//! Diagnostics for tower configuration, program input, and synthetic entries

use crate::diagnostic::{Diagnostic, Report, Severity};

/// A metacircular construction or execution diagnostic.
pub type Result<T> = std::result::Result<T, Box<Report>>;

fn diagnostic(code: &str, message: impl Into<String>) -> Box<Report> {
    Box::new(
        Diagnostic::new("metacirc", Severity::Error, Some(code.into()), message, vec![], vec![])
            .into(),
    )
}

/// Reports an invalid tower configuration.
pub(super) fn config_invalid(message: impl Into<String>) -> Box<Report> {
    diagnostic("metacirc/config-invalid", message)
}

/// Reports invalid program input for a tower.
pub(super) fn input_invalid(message: impl Into<String>) -> Box<Report> {
    diagnostic("metacirc/input-invalid", message)
}

/// Reports a failure to construct a synthetic entry.
pub(super) fn entry_invalid(message: impl Into<String>) -> Box<Report> {
    diagnostic("metacirc/entry-invalid", message)
}

/// Reports a meta execution whose result is UNMATCH.
pub(super) fn execution_unmatched(name: &str) -> Box<Report> {
    diagnostic("metacirc/execution-unmatched", format!("meta execution of {name} unmatched"))
}

/// Reports a meta execution whose result is ERROR.
pub(super) fn execution_failed(name: &str) -> Box<Report> {
    diagnostic("metacirc/execution-failed", format!("meta execution of {name} failed"))
}
