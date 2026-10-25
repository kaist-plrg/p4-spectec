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

/// Reports a host extern that reenters a tower lending no interpreter.
pub(super) fn reentry_unsupported() -> Box<Report> {
    diagnostic(
        "metacirc/reentry-unsupported",
        "a host extern reentered the tower, which lends no interpreter to answer it",
    )
}
