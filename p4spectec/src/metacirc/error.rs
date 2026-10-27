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

/// Reports a part of a tower that is a template slot without an implementation yet.
pub(super) fn unimplemented(what: &str) -> Box<Report> {
    diagnostic("metacirc/unimplemented", format!("{what} is not implemented yet"))
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

/// Reports a host call outside a boundary's contract.
pub(super) fn extern_unsupported(name: &str) -> Box<Report> {
    diagnostic("metacirc/extern-unsupported", format!("unsupported meta extern: {name}"))
}

/// Reports a host call that reached a meta base run without lending its levels.
pub(super) fn base_not_lent(name: &str) -> Box<Report> {
    diagnostic(
        "metacirc/base-not-lent",
        format!(
            "meta extern {name} reached a meta base run directly; enter it through Tower::eval_program"
        ),
    )
}
