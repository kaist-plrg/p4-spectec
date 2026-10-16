//! Diagnostics for emitting K and Redex inputs and for the host boundary

use crate::diagnostic::{Diagnostic, Report, Severity};

/// A metacross emission or host diagnostic.
pub type Result<T> = std::result::Result<T, Box<Report>>;

fn diagnostic(code: &str, message: impl Into<String>) -> Box<Report> {
    Box::new(
        Diagnostic::new("metacross", Severity::Error, Some(code.into()), message, vec![], vec![])
            .into(),
    )
}

/// Reports a meta value the emitters have no term for.
pub(super) fn emit_invalid(message: impl Into<String>) -> Box<Report> {
    diagnostic("metacross/emit-invalid", message)
}

/// Reports a host request or value the wire codec cannot read.
pub(super) fn wire_invalid(message: impl Into<String>) -> Box<Report> {
    diagnostic("metacross/wire-invalid", message)
}

/// Reports a failure to write the emitted text.
pub fn output_failed(message: impl Into<String>) -> Box<Report> {
    diagnostic("metacross/output-failed", message)
}
