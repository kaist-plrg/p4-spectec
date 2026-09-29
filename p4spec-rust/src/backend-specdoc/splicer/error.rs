//! Located splice failures and immediate warnings
//!
//! `${func-prose: @}` returns a `splice/identifier-invalid` report at `@`.
//! LaTeX failures preserve their EL spans; file failures name their paths.
//! Errors are returned to the caller as reports, while warnings are rendered
//! immediately to stderr without accumulating in the splice context.

use std::path::Path;

use super::super::latex;
use crate::{
    diagnostic::{Diagnostic, Label, RenderConfig, Renderer, Report, Severity},
    lang::common::source::Span,
};

// == Errors

/// Names a structured splice failure without adding a wrapper.
pub type Error = Box<Report>;

/// Creates an error report authored by the splicer.
fn cause(code: &str, message: impl Into<String>, labels: Vec<Label>) -> Error {
    Box::new(
        Diagnostic::new(
            "splice",
            Severity::Error,
            Some(code.to_owned()),
            message,
            labels,
            Vec::new(),
        )
        .into(),
    )
}

const IDENTIFIER_INVALID: &str = "splice/identifier-invalid";

/// Reports a missing identifier at the skeleton parser's current position.
pub(super) fn identifier(span: &Span) -> Error {
    cause(
        IDENTIFIER_INVALID,
        "cannot parse identifier",
        vec![Label::primary(span, "expected an identifier")],
    )
}

const IO: &str = "splice/io";

/// Reports a failed file operation with its path and filesystem cause.
pub(super) fn io(path: &Path, source: std::io::Error) -> Error {
    // Keep common filesystem causes independent of OS wording and error numbers
    let message = match source.kind() {
        std::io::ErrorKind::NotFound => "file does not exist".to_owned(),
        std::io::ErrorKind::PermissionDenied => "permission denied".to_owned(),
        std::io::ErrorKind::NotADirectory => "not a directory".to_owned(),
        _ => source.to_string(),
    };
    cause(IO, format!("{}: {message}", path.display()), Vec::new())
}

const LATEX_RENDERING_INVALID: &str = "splice/latex-rendering-invalid";

/// Converts a LaTeX failure while retaining its message and expression span.
pub(super) fn latex(error: latex::Error) -> Error {
    let span = error.span();
    let labels = if span.left.line == 0 {
        Vec::new()
    } else {
        vec![Label::primary(&span, "cannot render this expression")]
    };
    cause(LATEX_RENDERING_INVALID, error.to_string(), labels)
}

// == Warnings

/// Renders a splice warning immediately to stderr.
pub(super) fn warn(span: &Span, message: String) {
    let labels =
        if span.left.line == 0 { Vec::new() } else { vec![Label::primary(span, "splice marker")] };
    let report: Report =
        Diagnostic::new("splice", Severity::Warning, None, message, labels, Vec::new()).into();
    let mut renderer = Renderer::new(RenderConfig::default());
    if let Err(error) = renderer.render_to_stderr(&report) {
        eprintln!("{report}\n{error}");
    }
}
