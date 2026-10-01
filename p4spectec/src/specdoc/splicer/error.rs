//! Structured diagnostics authored by document splicing
//!
//! `${func-prose: @}` returns a `splice/identifier-invalid` report at `@`.
//! LaTeX failures preserve their EL spans; file failures name their paths.
//! Errors and warnings retain diagnostic data without loading source files
//! or choosing how the caller renders them.

use std::path::Path;

use crate::lang::common::source::Span;

use crate::diagnostic::{Diagnostic, Label, Report, Severity};

// == Errors

/// Names a structured splice failure without adding a wrapper.
pub type Error = Box<Report>;

/// Creates a splice diagnostic without reading source files.
fn diagnostic(
    severity: Severity,
    code: &str,
    message: impl Into<String>,
    labels: Vec<Label>,
    notes: Vec<String>,
) -> Diagnostic {
    Diagnostic::new("splice", severity, Some(code.to_owned()), message, labels, notes)
}

/// Creates a boxed error report.
fn cause(code: &str, message: impl Into<String>, labels: Vec<Label>) -> Error {
    Box::new(diagnostic(Severity::Error, code, message, labels, Vec::new()).into())
}

/// Creates a warning report.
fn warning(
    code: &str,
    message: impl Into<String>,
    labels: Vec<Label>,
    notes: Vec<String>,
) -> Report {
    diagnostic(Severity::Warning, code, message, labels, notes).into()
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

// == Warnings

const KEY_NOT_FOUND: &str = "splice/key-not-found";

/// Reports a requested key that has no definition for its marker kind.
pub(super) fn key_not_found(span: &Span, name: &str, key: &str) -> Report {
    warning(
        KEY_NOT_FOUND,
        format!("{name} splice key not found: {key}"),
        vec![Label::primary(span, "splice marker")],
        Vec::new(),
    )
}

const TARGET_DUPLICATE: &str = "splice/target-duplicate";

/// Reports a title occurrence whose destination is already registered.
pub(super) fn target_duplicate(span: &Span, name: &str, id: &str) -> Report {
    warning(
        TARGET_DUPLICATE,
        format!("duplicate {name} target: {id}"),
        vec![Label::primary(span, "splice marker")],
        Vec::new(),
    )
}

const KEYS_UNUSED: &str = "splice/keys-unused";

/// Reports unused totals with key lists grouped into notes of five keys.
pub(super) fn keys_unused(name: &str, total: usize, keys: &[String]) -> Report {
    let num_unused = keys.len();
    let percentage = if total == 0 { 0.0 } else { num_unused as f64 / total as f64 * 100.0 };
    let notes = keys.chunks(5).map(|keys| keys.join(", ")).collect();
    warning(
        KEYS_UNUSED,
        format!("unused {num_unused} {name} splices out of {total} ({percentage:.2}%)"),
        Vec::new(),
        notes,
    )
}
