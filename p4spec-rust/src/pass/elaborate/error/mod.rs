//! Structured diagnostics authored by elaboration
//!
//! Each constructor owns one stable `elab/...` code and preserves the source
//! spans available at its semantic check. Attempt frames remain uncoded context;
//! terminal causes keep their complete diagnostic payload in [`Report`].

use crate::lang::traits::print::Print;

use crate::lang::il::ast as il;

use crate::diagnostic::{Diagnostic, Label, Report, Severity};

pub(super) mod arg;
pub(super) mod decl;
pub(super) mod dim;
pub(super) mod exp;
pub(super) mod not;
pub(super) mod prem;
pub(super) mod typ;

/// Names a structured elaboration failure without adding a wrapper.
pub type ElabError = Box<Report>;

/// Creates an elaboration diagnostic without reading source files.
fn diagnostic(
    severity: Severity,
    code: &str,
    message: impl Into<String>,
    labels: Vec<Label>,
    notes: Vec<String>,
) -> Diagnostic {
    Diagnostic::new("elab", severity, Some(code.to_owned()), message, labels, notes)
}

/// Creates a boxed error report.
fn cause(
    code: &str,
    message: impl Into<String>,
    labels: Vec<Label>,
    notes: Vec<String>,
) -> ElabError {
    Box::new(diagnostic(Severity::Error, code, message, labels, notes).into())
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

/// Creates a type mismatch for a named declaration slot.
fn type_mismatch(
    code: &str,
    subject: String,
    typ_expect_il: &il::Typ,
    typ_infer_il: &il::Typ,
) -> ElabError {
    cause(
        code,
        format!(
            "{subject} expects '{}', but found '{}'",
            typ_expect_il.to_string(),
            typ_infer_il.to_string()
        ),
        vec![Label::primary(&typ_infer_il.span, "")],
        Vec::new(),
    )
}
