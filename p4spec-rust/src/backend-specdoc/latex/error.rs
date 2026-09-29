//! Diagnostics owned by canonical EL rendering and TeX layout
//!
//! Expression failures retain the original EL span.
//! Invalid link targets retain the referenced identifier and rejected destination.
//! Splicing forwards these reports without replacing their codes or payloads.

use crate::{
    diagnostic::{Diagnostic, Label, Report, Severity},
    lang::common::source::Span,
};

/// Carries a complete LaTeX diagnostic without a formatting wrapper.
pub type Error = Box<Report>;
pub(super) type Result<T> = std::result::Result<T, Error>;

fn cause(code: &str, message: impl Into<String>, labels: Vec<Label>) -> Error {
    Box::new(
        Diagnostic::new(
            "latex",
            Severity::Error,
            Some(code.to_owned()),
            message,
            labels,
            Vec::new(),
        )
        .into(),
    )
}

const HOLE_UNSUPPORTED: &str = "latex/hole-unsupported";

/// Rejects a hole at its original expression location.
pub(super) fn hole(span: &Span) -> Error {
    cause(
        HOLE_UNSUPPORTED,
        "LaTeX rendering is undefined for a hole expression",
        vec![Label::primary(span, "cannot render this expression")],
    )
}

const FUSE_UNSUPPORTED: &str = "latex/fuse-unsupported";

/// Rejects a fuse at its original expression location.
pub(super) fn fuse(span: &Span) -> Error {
    cause(
        FUSE_UNSUPPORTED,
        "LaTeX rendering is undefined for a fuse expression",
        vec![Label::primary(span, "cannot render this expression")],
    )
}

const UNPAREN_UNSUPPORTED: &str = "latex/unparen-unsupported";

/// Rejects an unparen at its original expression location.
pub(super) fn unparen(span: &Span) -> Error {
    cause(
        UNPAREN_UNSUPPORTED,
        "LaTeX rendering is undefined for an unparen expression",
        vec![Label::primary(span, "cannot render this expression")],
    )
}

const RAW_LATEX_UNSUPPORTED: &str = "latex/raw-latex-unsupported";

/// Rejects raw LaTeX at its original expression location.
pub(super) fn raw_latex(span: &Span) -> Error {
    cause(
        RAW_LATEX_UNSUPPORTED,
        "raw LaTeX expressions are not allowed in canonical rendering",
        vec![Label::primary(span, "cannot render this expression")],
    )
}

const LINK_TARGET_INVALID: &str = "latex/link-target-invalid";

/// Reports the rejected destination at the referenced identifier.
pub(super) fn link_target(span: &Span, text: &str) -> Error {
    cause(
        LINK_TARGET_INVALID,
        format!("invalid LaTeX link target {text:?}"),
        vec![Label::primary(span, "reference uses this target")],
    )
}
