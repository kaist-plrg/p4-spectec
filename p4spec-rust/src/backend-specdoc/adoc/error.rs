//! Diagnostics owned by AsciiDoc serialization
//!
//! Link warnings retain the definition or fragment that produced the markup.
//! Serialization collects warnings for its caller and preserves fallback text.

use crate::{
    diagnostic::{Diagnostic, Label, Severity},
    lang::common::source::Span,
};

fn warning(code: &str, message: impl Into<String>, span: &Span) -> Diagnostic {
    let labels = if span.left.line == 0 {
        Vec::new()
    } else {
        vec![Label::primary(span, "while rendering this fragment")]
    };
    Diagnostic::new("adoc", Severity::Warning, Some(code.to_owned()), message, labels, Vec::new())
}

const LINK_TARGET_EMPTY: &str = "adoc/link-target-empty";

/// Reports a cross-reference without a destination.
pub(super) fn link_target_empty(span: &Span) -> Diagnostic {
    warning(LINK_TARGET_EMPTY, "link with empty target", span)
}

const LINK_NESTED: &str = "adoc/link-nested";

/// Reports the inner target dropped by AsciiDoc's single-link representation.
pub(super) fn link_nested(target_outer: &str, target_inner: &str, span: &Span) -> Diagnostic {
    warning(
        LINK_NESTED,
        format!(
            "nested link: cross-reference to {target_inner:?} is dropped inside the link to {target_outer:?} (asciidoc cannot nest cross-references)"
        ),
        span,
    )
}

const LINK_BODY_EMPTY: &str = "adoc/link-body-empty";

/// Reports a resolved cross-reference with no visible body.
pub(super) fn link_body_empty(target: &str, span: &Span) -> Diagnostic {
    warning(LINK_BODY_EMPTY, format!("link to {target:?} has empty body"), span)
}

const LINK_TEXT_INVALID: &str = "adoc/link-text-invalid";

/// Reports a label that cannot use either AsciiDoc cross-reference delimiter.
pub(super) fn link_text_invalid(text: &str, span: &Span) -> Diagnostic {
    let mut diagnostic = warning(
        LINK_TEXT_INVALID,
        "AsciiDoc link text contains both brackets and angle brackets; emitting the label without a link",
        span,
    );
    diagnostic.notes.push(text.to_owned());
    diagnostic
}
