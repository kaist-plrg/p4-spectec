//! Diagnostics owned by AsciiDoc serialization
//!
//! Link warnings identify the source subject and the template supplying its text.
//! Serialization collects warnings for its caller and preserves fallback text.

use super::pl::doc::doc::{Link, LinkKind, Subject};
use crate::{
    diagnostic::{Diagnostic, Label, Severity},
    lang::common::source::Span,
};

/// Describes the source-level reference without exposing generated anchors.
fn describe_link(link: &Link) -> String {
    match &link.kind {
        LinkKind::Direct(target) => format!("destination {target:?}"),
        LinkKind::Subject(Subject::Function(id)) => format!("function `${id}`"),
        LinkKind::Subject(Subject::Relation(id)) => format!("relation `{id}`"),
        LinkKind::Subject(Subject::Type(id)) => format!("type `{id}`"),
    }
}

/// Locates a link problem at its template, falling back to the rendered fragment.
fn warning(span: &Span, link: &Link, code: &str, message: String, label: &str) -> Diagnostic {
    // A propagated hint retains the template's declaration even at a call site
    let (span, label) = match &link.origin {
        Some(origin) => {
            let span = if origin.span.left.line == 0 { span } else { &origin.span };
            (span, format!("`{}`: {label}", origin.node))
        }
        None => (span, label.to_owned()),
    };
    let labels = if span.left.line == 0 { Vec::new() } else { vec![Label::primary(span, label)] };
    // Explain how the source template becomes the displayed reference text
    let notes = match &link.origin {
        Some(origin) => vec![format!(
            "The `{}` hint supplies the displayed text for links to {}.",
            origin.node,
            describe_link(link),
        )],
        None => Vec::new(),
    };
    Diagnostic::new("adoc", Severity::Warning, Some(code.to_owned()), message, labels, notes)
}

const LINK_TARGET_EMPTY: &str = "adoc/link-target-empty";

/// Reports a cross-reference without a destination.
pub(super) fn link_target_empty(span: &Span, link: &Link) -> Diagnostic {
    let mut diagnostic = warning(
        span,
        link,
        LINK_TARGET_EMPTY,
        "cross-reference has no destination anchor".into(),
        "this link has an empty destination",
    );
    diagnostic
        .notes
        .push("Supply a nonempty destination when constructing the link.".into());
    diagnostic
}

const LINK_NESTED: &str = "adoc/link-nested";

/// Reports the inner link's source and the enclosing link that suppresses it.
pub(super) fn link_nested(span: &Span, link_outer: &Link, link_inner: &Link) -> Diagnostic {
    let mut diagnostic = warning(
        span,
        link_inner,
        LINK_NESTED,
        format!(
            "link to {} is nested inside the display text of a link to {}",
            describe_link(link_inner),
            describe_link(link_outer),
        ),
        "this inner link is suppressed",
    );
    // Relate the enclosing template when its original location is available
    if let Some(origin) = &link_outer.origin
        && origin.span.left.line != 0
    {
        diagnostic.labels.push(Label::secondary(
            &origin.span,
            format!(
                "`{}` includes this text in the outer link to {}",
                origin.node,
                describe_link(link_outer)
            ),
        ));
    }
    // Explain both the emitted markup and how to avoid the lost reference
    diagnostic.notes.push(
        "AsciiDoc cannot nest cross-references. The outer link is kept; the inner text is included without its own link.".into(),
    );
    diagnostic.notes.push(
        "To retain both links, render the references separately instead of including one linked description in another link's text.".into(),
    );
    diagnostic
}

const LINK_BODY_EMPTY: &str = "adoc/link-body-empty";

/// Reports a template that leaves a resolved reference without display text.
pub(super) fn link_body_empty(span: &Span, link: &Link) -> Diagnostic {
    let mut diagnostic = warning(
        span,
        link,
        LINK_BODY_EMPTY,
        format!("empty display text for the link to {}", describe_link(link)),
        "this produces no link text",
    );
    // Suggest an edit only when a user-supplied template produced the empty text
    diagnostic.notes.push(if let Some(origin) = &link.origin {
        format!("Make `{}` produce nonempty text, or omit the custom prose hint.", origin.node)
    } else {
        "Supply nonempty display text for the link.".into()
    });
    diagnostic
}

const LINK_TEXT_INVALID: &str = "adoc/link-text-invalid";

/// Reports a label that cannot use either AsciiDoc cross-reference delimiter.
pub(super) fn link_text_invalid(span: &Span, link: &Link, text: &str) -> Diagnostic {
    let mut diagnostic = warning(
        span,
        link,
        LINK_TEXT_INVALID,
        format!("cannot represent the link text for {} in AsciiDoc", describe_link(link)),
        "this produces link text with conflicting delimiters",
    );
    diagnostic
        .notes
        .push(format!("Generated link text: {text:?}"));
    diagnostic.notes.push(
        "The renderer uses `xref:target[text]` only without `[` or `]` in the text, and `<<target,text>>` only without `<` or `>`. Neither form can represent this text.".into(),
    );
    diagnostic.notes.push(
        "The text is emitted without a link. Reword the displayed text to avoid combining square and angle brackets.".into(),
    );
    diagnostic
}
