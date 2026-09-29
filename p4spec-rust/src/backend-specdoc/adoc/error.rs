//! Diagnostics owned by AsciiDoc serialization
//!
//! Link warnings identify the referenced declaration or the owning fragment.
//! Serialization collects warnings for its caller and preserves fallback text.

use super::pl::doc::doc::{Link, Subject};
use crate::{
    diagnostic::{Diagnostic, Label, Severity},
    lang::common::source::Span,
};

/// Describes the source-level reference without exposing generated anchors.
fn describe_link(link: &Link) -> String {
    match link {
        Link::Direct(target) => format!("destination {target:?}"),
        Link::Subject(Subject::Function(id)) => format!("function `${}`", id.node),
        Link::Subject(Subject::Relation(id)) => format!("relation `{}`", id.node),
        Link::Subject(Subject::Type(id)) => format!("type `{}`", id.node),
    }
}

/// Locates a link problem at its declaration or the rendered fragment.
fn warning(span: &Span, link: &Link, code: &str, message: String, label: &str) -> Diagnostic {
    // Subject identifiers retain declaration spans independently of their hints
    let (span, label) = match link {
        Link::Direct(_) => (span, label.to_owned()),
        Link::Subject(Subject::Function(id) | Subject::Relation(id) | Subject::Type(id)) => {
            (&id.span, format!("linked declaration: {label}"))
        }
    };
    let labels = vec![Label::primary(span, label)];
    Diagnostic::new("adoc", Severity::Warning, Some(code.to_owned()), message, labels, Vec::new())
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
    // Relate the declaration referenced by the enclosing link
    if let Link::Subject(Subject::Function(id) | Subject::Relation(id) | Subject::Type(id)) =
        link_outer
    {
        diagnostic.labels.push(Label::secondary(
            &id.span,
            format!("declaration referenced by the outer link to {}", describe_link(link_outer)),
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

/// Reports a resolved reference without display text.
pub(super) fn link_body_empty(span: &Span, link: &Link) -> Diagnostic {
    let mut diagnostic = warning(
        span,
        link,
        LINK_BODY_EMPTY,
        format!("empty display text for the link to {}", describe_link(link)),
        "this link has no display text",
    );
    diagnostic
        .notes
        .push("Supply nonempty display text for the link.".into());
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
        "this link has display text with conflicting delimiters",
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
