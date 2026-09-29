//! Diagnostics owned by AsciiDoc serialization
//!
//! Each link carries the source location used by its warnings.
//! Serialization collects warnings for its caller and preserves fallback text.

use super::pl::doc::doc::{Link, Subject};
use crate::diagnostic::{Diagnostic, Label, Severity};

/// Describes the source-level reference without exposing generated anchors.
fn describe_link(link: &Link) -> String {
    match link {
        Link::Direct(id) => format!("destination {:?}", id.node),
        Link::Subject(Subject::Function(id)) => format!("function `${}`", id.node),
        Link::Subject(Subject::Relation(id)) => format!("relation `{}`", id.node),
        Link::Subject(Subject::Type(id)) => format!("type `{}`", id.node),
    }
}

/// Locates a link problem at the source span carried by the link.
fn warning(link: &Link, code: &str, message: String, label: &str) -> Diagnostic {
    // Every target retains the source span supplied when the link was built
    let span = match link {
        Link::Direct(id)
        | Link::Subject(Subject::Function(id) | Subject::Relation(id) | Subject::Type(id)) => {
            &id.span
        }
    };
    let labels = vec![Label::primary(span, label)];
    Diagnostic::new("adoc", Severity::Warning, Some(code.to_owned()), message, labels, Vec::new())
}

const LINK_TARGET_EMPTY: &str = "adoc/link-target-empty";

/// Reports a cross-reference without a destination.
pub(super) fn link_target_empty(link: &Link) -> Diagnostic {
    let mut diagnostic = warning(
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
pub(super) fn link_nested(link_outer: &Link, link_inner: &Link) -> Diagnostic {
    let mut diagnostic = warning(
        link_inner,
        LINK_NESTED,
        format!(
            "link to {} is nested inside the display text of a link to {}",
            describe_link(link_inner),
            describe_link(link_outer),
        ),
        "this inner link is suppressed",
    );
    // Relate the source of the enclosing link's displayed text
    let span_outer = match link_outer {
        Link::Direct(id)
        | Link::Subject(Subject::Function(id) | Subject::Relation(id) | Subject::Type(id)) => {
            &id.span
        }
    };
    diagnostic.labels.push(Label::secondary(
        span_outer,
        format!(
            "this supplies the display text for the outer link to {}",
            describe_link(link_outer)
        ),
    ));
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
pub(super) fn link_body_empty(link: &Link) -> Diagnostic {
    let mut diagnostic = warning(
        link,
        LINK_BODY_EMPTY,
        format!("empty display text for the link to {}", describe_link(link)),
        "this produces no link text",
    );
    diagnostic
        .notes
        .push("Supply nonempty display text for the link.".into());
    diagnostic
}

const LINK_TEXT_INVALID: &str = "adoc/link-text-invalid";

/// Reports a label that cannot use either AsciiDoc cross-reference delimiter.
pub(super) fn link_text_invalid(link: &Link, text: &str) -> Diagnostic {
    let mut diagnostic = warning(
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
