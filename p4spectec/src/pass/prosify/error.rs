//! Source-located reports for invalid prose hints
//!
//! Constructors retain the responsible hint element and its declaration.
//! Shared hint operations return typed failures;
//! prose conversion supplies their source meaning and diagnostic code here.

use crate::lang::{
    common::source::Span,
    hints::{
        alter::{AlterationError, Hole},
        fields::FieldHint,
    },
    traits::print::Print,
};

use crate::lang::el::ast::{Exp, Id};

use crate::diagnostic::{Diagnostic, Label, Report, Severity};

/// Names a prose report without adding a wrapper.
pub type ProseError = Box<Report>;

/// Creates a prose diagnostic from the responsible and related locations.
fn cause(code: &str, message: String, labels: Vec<Label>) -> ProseError {
    Box::new(
        Diagnostic::new("prose", Severity::Error, Some(code.to_owned()), message, labels, vec![])
            .into(),
    )
}

// = Field hints

const FIELD_HINT_ARITY_MISMATCH: &str = "prose/field-hint-arity-mismatch";

/// Locates the first extra field name or the position of a missing name.
pub(super) fn field_hint_arity_mismatch(
    span_decl: &Span,
    hint: &FieldHint,
    len_expect: usize,
    len_actual: usize,
) -> ProseError {
    // Extra names identify the first excess; missing names follow the last field
    let fields = hint.node.as_slice();
    let span = if let Some(field) = fields.get(len_expect) {
        field.span
    } else if let Some(field) = fields.last() {
        Span::new(field.span.right, field.span.right)
    } else {
        hint.span
    };
    // Match number agreement without hiding the actual and expected counts
    let names = if len_actual == 1 { "field name" } else { "field names" };
    let fields = if len_expect == 1 { "field" } else { "fields" };
    cause(
        FIELD_HINT_ARITY_MISMATCH,
        format!(
            "hint `prose_fields` has {len_actual} {names}, but the syntax case has {len_expect} {fields}"
        ),
        vec![
            Label::primary(&span, format!("expected {len_expect}, got {len_actual}")),
            Label::secondary(span_decl, "syntax case declared here"),
        ],
    )
}

const FIELD_HINT_ELEMENT_INVALID: &str = "prose/field-hint-element-invalid";

/// Reports the first field name that is not a text literal.
pub(super) fn field_hint_element_invalid(id: &Id, exp: &Exp, span_decl: &Span) -> ProseError {
    cause(
        FIELD_HINT_ELEMENT_INVALID,
        format!(
            "hint `{}` field names must be text literals, but got `{}`",
            id.node,
            Print::to_string(exp)
        ),
        vec![
            Label::primary(&exp.span, "expected a text literal"),
            Label::secondary(span_decl, "hint belongs to this declaration"),
        ],
    )
}

// = Alteration hints

const ALTERATION_HINT_INDEX_OUT_OF_BOUNDS: &str = "prose/alteration-hint-index-out-of-bounds";

/// Reports the placeholder and the number of values it can select from.
pub(super) fn alteration_hint_index_out_of_bounds(
    span_decl: &Span,
    name_hint: &str,
    error: AlterationError,
) -> ProseError {
    let AlterationError::IndexOutOfBounds { hole, index, item_count } = error;
    let text = match hole.node {
        Hole::Next => "%".to_owned(),
        Hole::Num(idx) => format!("%{idx}"),
    };
    let values = if item_count == 1 { "value" } else { "values" };
    let verb = if item_count == 1 { "is" } else { "are" };
    cause(
        ALTERATION_HINT_INDEX_OUT_OF_BOUNDS,
        format!(
            "hint `{name_hint}` placeholder `{text}` selects index {index}, but only {item_count} {values} {verb} available"
        ),
        vec![
            Label::primary(&hole.span, format!("index {index} is out of bounds")),
            Label::secondary(
                span_decl,
                format!("declaration provides {item_count} {values} for this hint"),
            ),
        ],
    )
}
