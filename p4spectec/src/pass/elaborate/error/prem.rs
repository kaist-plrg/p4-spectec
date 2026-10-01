//! Relation input hints, premises, and rule-shape diagnostics
//!
//! Relation declarations emit input-hint warnings directly, while premise and
//! rule elaboration attach these failures to their enclosing attempt frames.

use crate::lang::common::{Id, source::Span};

use crate::diagnostic::{Label, Report};

use super::{ElabError, cause, warning};

const PREMISE_VARIABLE_IDENTIFIER_INVALID: &str = "elab/premise-variable-identifier-invalid";

/// Reports an invalid meta-variable identifier in a variable premise.
pub(in crate::pass::elaborate) fn premise_variable_identifier_invalid(id: &Id) -> ElabError {
    cause(
        PREMISE_VARIABLE_IDENTIFIER_INVALID,
        format!("meta-variable identifier `{}` must not have a suffix", id.node),
        vec![Label::primary(&id.span, "")],
        Vec::new(),
    )
}

const PREMISE_VARIABLE_TYPE_REPEATED: &str = "elab/premise-variable-type-repeated";

/// Reports a variable premise whose name is already used by a type.
pub(in crate::pass::elaborate) fn premise_variable_type_repeated(
    id: &Id,
    span_previous: &Span,
) -> ElabError {
    let labels =
        vec![Label::primary(&id.span, ""), Label::secondary(span_previous, "type declared here")];
    cause(
        PREMISE_VARIABLE_TYPE_REPEATED,
        format!("meta-variable name `{}` is already used by a type", id.node),
        labels,
        Vec::new(),
    )
}

const PREMISE_OTHERWISE_REPEATED: &str = "elab/premise-otherwise-repeated";

/// Reports the second otherwise premise and relates the first.
pub(in crate::pass::elaborate) fn premise_otherwise_repeated(
    span: &Span,
    span_previous: &Span,
) -> ElabError {
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(span_previous, "first `otherwise` premise here"),
    ];
    cause(
        PREMISE_OTHERWISE_REPEATED,
        "cannot use more than one `otherwise` premise",
        labels,
        Vec::new(),
    )
}

const PREMISE_NEGATED_OUTPUT_UNSUPPORTED: &str = "elab/premise-negated-output-unsupported";

/// Reports a negated relation premise with an output position.
pub(in crate::pass::elaborate) fn premise_negated_output_unsupported(
    id: &Id,
    span_output: &Span,
    span_signature: &Span,
) -> ElabError {
    let labels = vec![
        Label::primary(span_output, ""),
        Label::secondary(span_signature, "relation signature with output positions here"),
    ];
    cause(
        PREMISE_NEGATED_OUTPUT_UNSUPPORTED,
        format!("negated rule premise for relation `{}` cannot use output positions", id.node),
        labels,
        vec![
            concat!("Rule premise negation is supported only for relations without ", "outputs.",)
                .to_owned(),
        ],
    )
}

const PREMISE_VARIABLE_ITERATION_UNSUPPORTED: &str = "elab/premise-variable-iteration-unsupported";
const PREMISE_OTHERWISE_ITERATION_UNSUPPORTED: &str =
    "elab/premise-otherwise-iteration-unsupported";

/// Reports an attempted iteration of a variable premise.
pub(in crate::pass::elaborate) fn premise_variable_iteration_unsupported(span: &Span) -> ElabError {
    cause(
        PREMISE_VARIABLE_ITERATION_UNSUPPORTED,
        "cannot iterate a `var` premise",
        vec![Label::primary(span, "")],
        vec!["A variable premise declares its variable once for the rule.".to_owned()],
    )
}

/// Reports an attempted iteration of an otherwise premise.
pub(in crate::pass::elaborate) fn premise_otherwise_iteration_unsupported(
    span: &Span,
) -> ElabError {
    cause(
        PREMISE_OTHERWISE_ITERATION_UNSUPPORTED,
        "cannot iterate an `otherwise` premise",
        vec![Label::primary(span, "")],
        vec!["An `otherwise` premise selects one fallback path for the rule.".to_owned()],
    )
}

const RELATION_RULE_GROUP_NAME_MISMATCH: &str = "elab/relation-rule-group-name-mismatch";

/// Reports a rule naming a different relation from its group.
pub(in crate::pass::elaborate) fn relation_rule_group_name_mismatch(
    id_rule: &Id,
    id_group: &Id,
) -> ElabError {
    let labels = vec![
        Label::primary(&id_rule.span, ""),
        Label::secondary(
            &id_group.span,
            format!("enclosing rule group names relation `{}`", id_group.node),
        ),
    ];
    cause(
        RELATION_RULE_GROUP_NAME_MISMATCH,
        format!(
            "rule belongs to relation `{}`, but its enclosing rule group \
            belongs to relation `{}`",
            id_rule.node, id_group.node
        ),
        labels,
        Vec::new(),
    )
}

const RELATION_RULE_OTHERWISE_REPEATED: &str = "elab/relation-rule-otherwise-repeated";

/// Reports the second otherwise rule in a group.
pub(in crate::pass::elaborate) fn relation_rule_otherwise_repeated(
    span: &Span,
    span_previous: &Span,
) -> ElabError {
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(span_previous, "first `otherwise` rule here"),
    ];
    cause(
        RELATION_RULE_OTHERWISE_REPEATED,
        "cannot use more than one `otherwise` rule in a rule group",
        labels,
        Vec::new(),
    )
}

const RELATION_RULE_OTHERWISE_INVALID: &str = "elab/relation-rule-otherwise-invalid";

/// Reports an otherwise rule sharing a group with an ordinary rule.
pub(in crate::pass::elaborate) fn relation_rule_otherwise_invalid(
    span: &Span,
    span_other: &Span,
    message_other: &str,
) -> ElabError {
    let labels = vec![Label::primary(span, ""), Label::secondary(span_other, message_other)];
    cause(
        RELATION_RULE_OTHERWISE_INVALID,
        "an `otherwise` rule must be the only rule in its rule group",
        labels,
        Vec::new(),
    )
}

const RELATION_INPUT_HINT_EMPTY: &str = "elab/relation-input-hint-empty";
const RELATION_INPUT_HINT_INDEX_REPEATED: &str = "elab/relation-input-hint-index-repeated";
const RELATION_INPUT_HINT_INDEX_OUT_OF_BOUNDS: &str =
    "elab/relation-input-hint-index-out-of-bounds";
const RELATION_INPUT_HINT_INVALID: &str = "elab/relation-input-hint-invalid";
const RELATION_INPUT_HINT_MISSING: &str = "elab/relation-input-hint-missing";

/// Reports a user-supplied input hint with no positions.
pub(in crate::pass::elaborate) fn relation_input_hint_empty(span: &Span) -> ElabError {
    cause(
        RELATION_INPUT_HINT_EMPTY,
        "input hint must contain at least one index such as `%0`",
        vec![Label::primary(span, "")],
        Vec::new(),
    )
}

/// Reports a repeated input-hint index and relates its first occurrence.
pub(in crate::pass::elaborate) fn relation_input_hint_index_repeated(
    idx: usize,
    span: &Span,
    span_previous: &Span,
) -> ElabError {
    let labels =
        vec![Label::primary(span, ""), Label::secondary(span_previous, "first occurrence here")];
    cause(
        RELATION_INPUT_HINT_INDEX_REPEATED,
        format!("input hint repeats index `%{idx}`"),
        labels,
        Vec::new(),
    )
}

/// Reports an input-hint index outside the relation notation arity.
pub(in crate::pass::elaborate) fn relation_input_hint_index_out_of_bounds(
    idx: usize,
    arity: usize,
    span: &Span,
    span_notation: &Span,
) -> ElabError {
    let text_positions = if arity == 1 { "position" } else { "positions" };
    let labels = vec![
        Label::primary(span, ""),
        Label::secondary(span_notation, format!("relation has {arity} {text_positions}")),
    ];
    cause(
        RELATION_INPUT_HINT_INDEX_OUT_OF_BOUNDS,
        format!(
            "input hint index `%{idx}` is out of bounds for a relation \
            with {arity} {text_positions}"
        ),
        labels,
        Vec::new(),
    )
}

/// Reports an input hint containing something other than indexed holes.
pub(in crate::pass::elaborate) fn relation_input_hint_invalid(
    span: &Span,
    text_actual: &str,
) -> ElabError {
    cause(
        RELATION_INPUT_HINT_INVALID,
        format!(
            "input hint must be a sequence of indexed holes such as `%0`, \
            but got `{text_actual}`"
        ),
        vec![Label::primary(span, "")],
        Vec::new(),
    )
}

/// Warns that a relation has no input hint and defaults every position to input.
pub(in crate::pass::elaborate) fn relation_input_hint_missing(id: &Id, span: &Span) -> Report {
    warning(
        RELATION_INPUT_HINT_MISSING,
        format!("relation `{}` has no input hint", id.node),
        vec![Label::primary(span, "")],
        Vec::new(),
    )
}

const RELATION_INPUT_HINT_MISMATCH: &str = "elab/relation-input-hint-mismatch";

/// Reports a stored input hint that does not fit the elaborated relation use.
pub(in crate::pass::elaborate) fn relation_input_hint_mismatch(
    span: &Span,
    message: impl Into<String>,
) -> ElabError {
    cause(RELATION_INPUT_HINT_MISMATCH, message, vec![Label::primary(span, "")], Vec::new())
}
