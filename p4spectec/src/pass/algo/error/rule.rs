//! Relation input hints and shared rule input diagnostics
//!
//! Invalid hints and terminal overlap failures become reports at rule boundaries.

use crate::lang::{common::source::Span, hints::input::InputError};

use crate::diagnostic::Label;

use super::{AlgoError, cause};

const RELATION_INPUT_HINT_INVALID: &str = "algo/relation-input-hint-invalid";

/// Reports an invalid relation input hint in directly supplied IL.
pub(crate) fn relation_input_hint_invalid(error: InputError, span: Span) -> AlgoError {
    cause(
        RELATION_INPUT_HINT_INVALID,
        format!("invalid relation input hint: {error}"),
        vec![Label::primary(&span, "")],
        vec![],
    )
}

const RULE_INPUT_MISMATCH: &str = "algo/rule-input-mismatch";

/// Reports input templates that cannot overlap in directly supplied IL.
pub(crate) fn rule_input_mismatch(span: &Span) -> AlgoError {
    cause(
        RULE_INPUT_MISMATCH,
        "cannot anti-unify rule inputs",
        vec![Label::primary(span, "")],
        vec![],
    )
}
