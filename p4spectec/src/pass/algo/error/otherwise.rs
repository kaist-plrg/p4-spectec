//! Diagnostics for partial operations in otherwise bodies
//!
//! Callers locate the first forbidden premise or nested function call.

use crate::lang::common::source::Span;

use crate::diagnostic::Label;

use super::{AlgoError, cause};

const OTHERWISE_CONDITION_INVALID: &str = "algo/otherwise-condition-invalid";

/// Reports a conditional premise in an otherwise body.
pub(crate) fn otherwise_condition_invalid(span: &Span, span_otherwise: &Span) -> AlgoError {
    cause(
        OTHERWISE_CONDITION_INVALID,
        "an `otherwise` body cannot test a condition",
        vec![
            Label::primary(span, ""),
            Label::secondary(span_otherwise, "`otherwise` body starts here"),
        ],
        vec![],
    )
}

const OTHERWISE_FUNCTION_CALL_INVALID: &str = "algo/otherwise-function-call-invalid";

/// Reports the nested function call in an otherwise body.
pub(crate) fn otherwise_function_call_invalid(span: &Span, span_otherwise: &Span) -> AlgoError {
    cause(
        OTHERWISE_FUNCTION_CALL_INVALID,
        "an `otherwise` body cannot call a function",
        vec![
            Label::primary(span, ""),
            Label::secondary(span_otherwise, "`otherwise` body starts here"),
        ],
        vec![],
    )
}

const OTHERWISE_RELATION_CALL_INVALID: &str = "algo/otherwise-relation-call-invalid";

/// Reports a relation call in an otherwise body.
pub(crate) fn otherwise_relation_call_invalid(span: &Span, span_otherwise: &Span) -> AlgoError {
    cause(
        OTHERWISE_RELATION_CALL_INVALID,
        "an `otherwise` body cannot call a relation",
        vec![
            Label::primary(span, ""),
            Label::secondary(span_otherwise, "`otherwise` body starts here"),
        ],
        vec![],
    )
}
