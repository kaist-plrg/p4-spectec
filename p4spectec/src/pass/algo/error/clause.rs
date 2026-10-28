//! Diagnostics for relation premises in function clauses
//!
//! Callers locate the first relation premise, iterated or not.

use crate::lang::common::source::Span;

use crate::diagnostic::Label;

use super::{AlgoError, cause};

const CLAUSE_RELATION_CALL_INVALID: &str = "algo/clause-relation-call-invalid";

/// Reports a relation premise in a function clause.
pub(crate) fn clause_relation_call_invalid(span: &Span) -> AlgoError {
    cause(
        CLAUSE_RELATION_CALL_INVALID,
        "a function clause cannot call a relation",
        vec![Label::primary(span, "")],
        vec![],
    )
}
