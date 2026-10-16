//! Reports for source-reachable structuring limitations
//!
//! Validated AL establishes the internal shape and binding invariants.
//! Type operations can still fail on validated inputs.

use crate::lang::common::source::Span;

use crate::diagnostic::{Diagnostic, Label, Report, Severity};

use crate::runtime::ops::typ::TypeError;

/// Names a structuring report without adding a wrapper.
pub type StructureError = Box<Report>;

/// Creates a structuring diagnostic at the operation's source location.
fn cause(code: &str, message: impl Into<String>, span: &Span) -> StructureError {
    Box::new(
        Diagnostic::new(
            "structure",
            Severity::Error,
            Some(code.to_owned()),
            message,
            vec![Label::primary(span, "")],
            vec![],
        )
        .into(),
    )
}

const TYPE_OPERATION_INVALID: &str = "structure/type-operation-invalid";

/// Promotes a type operation failure without losing its original location.
pub(super) fn type_operation_invalid(error: TypeError) -> StructureError {
    cause(TYPE_OPERATION_INVALID, format!("type operation failed: {}", error.kind), &error.span)
}
