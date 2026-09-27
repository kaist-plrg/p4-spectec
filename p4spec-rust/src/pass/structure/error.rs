//! Reports for source-reachable structuring limitations
//!
//! Validated AL establishes the internal shape and binding invariants.
//! Cross-clause unifier collisions, template population, type operations,
//! and extending total cases can still fail on validated source inputs.

use crate::{
    diagnostic::{Diagnostic, Label, Report, Severity},
    lang::common::{Id, source::Span},
    runtime::ops::typ::TypeError,
};

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

const INPUT_UNIFICATION_CONFLICTING: &str = "structure/input-unification-conflicting";

/// Reports an identifier generalized in more than one input position.
pub(super) fn input_unification_conflicting(id: &Id) -> StructureError {
    cause(
        INPUT_UNIFICATION_CONFLICTING,
        format!("cannot unify identifier `{}` in more than one input position", id.node),
        &id.span,
    )
}

const INPUT_TEMPLATE_UNSUPPORTED: &str = "structure/input-template-unsupported";

/// Reports an input whose generated template cannot recover its bindings.
pub(super) fn input_template_unsupported(span: &Span) -> StructureError {
    cause(INPUT_TEMPLATE_UNSUPPORTED, "cannot populate the shared input template", span)
}

const TYPE_OPERATION_INVALID: &str = "structure/type-operation-invalid";

/// Promotes a type operation failure without losing its original location.
pub(super) fn type_operation_invalid(error: TypeError) -> StructureError {
    cause(TYPE_OPERATION_INVALID, format!("type operation failed: {}", error.kind), &error.span)
}

const CASE_EXTENSION_UNSUPPORTED: &str = "structure/case-extension-unsupported";

/// Reports a guard that cannot be added to an already total case analysis.
pub(super) fn case_extension_unsupported(span: &Span) -> StructureError {
    cause(
        CASE_EXTENSION_UNSUPPORTED,
        "cannot extend a total case analysis with another branch",
        span,
    )
}
