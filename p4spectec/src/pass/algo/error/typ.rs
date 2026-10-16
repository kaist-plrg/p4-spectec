//! Type lookup and operation diagnostics during algorithmic conversion
//!
//! Constructors preserve locations from type operations and directly supplied IL.

use crate::lang::common::{Id, source::Span};

use crate::diagnostic::Label;

use crate::runtime::ops::typ::TypeError;

use super::{AlgoError, cause};

const TYPE_OPERATION_INVALID: &str = "algo/type-operation-invalid";

/// Promotes a terminal type operation failure at its original location.
pub(crate) fn type_operation_invalid(error: TypeError) -> AlgoError {
    cause(
        TYPE_OPERATION_INVALID,
        format!("type operation failed: {}", error.kind),
        vec![Label::primary(&error.span, "")],
        vec![],
    )
}

const TYPE_UNDEFINED: &str = "algo/type-undefined";

/// Reports a missing type in directly supplied IL.
pub(crate) fn type_undefined(id: &Id) -> AlgoError {
    cause(
        TYPE_UNDEFINED,
        format!("type `{}` is not defined", id.node),
        vec![Label::primary(&id.span, "")],
        vec![],
    )
}

const TYPE_ARGUMENT_ARITY_MISMATCH: &str = "algo/type-argument-arity-mismatch";

/// Reports invalid type arguments in directly supplied IL.
pub(crate) fn type_argument_arity_mismatch(
    span: &Span,
    expected: usize,
    actual: usize,
) -> AlgoError {
    cause(
        TYPE_ARGUMENT_ARITY_MISMATCH,
        format!("type argument arity mismatch: expected {expected}, got {actual}"),
        vec![Label::primary(span, "")],
        vec![],
    )
}
