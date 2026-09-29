//! Assign diagnostics for interpreter operations
//!
//! Constructors identify runtime checks without deciding whether callers retry.

use super::diagnostic;
use crate::diagnostic::Diagnostic;

const ASSIGNMENT_MISMATCH: &str = "runtime/assignment-mismatch";

/// Reports mismatch.
pub fn assignment_mismatch(exp: String, value: String) -> Diagnostic {
    diagnostic(ASSIGNMENT_MISMATCH, format!("match failed {exp} <- {value}"), Vec::new())
}

const EXPRESSION_ARITY_ASSIGNMENT_MISMATCH: &str = "runtime/assignment-expression-arity-mismatch";

/// Reports expression arity mismatch.
pub fn assignment_expression_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        EXPRESSION_ARITY_ASSIGNMENT_MISMATCH,
        format!(
            "mismatch in number of expressions and values while assigning, expected {expected} value(s) but got {actual}"
        ),
        Vec::new(),
    )
}

const ARGUMENT_ARITY_ASSIGNMENT_MISMATCH: &str = "runtime/assignment-argument-arity-mismatch";

/// Reports argument arity mismatch.
pub fn assignment_argument_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        ARGUMENT_ARITY_ASSIGNMENT_MISMATCH,
        format!(
            "mismatch in number of arguments and values while assigning, expected {expected} value(s) but got {actual}"
        ),
        Vec::new(),
    )
}

const ASSIGNMENT_CONS_EMPTY: &str = "runtime/assignment-cons-empty";

/// Reports empty cons.
pub fn assignment_cons_empty() -> Diagnostic {
    diagnostic(
        ASSIGNMENT_CONS_EMPTY,
        "cannot assign an empty list to a cons expression",
        Vec::new(),
    )
}

const DEFINITION_ASSIGNMENT_MISMATCH: &str = "runtime/assignment-definition-mismatch";

/// Reports definition mismatch.
pub fn assignment_definition_mismatch(value: String, def: String) -> Diagnostic {
    diagnostic(
        DEFINITION_ASSIGNMENT_MISMATCH,
        format!("cannot assign a value {value} to a definition {def}"),
        Vec::new(),
    )
}
