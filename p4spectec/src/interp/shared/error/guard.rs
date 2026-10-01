//! Guard diagnostics for interpreter operations
//!
//! Builds diagnostics; callers choose whether to stop or try another candidate.

use crate::diagnostic::Diagnostic;

use super::diagnostic;

const RELATION_INPUT_ARITY_MISMATCH: &str = "runtime/relation-input-arity-mismatch";

/// Reports the wrong number of relation inputs.
pub fn relation_input_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        RELATION_INPUT_ARITY_MISMATCH,
        "arity mismatch in relation inputs",
        vec![format!("expected: {expected}, actual: {actual}")],
    )
}

const FUNCTION_INPUT_ARITY_MISMATCH: &str = "runtime/function-input-arity-mismatch";

/// Reports the wrong number of function arguments.
pub fn function_input_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        FUNCTION_INPUT_ARITY_MISMATCH,
        "arity mismatch in function arguments",
        vec![format!("expected: {expected}, actual: {actual}")],
    )
}

const RELATION_OUTPUT_ARITY_MISMATCH: &str = "runtime/relation-output-arity-mismatch";

/// Reports the wrong number of extern relation outputs.
pub fn relation_output_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        RELATION_OUTPUT_ARITY_MISMATCH,
        "arity mismatch in relation outputs",
        vec![format!("expected: {expected}, actual: {actual}")],
    )
}

const RELATION_INPUT_TYPE_MISMATCH: &str = "runtime/relation-input-type-mismatch";

/// Reports relation input mismatch.
pub fn relation_input_type_mismatch(relation: String) -> Diagnostic {
    diagnostic(
        RELATION_INPUT_TYPE_MISMATCH,
        format!("relation input of {relation} does not match the expected type"),
        Vec::new(),
    )
}

const RELATION_OUTPUT_TYPE_MISMATCH: &str = "runtime/relation-output-type-mismatch";

/// Reports relation output mismatch.
pub fn relation_output_type_mismatch(relation: String) -> Diagnostic {
    diagnostic(
        RELATION_OUTPUT_TYPE_MISMATCH,
        format!("relation output of {relation} does not match the expected type"),
        Vec::new(),
    )
}

const FUNCTION_INPUT_TYPE_MISMATCH: &str = "runtime/function-input-type-mismatch";

/// Reports function input mismatch.
pub fn function_input_type_mismatch(func: String) -> Diagnostic {
    diagnostic(
        FUNCTION_INPUT_TYPE_MISMATCH,
        format!("function argument of {func} does not match the parameter type"),
        Vec::new(),
    )
}

const FUNCTION_OUTPUT_TYPE_MISMATCH: &str = "runtime/function-output-type-mismatch";

/// Reports function output mismatch.
pub fn function_output_type_mismatch(func: String) -> Diagnostic {
    diagnostic(
        FUNCTION_OUTPUT_TYPE_MISMATCH,
        format!("return value of function {func} does not match the expected type"),
        Vec::new(),
    )
}
