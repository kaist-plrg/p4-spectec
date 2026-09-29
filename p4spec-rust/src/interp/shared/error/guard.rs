//! Guard diagnostics for interpreter operations
//!
//! Constructors identify runtime checks without deciding whether callers retry.

use super::diagnostic;
use crate::diagnostic::Diagnostic;

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
