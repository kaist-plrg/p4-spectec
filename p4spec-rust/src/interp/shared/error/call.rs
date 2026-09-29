//! Call diagnostics for interpreter operations
//!
//! Constructors identify runtime checks without deciding whether callers retry.

use super::diagnostic;
use crate::diagnostic::Diagnostic;

const INSTRUCTION_NONDETERMINISTIC: &str = "runtime/instruction-nondeterministic";

/// Reports instruction nondeterminism.
pub fn instruction_nondeterministic() -> Diagnostic {
    diagnostic(INSTRUCTION_NONDETERMINISTIC, "nondeterministic instruction evaluation", Vec::new())
}

const FLOW_INVALID: &str = "runtime/flow-invalid";

/// Reports invalid flow.
pub fn flow_invalid(message: &'static str) -> Diagnostic {
    diagnostic(FLOW_INVALID, message, Vec::new())
}

const RULE_ARITY_MISMATCH: &str = "runtime/rule-arity-mismatch";

/// Reports rule arity mismatch.
pub fn rule_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        RULE_ARITY_MISMATCH,
        "arity mismatch in rule",
        vec![format!("expected: {expected}, actual: {actual}")],
    )
}

const TABLE_ROW_ARITY_MISMATCH: &str = "runtime/table-row-arity-mismatch";

/// Reports table row arity mismatch.
pub fn table_row_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        TABLE_ROW_ARITY_MISMATCH,
        "arity mismatch while matching table row",
        vec![format!("expected: {expected}, actual: {actual}")],
    )
}

const TYPE_ARGUMENT_ARITY_MISMATCH: &str = "runtime/type-argument-arity-mismatch";

/// Reports type argument arity mismatch.
pub fn type_argument_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        TYPE_ARGUMENT_ARITY_MISMATCH,
        "arity mismatch in type arguments",
        vec![format!("expected: {expected}, actual: {actual}")],
    )
}

const CLAUSE_ARITY_MISMATCH: &str = "runtime/clause-arity-mismatch";

/// Reports clause arity mismatch.
pub fn clause_arity_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        CLAUSE_ARITY_MISMATCH,
        "arity mismatch while matching clause",
        vec![format!("expected: {expected}, actual: {actual}")],
    )
}

const RELATION_NONDETERMINISTIC: &str = "runtime/relation-nondeterministic";

/// Reports relation nondeterminism.
pub fn relation_nondeterministic(
    relation: String,
    group_a: String,
    path_a: String,
    group_b: String,
    path_b: String,
) -> Diagnostic {
    diagnostic(
        RELATION_NONDETERMINISTIC,
        format!(
            "non-deterministic application of relation {relation}: {group_a}/{path_a}, {group_b}/{path_b}"
        ),
        Vec::new(),
    )
}

const FUNCTION_NONDETERMINISTIC: &str = "runtime/function-nondeterministic";

/// Reports function nondeterminism.
pub fn function_nondeterministic(func: String, clause_a: usize, clause_b: usize) -> Diagnostic {
    diagnostic(
        FUNCTION_NONDETERMINISTIC,
        format!("non-deterministic application of function {func}: {clause_a}, {clause_b}"),
        Vec::new(),
    )
}
