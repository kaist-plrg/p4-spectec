//! Call diagnostics for interpreter operations
//!
//! Builds diagnostics; callers choose whether to stop or try another candidate.

use crate::lang::common::source::Span;

use crate::diagnostic::{Diagnostic, Label};

use super::diagnostic;

const INSTRUCTION_NONDETERMINISTIC: &str = "runtime/instruction-nondeterministic";

/// Reports instruction nondeterminism.
pub fn instruction_nondeterministic(span: &Span) -> Diagnostic {
    let mut diagnostic = diagnostic(
        INSTRUCTION_NONDETERMINISTIC,
        "nondeterministic instruction evaluation",
        Vec::new(),
    );
    diagnostic
        .labels
        .push(Label::secondary(span, "first successful instruction"));
    diagnostic
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
