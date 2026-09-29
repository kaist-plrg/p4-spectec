//! Context diagnostics for interpreter operations
//!
//! Builds diagnostics; callers choose whether to stop or try another candidate.

use super::EntityKind;
use super::diagnostic;
use crate::diagnostic::Diagnostic;

const BINDING_UNDEFINED: &str = "runtime/binding-undefined";

/// Reports an undefined binding.
pub fn binding_undefined(kind: EntityKind, name: String) -> Diagnostic {
    diagnostic(BINDING_UNDEFINED, format!("{kind} `{name}` is undefined"), Vec::new())
}

const BINDING_REPEATED: &str = "runtime/binding-repeated";

/// Reports a duplicate binding.
pub fn binding_repeated(kind: EntityKind, name: String) -> Diagnostic {
    diagnostic(BINDING_REPEATED, format!("{kind} `{name}` was already defined"), Vec::new())
}

const ITERATION_OPTIONALITY_MISMATCH: &str = "runtime/iteration-optionality-mismatch";

/// Reports optionality mismatch.
pub fn iteration_optionality_mismatch() -> Diagnostic {
    diagnostic(
        ITERATION_OPTIONALITY_MISMATCH,
        "mismatch in optionality of iterated variables",
        Vec::new(),
    )
}

const ITERATION_LENGTH_MISMATCH: &str = "runtime/iteration-length-mismatch";

/// Reports iteration length mismatch.
pub fn iteration_length_mismatch(expected: usize, actual: usize) -> Diagnostic {
    diagnostic(
        ITERATION_LENGTH_MISMATCH,
        "cannot transpose a matrix of value batches",
        vec![format!("expected: {expected}, actual: {actual}")],
    )
}
