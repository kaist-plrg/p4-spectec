//! Context diagnostics for interpreter operations
//!
//! Builds diagnostics; callers choose whether to stop or try another candidate.
//! Loading also reports extern syntax types without one host definition.

use crate::lang::{data::value::ValueError, il::prepared as ast};

use crate::diagnostic::{Diagnostic, Label, Report};

use super::{EntityKind, Error, diagnostic};

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

const EXTERN_TYPE_UNDEFINED: &str = "runtime/extern-type-undefined";

/// Reports an extern syntax type without a host definition.
pub fn extern_type_undefined(id: &ast::Id) -> Error {
    let diagnostic = diagnostic(
        EXTERN_TYPE_UNDEFINED,
        format!("extern type `{}` has no host definition", id.node),
        Vec::new(),
    )
    .with_label(Label::primary(&id.span, ""));
    Box::new(Report::from(diagnostic))
}

const EXTERN_TYPE_REPEATED: &str = "runtime/extern-type-repeated";

/// Reports host definitions that cannot be registered together.
pub fn extern_types_invalid(error: ValueError) -> Error {
    match error {
        // Two definitions claim one name
        ValueError::TypDefRepeated { name } => Box::new(Report::from(diagnostic(
            EXTERN_TYPE_REPEATED,
            format!("extern type `{name}` has more than one host definition"),
            Vec::new(),
        ))),
        // Other failures, such as a definition from another arena
        error => Error::from(error),
    }
}
