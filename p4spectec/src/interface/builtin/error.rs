//! Fatal diagnostics produced while evaluating specification builtins
//!
//! Builtin operations describe their failures here.
//! The interpreter attaches the call location and stops execution.

use thiserror::Error;

use crate::lang::{common::prim::num::NumericError, data::value::ValueError};

use crate::diagnostic::{Diagnostic, Report, Severity};

use crate::interface::p4::error::P4UnparseError;

/// A fatal diagnostic produced by a builtin call.
#[derive(Debug, Error)]
#[error(transparent)]
pub struct BuiltinError(#[from] pub Box<Report>);

const BUILTIN_ARGUMENT_ARITY_MISMATCH: &str = "runtime/builtin-argument-arity-mismatch";
const BUILTIN_IMPLEMENTATION_MISSING: &str = "runtime/builtin-implementation-missing";
const BUILTIN_ARGUMENT_INVALID: &str = "runtime/builtin-argument-invalid";
const BUILTIN_VALUE_INVALID: &str = "runtime/builtin-value-invalid";
const BUILTIN_PRINT_UNSUPPORTED: &str = "runtime/builtin-print-unsupported";

impl BuiltinError {
    /// Creates a diagnostic without a source location.
    fn diagnostic(code: &str, message: impl Into<String>) -> Self {
        Self(Box::new(
            Diagnostic::new(
                "runtime",
                Severity::Error,
                Some(code.to_owned()),
                message,
                vec![],
                vec![],
            )
            .into(),
        ))
    }

    /// Describes an invalid builtin argument.
    pub fn argument_invalid(message: impl Into<String>) -> Self {
        Self::diagnostic(BUILTIN_ARGUMENT_INVALID, message)
    }

    /// Describes an argument count mismatch.
    pub fn arity(expected: usize, actual: usize) -> Self {
        Self::diagnostic(
            BUILTIN_ARGUMENT_ARITY_MISMATCH,
            format!("arity mismatch: expected {expected}, got {actual}"),
        )
    }

    /// Describes a declared builtin with no implementation.
    pub fn implementation_missing(name: &str) -> Self {
        Self::diagnostic(
            BUILTIN_IMPLEMENTATION_MISSING,
            format!("implementation for builtin {name} is missing"),
        )
    }

    /// Returns the builtin diagnostic without reconstructing it.
    pub fn into_report(self) -> Box<Report> {
        self.0
    }
}

impl From<ValueError> for BuiltinError {
    fn from(error: ValueError) -> Self {
        Self::diagnostic(BUILTIN_VALUE_INVALID, error.to_string())
    }
}

impl From<NumericError> for BuiltinError {
    fn from(error: NumericError) -> Self {
        Self::diagnostic(BUILTIN_VALUE_INVALID, error.to_string())
    }
}

impl From<P4UnparseError> for BuiltinError {
    fn from(error: P4UnparseError) -> Self {
        Self::diagnostic(BUILTIN_PRINT_UNSUPPORTED, error.to_string())
    }
}
