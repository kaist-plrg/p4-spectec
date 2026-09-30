//! Errors produced while evaluating specification builtins
//!
//! Every builtin returns a value or a fatal error.
//! Local causes stay typed until the interpreter diagnostic boundary.

use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, Report, Severity},
    interface::p4::error::P4UnparseError,
    lang::data::value::ValueError,
};

/// Why a builtin call failed.
#[derive(Debug, Error)]
pub enum BuiltinError {
    /// A complete diagnostic returned by a registered host builtin.
    #[error(transparent)]
    Report(#[from] Box<Report>),

    /// Wrong number of type arguments or values.
    #[error("arity mismatch: expected {expected}, got {actual}")]
    ArgumentCountMismatch { expected: usize, actual: usize },

    /// The specification declares a builtin this interface lacks.
    #[error("implementation for builtin {0} is missing")]
    ImplementationMissing(String),

    /// An argument had the right kind but an unusable value.
    #[error("{0}")]
    ArgumentInvalid(String),

    /// A value projection or construction failed.
    #[error(transparent)]
    Value(#[from] ValueError),

    /// A numeric conversion failed.
    #[error(transparent)]
    Numeric(#[from] crate::lang::common::prim::num::NumericError),

    /// `print_` could not render the value.
    #[error(transparent)]
    P4Unparse(#[from] P4UnparseError),
}

impl BuiltinError {
    /// An invalid-argument failure with a message.
    pub fn new(message: impl Into<String>) -> Self {
        Self::ArgumentInvalid(message.into())
    }

    /// An arity failure.
    pub fn arity(expected: usize, actual: usize) -> Self {
        Self::ArgumentCountMismatch { expected, actual }
    }
}

const BUILTIN_ARGUMENT_ARITY_MISMATCH: &str = "runtime/builtin-argument-arity-mismatch";
const BUILTIN_IMPLEMENTATION_MISSING: &str = "runtime/builtin-implementation-missing";
const BUILTIN_ARGUMENT_INVALID: &str = "runtime/builtin-argument-invalid";
const BUILTIN_VALUE_INVALID: &str = "runtime/builtin-value-invalid";
const BUILTIN_PRINT_UNSUPPORTED: &str = "runtime/builtin-print-unsupported";

impl BuiltinError {
    /// Describes a local builtin failure at the host operation boundary.
    pub fn into_report(self) -> Box<Report> {
        // Forward extension diagnostics before interpreting local failure kinds
        let kind = match self {
            BuiltinError::Report(report) => return report,
            kind => kind,
        };
        let code = match &kind {
            BuiltinError::Report(_) => unreachable!(),
            BuiltinError::ArgumentCountMismatch { .. } => BUILTIN_ARGUMENT_ARITY_MISMATCH,
            BuiltinError::ImplementationMissing(_) => BUILTIN_IMPLEMENTATION_MISSING,
            BuiltinError::ArgumentInvalid(_) => BUILTIN_ARGUMENT_INVALID,
            BuiltinError::Value(_) | BuiltinError::Numeric(_) => BUILTIN_VALUE_INVALID,
            BuiltinError::P4Unparse(_) => BUILTIN_PRINT_UNSUPPORTED,
        };
        Box::new(
            Diagnostic::new(
                "runtime",
                Severity::Error,
                Some(code.to_owned()),
                kind.to_string(),
                vec![],
                vec![],
            )
            .into(),
        )
    }
}
