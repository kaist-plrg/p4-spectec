//! Errors produced while evaluating specification builtins
//!
//! A builtin failure is recoverable to the interpreter,
//! which tries the next candidate;
//! value errors inside a builtin are wrapped the same way.

use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, Report, Severity},
    interface::p4::error::P4UnparseError,
    lang::data::value::ValueError,
};

/// Why a builtin call failed.
#[derive(Debug, Error)]
pub enum BuiltinErrorKind {
    /// A complete diagnostic returned by a registered host builtin.
    #[error(transparent)]
    Report(#[from] Box<Report>),

    /// Wrong number of type arguments or values.
    #[error("arity mismatch: expected {expected}, got {actual}")]
    ArityMismatch { expected: usize, actual: usize },

    /// The specification declares a builtin this interface lacks.
    #[error("implementation for builtin {0} is missing")]
    MissingImplementation(String),

    /// An argument had the right kind but an unusable value.
    #[error("{0}")]
    InvalidArgument(String),

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

/// A builtin failure.
#[derive(Debug, Error)]
#[error("{kind}")]
pub struct BuiltinError {
    pub kind: BuiltinErrorKind,
}

impl BuiltinError {
    /// An invalid-argument failure with a message.
    pub fn new(message: impl Into<String>) -> Self {
        Self { kind: BuiltinErrorKind::InvalidArgument(message.into()) }
    }

    /// An arity failure.
    pub fn arity(expected: usize, actual: usize) -> Self {
        Self { kind: BuiltinErrorKind::ArityMismatch { expected, actual } }
    }
}

impl From<ValueError> for BuiltinError {
    fn from(error: ValueError) -> Self {
        Self { kind: BuiltinErrorKind::Value(error) }
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
        let Self { kind } = self;
        let kind = match kind {
            BuiltinErrorKind::Report(report) => return report,
            kind => kind,
        };
        let code = match &kind {
            BuiltinErrorKind::Report(_) => unreachable!(),
            BuiltinErrorKind::ArityMismatch { .. } => BUILTIN_ARGUMENT_ARITY_MISMATCH,
            BuiltinErrorKind::MissingImplementation(_) => BUILTIN_IMPLEMENTATION_MISSING,
            BuiltinErrorKind::InvalidArgument(_) => BUILTIN_ARGUMENT_INVALID,
            BuiltinErrorKind::Value(_) | BuiltinErrorKind::Numeric(_) => BUILTIN_VALUE_INVALID,
            BuiltinErrorKind::P4Unparse(_) => BUILTIN_PRINT_UNSUPPORTED,
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

impl From<crate::lang::common::prim::num::NumericError> for BuiltinError {
    fn from(error: crate::lang::common::prim::num::NumericError) -> Self {
        Self { kind: BuiltinErrorKind::Numeric(error) }
    }
}

impl From<Box<Report>> for BuiltinError {
    fn from(report: Box<Report>) -> Self {
        Self { kind: BuiltinErrorKind::Report(report) }
    }
}
