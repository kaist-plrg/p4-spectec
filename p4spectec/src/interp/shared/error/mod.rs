//! Structured causes and frames for interpreter failures
//!
//! These helpers create reports and attach source locations.
//! `InterpreterError` distinguishes fatal errors from recoverable mismatches.
//! The renderer reads source files when displaying the reports.

use std::fmt;

use crate::lang::{
    common::{ds::map::ArityMismatch, prim::num::NumericError, source::Span},
    data::{notation::ArityMismatch as MixopArityMismatch, value::ValueError},
};

use crate::diagnostic::{Diagnostic, Label, Report, Severity};

use crate::runtime::ops::{typ::TypeError, value::MatchError};

pub mod call;
pub mod context;
pub mod expr;
pub mod guard;
pub mod prem;
pub mod trace;

/// An interpreter error report.
pub type Error = Box<Report>;

/// Namespace of a failed context lookup.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EntityKind {
    Value,
    Type,
    DefinedType,
    Relation,
    Function,
}

impl fmt::Display for EntityKind {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::Value => "value",
            Self::Type => "type",
            Self::DefinedType => "defined type",
            Self::Relation => "relation",
            Self::Function => "function",
        })
    }
}

// = Report construction

/// Creates runtime diagnostic data without loading source files.
fn diagnostic(code: &str, message: impl Into<String>, notes: Vec<String>) -> Diagnostic {
    Diagnostic::new("runtime", Severity::Error, Some(code.to_owned()), message, Vec::new(), notes)
}

// = Local operation conversions

const VALUE_INVALID: &str = "runtime/value-invalid";
const NUMERIC_INVALID: &str = "runtime/numeric-invalid";
const ARITY_MISMATCH: &str = "runtime/arity-mismatch";
const MIXOP_ARITY_MISMATCH: &str = "runtime/mixop-arity-mismatch";
const TYPE_INVALID: &str = "runtime/type-invalid";
const MATCH_FAILED: &str = "runtime/match-failed";

/// Converts an error to a runtime diagnostic without source labels.
macro_rules! from_error {
    ($typ:ty, $code:ident) => {
        impl From<$typ> for Error {
            fn from(error: $typ) -> Self {
                Box::new(diagnostic($code, error.to_string(), Vec::new()).into())
            }
        }
    };
}
from_error!(ValueError, VALUE_INVALID);
from_error!(NumericError, NUMERIC_INVALID);
from_error!(ArityMismatch, ARITY_MISMATCH);
from_error!(MixopArityMismatch, MIXOP_ARITY_MISMATCH);

impl From<TypeError> for Error {
    fn from(error: TypeError) -> Self {
        let mut diagnostic_error = diagnostic(TYPE_INVALID, error.kind.to_string(), Vec::new());
        if error.span != Span::default() {
            diagnostic_error = diagnostic_error.with_label(Label::primary(&error.span, ""));
        }
        Box::new(Report::from(diagnostic_error))
    }
}

/// Prints a `MatchError` without its spans.
struct MatchDisplay<'a>(&'a MatchError);
impl fmt::Display for MatchDisplay<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MatchError::TypeUndefined { name, .. } => write!(formatter, "undefined type {name}"),
            MatchError::TypeVariableUnexpected { .. } => {
                formatter.write_str("unexpected type variable")
            }
            MatchError::TypeArgumentCountMismatch { expected, actual, .. } => {
                write!(formatter, "expected {expected} type arguments, got {actual}")
            }
            MatchError::FunctionUndefined { name, .. } => {
                write!(formatter, "undefined function {name}")
            }
            MatchError::Type(error) => fmt::Display::fmt(&error.kind, formatter),
        }
    }
}

impl From<MatchError> for Error {
    fn from(error: MatchError) -> Self {
        // Keep the failed operation's location when available
        let span = match &error {
            MatchError::TypeUndefined { span, .. }
            | MatchError::TypeVariableUnexpected { span }
            | MatchError::TypeArgumentCountMismatch { span, .. }
            | MatchError::FunctionUndefined { span, .. } => span.clone(),
            MatchError::Type(error) => error.span.clone(),
        };
        let mut diagnostic_error =
            diagnostic(MATCH_FAILED, MatchDisplay(&error).to_string(), Vec::new());
        // Leave unknown locations for the caller to fill
        if span != Span::default() {
            diagnostic_error = diagnostic_error.with_label(Label::primary(&span, ""));
        }
        Box::new(Report::from(diagnostic_error))
    }
}
