//! Structured causes and frames for interpreter failures
//!
//! These helpers create reports and attach source locations.
//! `Failure` distinguishes fatal errors from recoverable mismatches.
//! The renderer reads source files when displaying the reports.

use crate::{
    diagnostic::{Diagnostic, Label, Report, ReportKind, Severity},
    lang::{
        common::{
            ds::map::ArityMismatch, notation::mixop::ArityMismatch as MixopArityMismatch,
            prim::num::NumericError, source::Span,
        },
        data::value::ValueError,
        hints::input::InputError,
    },
    runtime::ops::{typ::TypeError, value::MatchError},
};
use std::fmt;

pub mod call;
pub mod context;
pub mod expr;
pub mod guard;
mod host;
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

/// Adds a primary source label to the diagnostic.
pub fn at(mut diagnostic: Diagnostic, span: Span) -> Error {
    diagnostic.labels.push(Label::primary(&span, ""));
    Box::new(diagnostic.into())
}

/// Adds a source label to a cause that has none, leaving frames unchanged.
pub fn locate(mut report: Error, span: &Span) -> Error {
    if let ReportKind::Cause(diagnostic) = &mut report.kind
        && diagnostic.labels.is_empty()
    {
        diagnostic.labels.push(Label::primary(span, ""));
    }
    report
}

// = Local operation conversions

const VALUE_INVALID: &str = "runtime/value-invalid";
const NUMERIC_INVALID: &str = "runtime/numeric-invalid";
const INPUT_INVALID: &str = "runtime/input-invalid";
const ARITY_MISMATCH: &str = "runtime/arity-mismatch";
const MIXOP_ARITY_MISMATCH: &str = "runtime/mixop-arity-mismatch";
const TYPE_INVALID: &str = "runtime/type-invalid";
const MATCH_FAILED: &str = "runtime/match-failed";

/// Adds a source label unless the span is unknown.
fn local(diagnostic: Diagnostic, span: Span) -> Error {
    if span == Span::default() { Box::new(diagnostic.into()) } else { at(diagnostic, span) }
}

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
from_error!(InputError, INPUT_INVALID);
from_error!(ArityMismatch, ARITY_MISMATCH);
from_error!(MixopArityMismatch, MIXOP_ARITY_MISMATCH);

impl From<TypeError> for Error {
    fn from(error: TypeError) -> Self {
        local(diagnostic(TYPE_INVALID, error.kind.to_string(), Vec::new()), error.span)
    }
}

/// Prints a `MatchError` without its spans.
struct MatchDisplay<'a>(&'a MatchError);
impl fmt::Display for MatchDisplay<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MatchError::UndefinedType { name, .. } => write!(formatter, "undefined type {name}"),
            MatchError::UnexpectedTypeVariable { .. } => {
                formatter.write_str("unexpected type variable")
            }
            MatchError::TypeArgumentMismatch { expected, actual, .. } => {
                write!(formatter, "expected {expected} type arguments, got {actual}")
            }
            MatchError::UndefinedFunction { name, .. } => {
                write!(formatter, "undefined function {name}")
            }
            MatchError::Type(error) => fmt::Display::fmt(&error.kind, formatter),
        }
    }
}

impl From<MatchError> for Error {
    fn from(error: MatchError) -> Self {
        let span = match &error {
            MatchError::UndefinedType { span, .. }
            | MatchError::UnexpectedTypeVariable { span }
            | MatchError::TypeArgumentMismatch { span, .. }
            | MatchError::UndefinedFunction { span, .. } => span.clone(),
            MatchError::Type(error) => error.span.clone(),
        };
        local(diagnostic(MATCH_FAILED, MatchDisplay(&error).to_string(), Vec::new()), span)
    }
}
