//! Structured causes and frames for interpreter failures
//!
//! Constructors retain runtime error data without choosing recovery behavior.
//! Evaluation transports reports through the separate Failure control carrier;
//! only output consumers render source snippets.

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

pub mod assign;
pub mod call;
pub mod context;
pub mod expr;
pub mod guard;
mod host;
pub mod prem;
pub mod trace;

/// Names report-only failures during loading and context operations.
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

/// Locates a newly authored diagnostic at its owning operation.
pub fn at(mut diagnostic: Diagnostic, span: Span) -> Error {
    diagnostic.labels.push(Label::primary(&span, ""));
    Box::new(diagnostic.into())
}

/// Locates an unlocated local cause without replacing meaningful source labels.
pub fn locate(mut report: Error, span: &Span) -> Error {
    if let ReportKind::Cause(diagnostic) = &mut report.kind
        && diagnostic.labels.is_empty()
    {
        diagnostic.labels.push(Label::primary(span, ""));
    }
    report
}

// = Local operation conversions

/// Retains a local operation's span, leaving unknown locations for its caller.
fn local(diagnostic: Diagnostic, span: Span) -> Error {
    if span == Span::default() { Box::new(diagnostic.into()) } else { at(diagnostic, span) }
}

/// Converts local typed failures before their operation supplies a source span.
macro_rules! from_error {
    ($typ:ty, $code:literal) => {
        impl From<$typ> for Error {
            fn from(error: $typ) -> Self {
                Box::new(diagnostic($code, error.to_string(), Vec::new()).into())
            }
        }
    };
}
from_error!(ValueError, "runtime/value-invalid");
from_error!(NumericError, "runtime/numeric-invalid");
from_error!(InputError, "runtime/input-invalid");
from_error!(ArityMismatch, "runtime/arity-mismatch");
from_error!(MixopArityMismatch, "runtime/mixop-arity-mismatch");

impl From<TypeError> for Error {
    fn from(error: TypeError) -> Self {
        local(diagnostic("runtime/type-invalid", error.kind.to_string(), Vec::new()), error.span)
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
        local(
            diagnostic("runtime/match-failed", MatchDisplay(&error).to_string(), Vec::new()),
            span,
        )
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
