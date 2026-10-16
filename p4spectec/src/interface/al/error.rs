//! Located errors at the AL representation boundary
//!
//! Conversion keeps allocation, notation, and malformed-data causes distinct.
//! `into_report` attaches the meta value's location without rendering sources.

use crate::{
    diagnostic::{Diagnostic, Label, Report, Severity},
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            notation::{self, flat::parse::ParseMixopError},
            value::{ValueError, flat::Value},
        },
        hints::input::InputError,
        traits::print::Print,
    },
};

/// A conversion failure with its source location.
#[derive(Debug, thiserror::Error)]
#[error("{kind}")]
pub struct Error {
    /// The nearest available source location.
    pub span: Span,
    /// The underlying conversion failure.
    #[source]
    pub kind: ErrorKind,
}

/// The cause of a conversion failure.
#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    /// Relation input positions are invalid.
    #[error(transparent)]
    Input(#[from] InputError),
    /// A value projection or arena allocation failed.
    #[error(transparent)]
    Value(#[from] ValueError),
    /// A notation has a different number of arguments.
    #[error(transparent)]
    Arity(#[from] notation::ArityMismatch),
    /// Parsing an encoded atom or notation failed.
    #[error(transparent)]
    Syntax(Box<Report>),
    /// A meta constructor or payload has an invalid shape.
    #[error("expected {expected}, found {actual}")]
    Malformed { expected: &'static str, actual: String },
}

impl Error {
    /// Creates a located malformed-data diagnostic.
    pub(crate) fn malformed(span: Span, expected: &'static str, actual: impl Into<String>) -> Self {
        Self { span, kind: ErrorKind::Malformed { expected, actual: actual.into() } }
    }

    /// Describes a rejected meta value without discarding its location.
    pub(crate) fn unexpected(arena: &Arena, value: Value, expected: &'static str) -> Self {
        Self::malformed(*arena.value.span(value.span), expected, value.view(arena).to_string())
    }

    /// Supplies the location of an enclosing conversion.
    pub(crate) fn at(mut self, span: Span) -> Self {
        if self.span == Span::default() {
            self.span = span;
        }
        self
    }

    /// Converts a failure into a diagnostic while retaining parser causes.
    pub fn into_report(self) -> Box<Report> {
        let Self { span, kind } = self;
        // Keep parser diagnostics as children of the conversion context
        if let ErrorKind::Syntax(report) = kind {
            return Box::new(Report::frame(span, "decoding AL notation", vec![*report]));
        }
        // Value and notation failures remain classified at the boundary
        let code = match &kind {
            ErrorKind::Value(_) => "metacirc/value-invalid",
            ErrorKind::Arity(_) => "metacirc/notation-arity",
            _ => "metacirc/representation-invalid",
        };
        Box::new(
            Diagnostic::new(
                "metacirc",
                Severity::Error,
                Some(code.into()),
                kind.to_string(),
                vec![Label::primary(&span, "invalid AL representation")],
                vec![],
            )
            .into(),
        )
    }
}

impl From<ValueError> for Error {
    fn from(error: ValueError) -> Self {
        Self { span: Span::default(), kind: error.into() }
    }
}
impl From<notation::ArityMismatch> for Error {
    fn from(error: notation::ArityMismatch) -> Self {
        Self { span: Span::default(), kind: error.into() }
    }
}
impl From<Box<Report>> for Error {
    fn from(report: Box<Report>) -> Self {
        Self { span: Span::default(), kind: ErrorKind::Syntax(report) }
    }
}

impl From<InputError> for Error {
    fn from(error: InputError) -> Self {
        Self { span: Span::default(), kind: error.into() }
    }
}

impl From<ParseMixopError> for Error {
    fn from(error: ParseMixopError) -> Self {
        match error {
            ParseMixopError::Syntax(error) => Self::from(error),
            ParseMixopError::Intern(error) => Self::from(ValueError::from(error)),
        }
    }
}
