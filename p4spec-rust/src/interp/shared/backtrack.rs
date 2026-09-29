//! Recoverable mismatches and fatal interpreter failures
//!
//! `Fatal` stops execution; `Mismatch` lets the caller try another candidate.
//! Runner and extern calls preserve this distinction.
//! `into_report` groups mismatches under an execution frame for display.

use super::error::{self, Error};
use crate::{
    diagnostic::{Diagnostic, Report},
    lang::{
        common::{prim::num::NumericError, source::Span},
        data::value::ValueError,
    },
    runner::{ExternError, InterfaceError},
};

/// Returns a value, a fatal error, or a mismatch.
pub type Backtrack<T> = Result<T, Failure>;

/// Separates aborting execution from trying another candidate.
#[derive(Debug)]
pub enum Failure {
    /// Aborts execution without retrying another candidate.
    Fatal(Error),
    /// Retains the ordered reports of candidates that did not match.
    Mismatch(Vec<Report>),
}

// = Failure reports

impl Failure {
    /// Returns the fatal report or groups mismatches under an execution frame.
    pub fn into_report(self) -> Error {
        match self {
            Self::Fatal(report) => report,
            Self::Mismatch(reports) => error::trace::execution(reports),
        }
    }

    /// Adds a frame while keeping the failure kind and child reports.
    pub fn with_frame(self, span: Span, message: impl Into<String>) -> Self {
        match self {
            Self::Fatal(report) => {
                Self::Fatal(Box::new(Report::frame(span, message, vec![*report])))
            }
            Self::Mismatch(reports) => Self::Mismatch(vec![Report::frame(span, message, reports)]),
        }
    }

    /// Adds a source label to causes that have none, leaving frames unchanged.
    pub fn at_if_missing(self, span: &Span) -> Self {
        match self {
            Self::Fatal(report) => Self::Fatal(error::locate(report, span)),
            Self::Mismatch(reports) => Self::Mismatch(
                reports
                    .into_iter()
                    .map(|report| *error::locate(Box::new(report), span))
                    .collect(),
            ),
        }
    }
}

impl From<Error> for Failure {
    fn from(report: Error) -> Self {
        Self::Fatal(report)
    }
}

impl From<InterfaceError> for Failure {
    fn from(error: InterfaceError) -> Self {
        // Builtin failures are mismatches; an unconfigured interface is fatal
        let recoverable = matches!(&error, InterfaceError::Builtin(_));
        let report: Error = error.into();
        if recoverable { Self::Mismatch(vec![*report]) } else { Self::Fatal(report) }
    }
}

impl From<ExternError> for Failure {
    fn from(error: ExternError) -> Self {
        Self::Fatal(error.into())
    }
}

impl From<ValueError> for Failure {
    fn from(error: ValueError) -> Self {
        Self::Fatal(error.into())
    }
}

impl From<NumericError> for Failure {
    fn from(error: NumericError) -> Self {
        Self::Fatal(error.into())
    }
}

impl std::fmt::Display for Failure {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fatal(report) => std::fmt::Display::fmt(report, fmt),
            Self::Mismatch(_) => fmt.write_str("execution did not match"),
        }
    }
}
impl std::error::Error for Failure {}

// = Error conversion

/// Converts an error to Fatal, adding a source label if missing.
pub fn from_result<T>(result: Result<T, impl Into<Error>>, span: &Span) -> Backtrack<T> {
    result.map_err(|error| Failure::Fatal(error::locate(error.into(), span)))
}

/// Returns Fatal at the given span if the condition is false.
pub fn check(
    condition: bool,
    span: Span,
    diagnostic: impl FnOnce() -> Diagnostic,
) -> Backtrack<()> {
    if condition { Ok(()) } else { Err(Failure::Fatal(error::at(diagnostic(), span))) }
}

/// Adds frames lazily to evaluation results.
pub trait WithFrame<T> {
    /// Wraps failures without formatting messages on the successful path.
    fn with_frame(self, span: Span, message: impl FnOnce() -> String) -> Self;
}

impl<T> WithFrame<T> for Backtrack<T> {
    fn with_frame(self, span: Span, message: impl FnOnce() -> String) -> Self {
        self.map_err(|failure| failure.with_frame(span, message()))
    }
}

// = Control syntax

/// Constructs or matches a successful result.
macro_rules! ok {
    ($($value:tt)*) => { Ok($($value)*) };
}
pub(crate) use ok;

/// Constructs or matches a fatal report.
macro_rules! fatal {
    ($span:expr, $diagnostic:expr $(,)?) => {
        Err($crate::interp::shared::backtrack::Failure::Fatal(
            $crate::interp::shared::error::at($diagnostic, $span)))
    };
    ($($report:tt)*) => {
        Err($crate::interp::shared::backtrack::Failure::Fatal($($report)*))
    };
}
pub(crate) use fatal;

/// Constructs or matches recoverable alternatives.
macro_rules! unmatch {
    ($span:expr, $diagnostic:expr $(,)?) => {
        Err($crate::interp::shared::backtrack::Failure::Mismatch(vec![
            *$crate::interp::shared::error::at($diagnostic, $span)]))
    };
    ($($reports:tt)*) => {
        Err($crate::interp::shared::backtrack::Failure::Mismatch($($reports)*))
    };
}
pub(crate) use unmatch;

/// Propagates a failure with `?`.
macro_rules! unwrap {
    ($result:expr) => {
        $result?
    };
}
pub(crate) use unwrap;

/// Converts an error with `from_result`, then propagates it with `?`.
macro_rules! unwrap_from_result {
    ($result:expr, $span:expr $(,)?) => {
        $crate::interp::shared::backtrack::from_result($result, $span)?
    };
}
pub(crate) use unwrap_from_result;
