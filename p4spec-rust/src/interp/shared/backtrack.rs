//! Recoverable mismatches and fatal interpreter failures
//!
//! Failure classification survives runner and extern reentry.
//! Ordinary Result propagation preserves both variants;
//! only the final output boundary promotes exhausted alternatives to a report.

use super::error::{self, Error};
use crate::{
    diagnostic::{Diagnostic, Report},
    lang::common::source::Span,
    runner::{ExternError, InterfaceError},
};

/// Separates aborting execution from trying another candidate.
#[derive(Debug)]
pub enum Failure {
    /// Aborts execution without retrying another candidate.
    Fatal(Error),
    /// Retains the ordered reports of candidates that did not match.
    Mismatch(Vec<Report>),
}

/// Carries a result without erasing recoverability.
pub type Backtrack<T> = Result<T, Failure>;

// = Failure transport

impl Failure {
    /// Converts exhausted mismatch at the final output boundary.
    pub fn into_report(self) -> Error {
        match self {
            Self::Fatal(report) => report,
            Self::Mismatch(reports) => error::trace::execution(reports),
        }
    }

    /// Adds context without changing classification or replacing incoming labels.
    pub fn with_frame(self, span: Span, message: impl Into<String>) -> Self {
        match self {
            Self::Fatal(report) => {
                Self::Fatal(Box::new(Report::frame(span, message, vec![*report])))
            }
            Self::Mismatch(reports) => Self::Mismatch(vec![Report::frame(span, message, reports)]),
        }
    }

    /// Locates local causes without rewriting existing report context.
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
        // Classify builtin rejection before converting its diagnostic payload
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

impl From<crate::lang::data::value::ValueError> for Failure {
    fn from(error: crate::lang::data::value::ValueError) -> Self {
        Self::Fatal(error.into())
    }
}

impl From<crate::lang::common::prim::num::NumericError> for Failure {
    fn from(error: crate::lang::common::prim::num::NumericError) -> Self {
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

// = Local operation lifting

/// Lifts a local operation failure at its owning source location.
pub fn from_result<T>(result: Result<T, impl Into<Error>>, span: &Span) -> Backtrack<T> {
    result.map_err(|error| Failure::Fatal(error::locate(error.into(), span)))
}

/// Rejects a violated runtime check at the owning operation.
pub fn check(
    condition: bool,
    span: Span,
    diagnostic: impl FnOnce() -> Diagnostic,
) -> Backtrack<()> {
    if condition { Ok(()) } else { Err(Failure::Fatal(error::at(diagnostic(), span))) }
}

/// Adds frames lazily to evaluation results.
pub trait BacktrackExt<T> {
    /// Wraps failures without formatting messages on the successful path.
    fn nest(self, span: Span, message: impl FnOnce() -> String) -> Self;
}

impl<T> BacktrackExt<T> for Backtrack<T> {
    fn nest(self, span: Span, message: impl FnOnce() -> String) -> Self {
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
macro_rules! err {
    ($span:expr, $diagnostic:expr $(,)?) => {
        Err($crate::interp::shared::backtrack::Failure::Fatal(
            $crate::interp::shared::error::at($diagnostic, $span)))
    };
    ($($report:tt)*) => {
        Err($crate::interp::shared::backtrack::Failure::Fatal($($report)*))
    };
}
pub(crate) use err;

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

/// Propagates the complete typed failure, like the question-mark operator.
macro_rules! unwrap {
    ($result:expr) => {
        $result?
    };
}
pub(crate) use unwrap;

/// Lifts a local operation at its source location before propagation.
macro_rules! unwrap_from_result {
    ($result:expr, $span:expr $(,)?) => {
        $crate::interp::shared::backtrack::from_result($result, $span)?
    };
}
pub(crate) use unwrap_from_result;
