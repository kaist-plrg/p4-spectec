//! Evaluation helpers for recoverable mismatches and fatal failures
//!
//! Macros propagate the runner error and add diagnostics or evaluation frames.
//! The runner owns the distinction between fatal errors and mismatches.

use crate::lang::{
    common::{prim::num::NumericError, source::Span},
    data::value::ValueError,
};

use crate::diagnostic::{Diagnostic, Label, Report};

use crate::runner::InterpreterError;

use super::error::Error;

/// Returns a value, a fatal error, or a mismatch.
pub type Backtrack<T> = Result<T, InterpreterError>;

impl From<ValueError> for InterpreterError {
    fn from(error: ValueError) -> Self {
        Self::Fatal(error.into())
    }
}

impl From<NumericError> for InterpreterError {
    fn from(error: NumericError) -> Self {
        Self::Fatal(error.into())
    }
}

// = Error conversion

/// Converts an error to Fatal, adding a source label if missing.
pub fn from_result<T>(result: Result<T, impl Into<Error>>, span: &Span) -> Backtrack<T> {
    result.map_err(|error| InterpreterError::Fatal(error.into()).with_span(span))
}

/// Returns Fatal at the given span if the condition is false.
pub fn check(
    condition: bool,
    span: Span,
    diagnostic: impl FnOnce() -> Diagnostic,
) -> Backtrack<()> {
    if condition {
        Ok(())
    } else {
        let diagnostic = diagnostic().with_label(Label::primary(&span, ""));
        Err(InterpreterError::Fatal(Box::new(Report::from(diagnostic))))
    }
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
    ($span:expr, $diagnostic:expr $(,)?) => {{
        use $crate::diagnostic::{Label, Report};
        use $crate::runner::InterpreterError;
        let diagnostic = ($diagnostic).with_label(Label::primary(&$span, ""));
        Err(InterpreterError::Fatal(Box::new(Report::from(diagnostic))))
    }};
    ($($report:tt)*) => {
        Err($crate::runner::InterpreterError::Fatal($($report)*))
    };
}
pub(crate) use fatal;

/// Constructs or matches recoverable alternatives.
macro_rules! unmatch {
    ($span:expr, $diagnostic:expr $(,)?) => {{
        use $crate::diagnostic::{Label, Report};
        use $crate::runner::InterpreterError;
        let diagnostic = ($diagnostic).with_label(Label::primary(&$span, ""));
        Err(InterpreterError::Mismatch(vec![Report::from(diagnostic)]))
    }};
    ($($reports:tt)*) => {
        Err($crate::runner::InterpreterError::Mismatch($($reports)*))
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
