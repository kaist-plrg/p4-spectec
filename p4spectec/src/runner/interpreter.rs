//! Stage-specific evaluation contract used by a composed runner
//!
//! An interpreter owns its configuration and cache for one language stage.
//! Evaluation receives the assembled runner context, including the interpreter,
//! so extern calls can reenter without a second mutable interpreter borrow.
//! Mismatches allow another candidate; host failures always abort.

use crate::lang::{common::source::Span, data::value::Value};

use crate::lang::il::ast::Typ;

use crate::diagnostic::Report;

use crate::runner::{ExternError, InterfaceError};

use super::{Extern, Interface, RunnerContext};

// == Interpreter errors

/// Separates aborting execution from trying another candidate.
#[derive(Debug)]
pub enum InterpreterError {
    /// Aborts execution without retrying another candidate.
    Fatal(Box<Report>),
    /// Retains the ordered reports of candidates that did not match.
    Mismatch(Vec<Report>),
}

// = Error reports

impl InterpreterError {
    /// Returns the fatal report or groups mismatches under an execution frame.
    pub fn into_report(self) -> Box<Report> {
        match self {
            Self::Fatal(report) => report,
            Self::Mismatch(reports) => {
                Box::new(Report::frame(Span::default(), "execution failed", reports))
            }
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
    pub fn with_span(self, span: &Span) -> Self {
        match self {
            Self::Fatal(mut report) => {
                *report = report.with_span(span);
                Self::Fatal(report)
            }
            Self::Mismatch(reports) => Self::Mismatch(
                reports
                    .into_iter()
                    .map(|report| report.with_span(span))
                    .collect(),
            ),
        }
    }
}

impl From<Box<Report>> for InterpreterError {
    fn from(report: Box<Report>) -> Self {
        Self::Fatal(report)
    }
}

impl From<InterfaceError> for InterpreterError {
    fn from(error: InterfaceError) -> Self {
        Self::Fatal(error.into_report())
    }
}

impl From<ExternError> for InterpreterError {
    fn from(error: ExternError) -> Self {
        Self::Fatal(error.into_report())
    }
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fatal(report) => std::fmt::Display::fmt(report, fmt),
            Self::Mismatch(_) => fmt.write_str("execution did not match"),
        }
    }
}
impl std::error::Error for InterpreterError {}

// == Interpreter contract

/// A language stage's evaluator, parameterized by its host components.
pub trait Interpreter<Iface, Ext>: Sized
where
    Iface: Interface,
    Ext: Extern,
{
    /// Loaded global definitions.
    type Spec;

    /// Clears cached results without invalidating arena values.
    fn clear(&mut self);

    /// Resets program-owned execution state while retaining configuration.
    fn reset(&mut self);

    /// Evaluates an already parsed program through the selected entry.
    fn eval_program(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        program: Value,
    ) -> Result<Vec<Value>, InterpreterError>;

    /// Calls a relation by name.
    ///
    /// AL, SL, and PL validate the name and input count on every call.
    /// With type guards disabled, values must match the declared input types.
    fn eval_rel(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError>;

    /// Calls a function by name with type arguments.
    ///
    /// AL, SL, and PL validate the name and argument counts on every call.
    /// With type guards disabled, values and type arguments must match
    /// the declared signature.
    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError>;
}
