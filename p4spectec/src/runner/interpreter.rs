//! Stage-specific evaluation contract used by a composed runner
//!
//! An interpreter owns its configuration and cache for one language stage.
//! Evaluation receives the assembled runner context, including the interpreter,
//! so extern calls can reenter without a second mutable interpreter borrow.
//! Mismatches allow another candidate; host failures always abort.

use crate::lang::{common::source::Span, data::value::flat::Value};

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

// == Evaluation effects

/// Whether evaluation changed host state or performed an observable effect.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum SideEffect {
    /// No observable effect occurred.
    #[default]
    Pure,
    /// At least one observable effect occurred.
    Impure,
}

impl SideEffect {
    /// Includes a child evaluation's effect before exposing its result.
    pub fn include<T, E>(&mut self, eval: Eval<T, E>) -> Result<T, E> {
        if eval.effect == Self::Impure {
            *self = Self::Impure;
        }
        eval.result
    }
}

/// An evaluation result together with effects retained even on failure.
#[derive(Debug)]
#[must_use]
pub struct Eval<T, E = InterpreterError> {
    /// The computed value or evaluation failure.
    pub result: Result<T, E>,
    /// Effects of every child that actually ran, including failed candidates.
    pub effect: SideEffect,
}

impl<T, E> Eval<T, E> {
    /// Pairs a result with its evaluation effect.
    pub fn new(result: Result<T, E>, effect: SideEffect) -> Self {
        Self { result, effect }
    }

    /// Evaluates a body with a local effect accumulator that survives `?`.
    pub fn with(evaluate: impl FnOnce(&mut SideEffect) -> Result<T, E>) -> Self {
        let mut effect = SideEffect::Pure;
        let result = evaluate(&mut effect);
        Self { result, effect }
    }

    /// Converts an error while preserving the evaluation effect.
    pub fn map_err<F>(self, convert: impl FnOnce(E) -> F) -> Eval<T, F> {
        Eval { result: self.result.map_err(convert), effect: self.effect }
    }
}

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
    ) -> Eval<Vec<Value>, InterpreterError>;

    /// Calls a relation by name.
    ///
    /// AL, SL, and PL validate the name and input count on every call.
    /// With type guards disabled, values must match the declared input types.
    fn eval_rel(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, InterpreterError>;

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
    ) -> Eval<Value, InterpreterError>;
}
