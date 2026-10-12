//! Four-state results for elaboration alternatives
//!
//! Unavailable rules and mismatches permit another candidate on the original context.
//! A fatal failure stops the search immediately.
//! Every failure retains complete reports in source candidate order.

use crate::lang::common::source::Span;

use crate::diagnostic::{LabelStyle, Report, ReportKind};

use super::{
    context::Context,
    error::{self, ElabError},
};

// == Result

/// A successful elaboration, unavailable rule, mismatch, or fatal failure.
#[derive(Debug)]
pub(super) enum Backtrack<T> {
    /// The operation produced a value.
    Success(T),
    /// The rule's prerequisites did not hold; another candidate may be tried.
    Unavailable(Vec<Report>),
    /// An applicable rule failed; another candidate may be tried.
    Mismatch(Vec<Report>),
    /// The operation failed and no alternative may be tried.
    Fatal(Vec<Report>),
}

// == Macros

/// Builds [`Backtrack::Success`].
macro_rules! success {
    ($($value:tt)*) => {
        $crate::pass::elaborate::backtrack::Backtrack::Success($($value)*)
    };
}
pub(super) use success;

/// Builds [`Backtrack::Unavailable`] from a report list.
macro_rules! unavailable {
    (error: $error:expr $(,)?) => {
        $crate::pass::elaborate::backtrack::Backtrack::Unavailable(vec![*$error])
    };
    (report: $report:expr $(,)?) => {
        $crate::pass::elaborate::backtrack::Backtrack::Unavailable(vec![$report])
    };
    ($($reports:tt)*) => {
        $crate::pass::elaborate::backtrack::Backtrack::Unavailable($($reports)*)
    };
}
pub(super) use unavailable;

/// Builds [`Backtrack::Fatal`] from a report list.
macro_rules! fatal {
    (error: $error:expr $(,)?) => {
        $crate::pass::elaborate::backtrack::Backtrack::Fatal(vec![*$error])
    };
    ($($reports:tt)*) => {
        $crate::pass::elaborate::backtrack::Backtrack::Fatal($($reports)*)
    };
}
pub(super) use fatal;

/// Builds [`Backtrack::Mismatch`] from a report list.
macro_rules! mismatch {
    (error: $error:expr $(,)?) => {
        $crate::pass::elaborate::backtrack::Backtrack::Mismatch(vec![*$error])
    };
    (report: $report:expr $(,)?) => {
        $crate::pass::elaborate::backtrack::Backtrack::Mismatch(vec![$report])
    };
    ($($reports:tt)*) => {
        $crate::pass::elaborate::backtrack::Backtrack::Mismatch($($reports)*)
    };
}
pub(super) use mismatch;

/// Returns early while preserving each failure state.
macro_rules! unwrap {
    ($result:expr) => {
        match $result {
            $crate::pass::elaborate::backtrack::success!(value) => value,
            $crate::pass::elaborate::backtrack::unavailable!(reports) => {
                return $crate::pass::elaborate::backtrack::unavailable!(reports)
            }
            $crate::pass::elaborate::backtrack::fatal!(reports) => {
                return $crate::pass::elaborate::backtrack::fatal!(reports)
            }
            $crate::pass::elaborate::backtrack::mismatch!(reports) => {
                return $crate::pass::elaborate::backtrack::mismatch!(reports)
            }
        }
    };
}
pub(super) use unwrap;

/// Promotes a plain elaboration result to fatal and returns its value.
macro_rules! unwrap_from_result {
    ($result:expr) => {
        match $result {
            Ok(value) => value,
            Err(error) => return $crate::pass::elaborate::backtrack::fatal!(vec![*error]),
        }
    };
}
pub(super) use unwrap_from_result;

// == Propagation and context

impl<T> Backtrack<T> {
    /// Maps a successful value while preserving each failure state.
    pub(super) fn map<U>(self, map: impl FnOnce(T) -> U) -> Backtrack<U> {
        match self {
            success!(value) => success!(map(value)),
            unavailable!(reports) => unavailable!(reports),
            fatal!(reports) => fatal!(reports),
            mismatch!(reports) => mismatch!(reports),
        }
    }

    /// Marks an unavailable child as a mismatch of its applicable parent rule.
    pub(super) fn unavailable_as_mismatch(self) -> Self {
        match self {
            unavailable!(reports) => mismatch!(reports),
            result => result,
        }
    }

    /// Promotes either recoverable failure at a non-backtracking boundary.
    pub(super) fn recoverable_as_failure(self) -> Self {
        match self {
            unavailable!(reports) | mismatch!(reports) => fatal!(reports),
            result => result,
        }
    }

    /// Wraps a recoverable failure under operation context.
    ///
    /// Fatal reports pass through unchanged so their direct cause is retained.
    pub(super) fn nest(self, span: Span, message: impl Into<String>) -> Self {
        match self {
            unavailable!(children) => unavailable!(vec![Report::frame(span, message, children)]),
            mismatch!(children) => mismatch!(vec![Report::frame(span, message, children)]),
            result => result,
        }
    }
}

// == Choice

/// Tries the second alternative after either recoverable failure.
///
/// Each candidate starts from the original context;
/// only the successful candidate is committed.
pub(super) fn choose_sequential<T>(
    ctx: &mut Context,
    first: impl FnOnce(&mut Context) -> Backtrack<T>,
    second: impl FnOnce(&mut Context) -> Backtrack<T>,
) -> Backtrack<T> {
    // Run the first alternative on a copy of the context
    let mut ctx_first = ctx.clone();
    let result_first = first(&mut ctx_first);
    let first_unavailable = matches!(&result_first, unavailable!(_));
    match result_first {
        // Commit the context of the successful alternative
        success!(value) => {
            *ctx = ctx_first;
            success!(value)
        }
        // Stop without committing the failed candidate
        fatal!(reports) => fatal!(reports),
        unavailable!(reports) | mismatch!(reports) => {
            // Retry the second alternative from the original context
            let mut ctx_second = ctx.clone();
            match second(&mut ctx_second) {
                success!(value) => {
                    *ctx = ctx_second;
                    success!(value)
                }
                fatal!(reports) => fatal!(reports),
                unavailable!(mut reports_second) => {
                    let mut reports = reports;
                    reports.append(&mut reports_second);
                    if first_unavailable { unavailable!(reports) } else { mismatch!(reports) }
                }
                mismatch!(mut reports_second) => {
                    let mut reports = reports;
                    reports.append(&mut reports_second);
                    mismatch!(reports)
                }
            }
        }
    }
}

// == Finishing

/// Converts a completed backtrack into a plain elaboration result.
pub(super) fn finish<T>(result: Backtrack<T>) -> Result<T, ElabError> {
    match result {
        success!(value) => Ok(value),
        unavailable!(reports) | mismatch!(reports) | fatal!(reports) => {
            Err(finish_reports(reports))
        }
    }
}

/// Preserves one report unchanged and groups multiple alternative reports.
fn finish_reports(mut reports: Vec<Report>) -> ElabError {
    match reports.len() {
        0 => error::typ::elaboration_alternative_missing(),
        1 => Box::new(reports.pop().expect("one report remains")),
        _ => {
            let span = reports.iter().find_map(report_span).unwrap_or_default();
            let report = Report::frame(span, "elaboration alternatives failed", reports);
            Box::new(report)
        }
    }
}

/// Finds the first located root span without inspecting report presentation.
fn report_span(report: &Report) -> Option<Span> {
    let span = match &report.kind {
        ReportKind::Frame { span, .. } => span,
        ReportKind::Cause(diagnostic) => diagnostic
            .labels
            .iter()
            .find(|label| label.style == LabelStyle::Primary)
            .map(|label| &label.span)?,
    };
    (*span != Span::default()).then_some(*span)
}
