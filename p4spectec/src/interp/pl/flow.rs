//! PL block continuations, function returns, and relation results
//!
//! PL concludes directly; invocations evaluate their callable bodies.
//! `choose_sequential` keeps the first conclusion and the deepest failures;
//! `choose_deterministic` combines outcomes and rejects multiple conclusions.
//! Evaluators supply candidates and manage their local bindings.

use crate::lang::{
    common::source::{Phrase, Span},
    data::value::flat::Value,
};

use crate::diagnostic::{Diagnostic, Label, Report};

use crate::interp::shared::{
    backtrack::{Backtrack, fatal, ok, unmatch, unwrap},
    error,
};

/// Records whether an instruction continues or concludes its callable.
#[derive(Debug)]
pub enum Flow {
    /// Fell through, with the failures met so far.
    Cont(Vec<Report>),
    /// A function body returned a value.
    Return(Phrase<Value>),
    /// A relation body produced its outputs.
    Result(Phrase<Vec<Value>>),
}

impl Flow {
    // = Continuation

    /// Creates a recoverable continuation with its premise diagnostic.
    pub(crate) fn cont(span: Span, error: Diagnostic) -> Self {
        let diagnostic = error.with_label(Label::primary(&span, ""));
        Self::Cont(vec![Report::from(diagnostic)])
    }

    /// Turns a mismatch into a continuation; errors and flows pass through.
    pub(crate) fn cont_from_unmatch(result: Backtrack<Self>) -> Backtrack<Self> {
        match result {
            // Alternative selection can recover from a mismatching block
            unmatch!(errors) => ok!(Self::Cont(errors)),
            // Successful flows and fatal errors retain their meaning
            result => result,
        }
    }
}

// = Sequential choice

/// Keeps the most deeply nested failure, preferring the later one on ties.
pub(super) fn retain_deepest_errors(errors: &mut Vec<Report>, errors_post: Vec<Report>) {
    if errors_post.iter().map(Report::depth_max).max().unwrap_or(0)
        >= errors.iter().map(Report::depth_max).max().unwrap_or(0)
    {
        *errors = errors_post;
    }
}

/// Selects the first conclusion, propagating mismatches and fatal errors.
pub(crate) fn choose_sequential<C>(
    candidates: impl IntoIterator<Item = C>,
    mut evaluate: impl FnMut(C) -> Backtrack<Flow>,
) -> Backtrack<Flow> {
    // An empty candidate sequence continues without failures
    let mut errors = Vec::new();
    // Evaluate in order until a candidate concludes or aborts
    for candidate in candidates {
        match unwrap!(evaluate(candidate)) {
            // Continuing candidates contribute their most specific failures
            Flow::Cont(errors_post) => retain_deepest_errors(&mut errors, errors_post),
            // The first conclusion ends sequential choice
            flow => return ok!(flow),
        }
    }
    ok!(Flow::Cont(errors))
}

// = Deterministic choice

/// Merges alternative outcomes and rejects multiple conclusions.
fn combine_deterministic(flow: Flow, flow_post: Flow) -> Backtrack<Flow> {
    let flow = match (flow, flow_post) {
        // Both continue: merge the failures
        (Flow::Cont(mut errors), Flow::Cont(errors_post)) => {
            errors.extend(errors_post);
            Flow::Cont(errors)
        }
        // One concluded: keep it
        (Flow::Cont(_), flow) | (flow, Flow::Cont(_)) => flow,
        // Two of the same kind: nondeterminism
        (Flow::Return(value), Flow::Return(value_post)) => {
            return fatal!(value_post.span, error::call::instruction_nondeterministic(&value.span));
        }
        // Empty output lists still carry the instruction that concluded
        (Flow::Result(values), Flow::Result(values_post)) => {
            return fatal!(
                values_post.span,
                error::call::instruction_nondeterministic(&values.span)
            );
        }
        // Different conclusion kinds cannot belong to the same callable
        _ => unreachable!("function and relation conclusions cannot mix"),
    };
    ok!(flow)
}

/// Evaluates candidates and merges their flows, skipping mismatches.
pub(crate) fn choose_deterministic<C>(
    candidates: impl IntoIterator<Item = C>,
    mut evaluate: impl FnMut(C) -> Backtrack<Flow>,
) -> Backtrack<Flow> {
    // Start from an empty continuation
    let mut flow = Flow::Cont(vec![]);
    // Every candidate must be checked for a second conclusion
    for candidate in candidates {
        let flow_post = match evaluate(candidate) {
            // A mismatching candidate contributes nothing
            unmatch!(_) => continue,
            // Fatal failures abort selection
            result => unwrap!(result),
        };
        // Reject multiple conclusions at the conflicting candidate
        flow = unwrap!(combine_deterministic(flow, flow_post));
    }
    ok!(flow)
}
