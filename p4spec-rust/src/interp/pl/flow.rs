//! PL block continuations, function returns, and relation results
//!
//! PL concludes directly; invocations evaluate their callable bodies.
//! `choose_sequential` keeps the first conclusion and the deepest failures;
//! `choose_deterministic` combines outcomes and rejects multiple conclusions.
//! Evaluators supply candidates and manage their local bindings.

use crate::interp::shared::error;
use crate::{
    interp::shared::backtrack::{Backtrack, err, ok, unmatch, unwrap},
    lang::{common::source::Span, data::value::Value},
};

/// Records whether an instruction continues or concludes its callable.
#[derive(Debug)]
pub enum Flow {
    /// Fell through, with the failures met so far.
    Cont(Vec<crate::diagnostic::Report>),
    /// A function body returned a value.
    Return(Value),
    /// A relation body produced its outputs.
    Result(Vec<Value>),
}

impl Flow {
    // = Continuation

    /// Creates a recoverable continuation with its premise diagnostic.
    pub(crate) fn cont(span: Span, error: crate::diagnostic::Diagnostic) -> Self {
        Self::Cont(vec![*error::at(error, span)])
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
pub(super) fn retain_deepest_errors(
    errors: &mut Vec<crate::diagnostic::Report>,
    errors_post: Vec<crate::diagnostic::Report>,
) {
    if errors_post
        .iter()
        .map(error::trace::depth)
        .max()
        .unwrap_or(0)
        >= errors.iter().map(error::trace::depth).max().unwrap_or(0)
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
fn combine_deterministic(flow: Flow, flow_post: Flow, span: &Span) -> Backtrack<Flow> {
    let flow = match (flow, flow_post) {
        // Both continue: merge the failures
        (Flow::Cont(mut errors), Flow::Cont(errors_post)) => {
            errors.extend(errors_post);
            Flow::Cont(errors)
        }
        // One concluded: keep it
        (Flow::Cont(_), flow) | (flow, Flow::Cont(_)) => flow,
        // Two of the same kind: nondeterminism
        (Flow::Return(_), Flow::Return(_)) | (Flow::Result(_), Flow::Result(_)) => {
            return err!(span.clone(), error::call::instruction_nondeterministic());
        }
        // Different conclusion kinds cannot belong to the same callable
        _ => {
            return err!(span.clone(), error::call::flow_invalid("incompatible PL conclusions"));
        }
    };
    ok!(flow)
}

/// Evaluates candidates and merges their flows, skipping mismatches.
pub(crate) fn choose_deterministic<C>(
    candidates: impl IntoIterator<Item = C>,
    mut evaluate: impl FnMut(C) -> Backtrack<Flow>,
    mut span_of: impl FnMut(&C) -> Span,
) -> Backtrack<Flow> {
    // Start from an empty continuation
    let mut flow = Flow::Cont(vec![]);
    // Every candidate must be checked for a second conclusion
    for candidate in candidates {
        let span = span_of(&candidate);
        let flow_post = match evaluate(candidate) {
            // A mismatching candidate contributes nothing
            unmatch!(_) => continue,
            // Fatal failures abort selection
            result => unwrap!(result),
        };
        // Reject multiple conclusions at the conflicting candidate
        flow = unwrap!(combine_deterministic(flow, flow_post, &span));
    }
    ok!(flow)
}
