//! PL block continuations, function returns, relation results, and tail calls
//!
//! PL concludes directly, or under `tco` with a call for the invoker to make.
//! `choose_sequential` keeps the first conclusion and the deepest failures;
//! `choose_deterministic` combines outcomes and rejects multiple conclusions.
//! Evaluators supply candidates and manage their local bindings.

use crate::lang::{
    common::source::{Phrase, Span},
    data::value::flat::Value,
};

use crate::lang::pl::prepared::{Id, Typ};

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
    /// A function call to make in place of the current one.
    TailFunc(Phrase<(Id, Vec<Typ>, Vec<Value>)>),
    /// A relation call to make in place of the current one.
    TailRel(Phrase<(Id, Vec<Value>)>),
}

impl Flow {
    /// Returns the instruction location of a conclusion.
    fn span(&self) -> Span {
        match self {
            Self::Return(value) => value.span,
            Self::Result(values) => values.span,
            Self::TailFunc(call) => call.span,
            Self::TailRel(call) => call.span,
            Self::Cont(_) => unreachable!("continuations have no concluding instruction"),
        }
    }

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
        // Different conclusion kinds cannot belong to the same callable
        (Flow::Return(_) | Flow::TailFunc(_), Flow::Result(_) | Flow::TailRel(_))
        | (Flow::Result(_) | Flow::TailRel(_), Flow::Return(_) | Flow::TailFunc(_)) => {
            unreachable!("function and relation conclusions cannot mix")
        }
        // Two conclusions of the same callable: nondeterminism
        (flow, flow_post) => {
            return fatal!(
                flow_post.span(),
                error::call::instruction_nondeterministic(&flow.span())
            );
        }
    };
    ok!(flow)
}

/// Evaluates candidates and merges their flows.
pub(crate) fn choose_deterministic<C>(
    candidates: impl IntoIterator<Item = C>,
    mut evaluate: impl FnMut(C) -> Backtrack<Flow>,
) -> Backtrack<Flow> {
    // Start from an empty continuation
    let mut flow = Flow::Cont(vec![]);
    // Every candidate must be checked for a second conclusion
    for candidate in candidates {
        let flow_post = unwrap!(evaluate(candidate));
        // Reject multiple conclusions at the conflicting candidate
        flow = unwrap!(combine_deterministic(flow, flow_post));
    }
    ok!(flow)
}
