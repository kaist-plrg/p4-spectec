//! SL continuations, returns, relation results, and tail calls
//!
//! `Flow` is what evaluating an instruction yields:
//! `Cont` fell through, carrying the failures collected so far;
//! `Return` and `Result` finish a function or relation;
//! `TailFunc` and `TailRel` ask the invoker to call again in place.
//! `choose_sequential` takes the first non-continuing instruction;
//! `choose_deterministic` runs all and rejects two that terminate.

use crate::interp::shared::error;
use crate::runtime::envs::interp::sl::ast_prepared as ast;
use crate::{
    interp::shared::backtrack::{Backtrack, err, ok, unmatch, unwrap},
    lang::{common::source::Span, data::value::Value},
};

/// The outcome of evaluating an instruction or block.
#[derive(Debug)]
pub enum Flow {
    /// Fell through, with the failures met so far.
    Cont(Vec<crate::diagnostic::Report>),
    /// A function body returned a value.
    Return(Value),
    /// A relation body produced its outputs.
    Result(Vec<Value>),
    /// A function call to make in place of the current one.
    TailFunc(ast::Id, Vec<ast::Typ>, Vec<Value>),
    /// A relation call to make in place of the current one.
    TailRel(ast::Id, Vec<Value>),
}

impl Flow {
    // = Continuation

    /// A continuation carrying one premise failure.
    pub(crate) fn cont(span: Span, error: crate::diagnostic::Diagnostic) -> Self {
        Self::Cont(vec![*error::at(error, span)])
    }

    /// Turns a mismatch into a continuation; errors and flows pass through.
    pub(crate) fn cont_from_unmatch(result: Backtrack<Self>) -> Backtrack<Self> {
        match result {
            unmatch!(errors) => ok!(Self::Cont(errors)),
            result => result,
        }
    }
}

// = Sequential choice

/// Keeps the failure set that got furthest, so the report is the most specific.
fn retain_deepest_errors(
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

/// Tries instructions in order; the last one runs in tail position.
pub(crate) fn choose_sequential<C>(
    mut candidates: impl DoubleEndedIterator<Item = C>,
    mut evaluate: impl FnMut(C, bool) -> Backtrack<Flow>,
) -> Backtrack<Flow> {
    // An empty block continues with no failures
    let Some(candidate_last) = candidates.next_back() else {
        return ok!(Flow::Cont(vec![]));
    };
    // Non-tail instructions run first; the first one that terminates wins
    let mut errors = Vec::new();
    for candidate in candidates {
        match unwrap!(evaluate(candidate, false)) {
            Flow::Cont(errors_post) => retain_deepest_errors(&mut errors, errors_post),
            flow => return ok!(flow),
        }
    }
    // The last instruction gets the tail flag
    match unwrap!(evaluate(candidate_last, true)) {
        Flow::Cont(errors_post) => {
            retain_deepest_errors(&mut errors, errors_post);
            ok!(Flow::Cont(errors))
        }
        flow => ok!(flow),
    }
}

// = Deterministic choice

/// Merges two flows; both terminating is nondeterminism or an invalid mix.
fn combine_deterministic(flow: Flow, flow_post: Flow, span: &Span) -> Backtrack<Flow> {
    let flow = match (flow, flow_post) {
        // Both continue: merge the failures
        (Flow::Cont(mut errors), Flow::Cont(errors_post)) => {
            errors.extend(errors_post);
            Flow::Cont(errors)
        }
        // One terminated: keep it
        (Flow::Cont(_), flow) | (flow, Flow::Cont(_)) => flow,
        // Two of the same kind: nondeterminism
        (Flow::Return(_), Flow::Return(_))
        | (Flow::Result(_), Flow::Result(_))
        | (Flow::TailFunc(..) | Flow::TailRel(..), Flow::TailFunc(..) | Flow::TailRel(..)) => {
            return err!(span.clone(), error::call::instruction_nondeterministic(),);
        }
        // Two of different kinds: an invalid body
        (flow_pre, flow_post) => {
            let message = match (flow_pre, flow_post) {
                (Flow::Result(_), Flow::Return(_)) => "cannot have both result and return",
                (Flow::Result(_), _) => "cannot have both result and tail call",
                (Flow::Return(_), Flow::Result(_)) => "cannot have both return and result",
                (Flow::Return(_), _) => "cannot have both return and tail call",
                (Flow::TailFunc(..), Flow::Result(_)) => "cannot have both tail call and result",
                (Flow::TailFunc(..), _) => "cannot have both tail call and return",
                (Flow::TailRel(..), Flow::Result(_)) => "cannot have both rel tail call and result",
                (Flow::TailRel(..), _) => "cannot have both rel tail call and return",
                (Flow::Cont(_), _) => unreachable!("continuations were combined above"),
            };
            return err!(span.clone(), error::call::flow_invalid(message),);
        }
    };
    ok!(flow)
}

/// Runs every instruction and merges the flows; mismatches are skipped.
pub(crate) fn choose_deterministic<C>(
    candidates: impl IntoIterator<Item = C>,
    mut evaluate: impl FnMut(C) -> Backtrack<Flow>,
    mut span_of: impl FnMut(&C) -> Span,
) -> Backtrack<Flow> {
    // Start from an empty continuation
    let mut flow = Flow::Cont(vec![]);
    for candidate in candidates {
        let span = span_of(&candidate);
        let flow_post = match evaluate(candidate) {
            // A mismatching instruction contributes nothing
            unmatch!(_) => continue,
            result => unwrap!(result),
        };
        // Merge, rejecting a second terminating flow
        flow = unwrap!(combine_deterministic(flow, flow_post, &span));
    }
    ok!(flow)
}
