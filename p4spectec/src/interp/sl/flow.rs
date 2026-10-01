//! SL continuations, returns, relation results, and tail calls
//!
//! `Flow` is what evaluating an instruction yields:
//! `Cont` fell through, carrying the failures collected so far;
//! `Return` and `Result` finish a function or relation;
//! `TailFunc` and `TailRel` ask the invoker to call again in place.
//! `choose_sequential` takes the first non-continuing instruction;
//! `choose_deterministic` runs all and rejects two that terminate.

use crate::lang::{
    common::source::{Phrase, Span},
    data::value::Value,
};

use crate::diagnostic::{Diagnostic, Label, Report};

use crate::runtime::envs::interp::sl::ast_prepared as ast;

use crate::interp::shared::{
    backtrack::{Backtrack, fatal, ok, unmatch, unwrap},
    error,
};

/// The outcome of evaluating an instruction or block.
#[derive(Debug)]
pub enum Flow {
    /// Fell through, with the failures met so far.
    Cont(Vec<Report>),
    /// A function body returned a value.
    Return(Phrase<Value>),
    /// A relation body produced its outputs.
    Result(Phrase<Vec<Value>>),
    /// A function call to make in place of the current one.
    TailFunc(Phrase<(ast::Id, Vec<ast::Typ>, Vec<Value>)>),
    /// A relation call to make in place of the current one.
    TailRel(Phrase<(ast::Id, Vec<Value>)>),
}

impl Flow {
    /// Returns the instruction location for a terminating flow.
    fn span(&self) -> &Span {
        match self {
            Self::Return(value) => &value.span,
            Self::Result(values) => &values.span,
            Self::TailFunc(call) => &call.span,
            Self::TailRel(call) => &call.span,
            Self::Cont(_) => unreachable!("continuations have no terminal instruction"),
        }
    }

    // = Continuation

    /// A continuation carrying one premise failure.
    pub(crate) fn cont(span: Span, error: Diagnostic) -> Self {
        let diagnostic = error.with_label(Label::primary(&span, ""));
        Self::Cont(vec![Report::from(diagnostic)])
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
fn retain_deepest_errors(errors: &mut Vec<Report>, errors_post: Vec<Report>) {
    if errors_post.iter().map(Report::depth_max).max().unwrap_or(0)
        >= errors.iter().map(Report::depth_max).max().unwrap_or(0)
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
fn combine_deterministic(flow: Flow, flow_post: Flow) -> Backtrack<Flow> {
    let flow = match (flow, flow_post) {
        // Both continue: merge the failures
        (Flow::Cont(mut errors), Flow::Cont(errors_post)) => {
            errors.extend(errors_post);
            Flow::Cont(errors)
        }
        // One terminated: keep it
        (Flow::Cont(_), flow) | (flow, Flow::Cont(_)) => flow,
        // Structuring preserves the conclusion kind of each callable
        (Flow::Return(_) | Flow::TailFunc(..), Flow::Result(_) | Flow::TailRel(..))
        | (Flow::Result(_) | Flow::TailRel(..), Flow::Return(_) | Flow::TailFunc(..)) => {
            unreachable!("function and relation conclusions cannot mix")
        }
        // Two conclusions from the same callable are nondeterministic
        (flow, flow_post) => {
            return fatal!(
                flow_post.span().clone(),
                error::call::instruction_nondeterministic(flow.span()),
            );
        }
    };
    ok!(flow)
}

/// Runs every instruction and merges the flows; mismatches are skipped.
pub(crate) fn choose_deterministic<C>(
    candidates: impl IntoIterator<Item = C>,
    mut evaluate: impl FnMut(C) -> Backtrack<Flow>,
) -> Backtrack<Flow> {
    // Start from an empty continuation
    let mut flow = Flow::Cont(vec![]);
    for candidate in candidates {
        let flow_post = match evaluate(candidate) {
            // A mismatching instruction contributes nothing
            unmatch!(_) => continue,
            result => unwrap!(result),
        };
        // Merge, rejecting a second terminating flow
        flow = unwrap!(combine_deterministic(flow, flow_post));
    }
    ok!(flow)
}
