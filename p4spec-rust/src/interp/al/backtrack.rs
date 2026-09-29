//! Ordered AL candidate selection and deterministic overlap checks
//!
//! `choose_sequential` returns the first match or the deepest failure set;
//! `choose_deterministic` evaluates every candidate and rejects a second match.
//! Both stop at the first fatal error.
//! Deterministic choice retains every mismatch when nothing matches.

use crate::diagnostic::Report;
use crate::interp::shared::{
    backtrack::{Backtrack, fatal, ok, unmatch},
    error::{self, Error},
};

// = Sequential choice

/// Keeps the most deeply nested failure set, preferring the later one on ties.
fn retain_deepest_errors(errors: &mut Vec<Report>, errors_post: Vec<Report>) {
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

/// Returns the first matching candidate or the deepest failure set.
pub fn choose_sequential<C, T>(
    candidates: impl IntoIterator<Item = C>,
    mut evaluate: impl FnMut(&C) -> Backtrack<T>,
) -> Backtrack<T> {
    let mut errors = Vec::new();
    for candidate in candidates {
        match evaluate(&candidate) {
            // The first match wins
            ok!(value) => return ok!(value),
            // A fatal error stops the search
            fatal!(errors) => return fatal!(errors),
            // Retain the deepest failures before trying the next candidate
            unmatch!(errors_post) => retain_deepest_errors(&mut errors, errors_post),
        }
    }
    unmatch!(errors)
}

// = Deterministic choice

/// Evaluates every candidate; a second match is reported through `nondet`.
pub fn choose_deterministic<C, T>(
    candidates: impl IntoIterator<Item = C>,
    mut evaluate: impl FnMut(&C) -> Backtrack<T>,
    nondet: impl FnOnce(C, C) -> Error,
) -> Backtrack<T> {
    // Remember the first match and the mismatches seen before it
    let mut success = None;
    let mut errors = Vec::new();
    for candidate in candidates {
        match evaluate(&candidate) {
            ok!(value) => {
                // A second match is nondeterminism
                if let Some((first, _)) = success {
                    return fatal!(nondet(first, candidate));
                }
                success = Some((candidate, value));
                errors.clear();
            }
            fatal!(errors) => return fatal!(errors),
            // Mismatches only matter while nothing has matched
            unmatch!(mut candidate_errors) => {
                if success.is_none() {
                    errors.append(&mut candidate_errors);
                }
            }
        }
    }
    // One match is the answer; none is a mismatch
    match success {
        Some((_, value)) => ok!(value),
        None => unmatch!(errors),
    }
}
