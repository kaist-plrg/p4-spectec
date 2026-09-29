//! Ordered AL candidate selection and deterministic overlap checks
//!
//! `choose_sequential` returns the first candidate that matches;
//! `choose_deterministic` evaluates every candidate and rejects a second match.
//! Both stop at the first fatal error and,
//! when nothing matches, return the mismatches of every candidate tried.

use crate::interp::shared::{
    backtrack::{Backtrack, err, ok, unmatch},
    error::Error,
};

// = Sequential choice

/// Returns the first matching candidate, collecting the mismatches otherwise.
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
            err!(errors) => return err!(errors),
            // A mismatch is recorded and the next candidate tried
            unmatch!(mut candidate_errors) => errors.append(&mut candidate_errors),
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
                    return err!(nondet(first, candidate));
                }
                success = Some((candidate, value));
                errors.clear();
            }
            err!(errors) => return err!(errors),
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
