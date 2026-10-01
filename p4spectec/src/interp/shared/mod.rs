//! Shared interpreter environments, failures, state, and evaluation
//!
//! `prepare` resolves identifiers to frame slots once per definition;
//! `eval` then evaluates prepared syntax against a `context` implementation,
//! returning `backtrack` results whose failures carry `error` traces;
//! `cache` memoizes pure calls.

pub mod backtrack;
pub mod cache;
pub mod context;
pub mod error;
pub mod eval;
pub mod prepare;
pub mod util;
