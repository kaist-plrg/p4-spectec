//! Structured language
//!
//! SL is AL after structuring:
//! rules and clauses become blocks of instructions with explicit control flow
//! (if, hold, case, let, rule call, result, return),
//! so a definition reads top to bottom like pseudocode.
//! `ast` is the model, `eq`/`free`/`print` the usual traversals.

pub mod ast;
pub mod eq;
pub mod free;
pub mod print;
