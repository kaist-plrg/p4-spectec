//! Algorithmic language
//!
//! AL is IL after binding analysis:
//! types, expressions, and definitions are the IL ones,
//! but rules are split into a shared match (inputs and shared premises)
//! and paths (remaining premises and outputs), and `let` premises replace
//! the equations that bind variables.
//! `ast` is the model, `eq`/`free`/`print` the usual traversals,
//! `fresh` and `var` mint variable expressions, `partial` asks whether
//! evaluation can fail.

pub mod ast;
pub mod eq;
pub mod free;
pub mod fresh;
pub mod print;
pub mod var;
