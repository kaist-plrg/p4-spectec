//! Specification interpreters
//!
//! `al` executes algorithmic definitions by backtracking over rule paths;
//! `sl` executes structured blocks with explicit fallthrough;
//! `shared` holds the evaluation both reuse:
//! expressions, assignment, iteration, value operations, contexts, and errors.

pub mod al;
pub mod pl;
pub mod shared;
pub mod sl;
