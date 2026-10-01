//! P4 source frontend
//!
//! `preprocessor` runs `cc -E`, `lexer` tokenizes with context-sensitive names,
//! `parse` drives the LALRPOP grammar into a value tree,
//! `context`, `declare`, and `extract` keep the name-resolution state
//! the grammar needs,
//! `binary` folds operator precedence, and `unparse` renders values back to P4.

pub mod context;
pub mod error;
pub mod lexer;
pub mod parse;
pub mod preprocessor;
pub mod unparse;

mod binary;
mod declare;
mod extract;

/// The LALRPOP-generated parser for `interface/p4/parser.lalrpop`.
#[allow(clippy::extra_unused_lifetimes, clippy::let_unit_value, clippy::type_complexity)]
pub(crate) mod parser {
    include!(concat!(env!("OUT_DIR"), "/interface/p4/parser.rs"));
}
