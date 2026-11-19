//! STF packet-test language frontend and execution
//!
//! `parse` turns STF source into statements,
//! and `run` executes them against one runner,
//! adding table entries through `table`
//! and matching outputs against `expect` lines through `expect`.

mod name;

pub(crate) mod ast;
pub(crate) mod error;
pub(crate) mod expect;
pub(crate) mod lexer;
pub(crate) mod r#match;
pub(crate) mod parse;
pub(crate) mod print;
pub(crate) mod run;
pub(crate) mod table;
pub(crate) mod transform;

#[allow(clippy::extra_unused_lifetimes, clippy::type_complexity)]
pub(crate) mod parser {
    include!(concat!(env!("OUT_DIR"), "/stf/parser.rs"));
}
