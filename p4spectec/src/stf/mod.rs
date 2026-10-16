//! STF packet-test language frontend and utilities

mod name;

pub mod ast;
pub mod error;
pub mod lexer;
pub mod r#match;
pub mod parse;
pub mod print;
pub mod transform;

#[allow(clippy::extra_unused_lifetimes, clippy::type_complexity)]
pub(crate) mod parser {
    include!(concat!(env!("OUT_DIR"), "/stf/parser.rs"));
}
