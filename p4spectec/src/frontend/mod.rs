//! SpecTec source frontend
//!
//! `lexer` turns source text into tokens, `tokens` adapts them for LALRPOP,
//! `ctx` carries the parser state contextual tokenization reads,
//! and `parse` is the entry point from files to an EL `Spec`.
//! The grammar itself is generated into `parser` at build time.

pub mod error;
pub mod lexer;
pub mod parse;

mod ctx;
mod tokens;

/// The LALRPOP-generated parser for `frontend/parser.lalrpop`.
#[allow(clippy::extra_unused_lifetimes, clippy::let_unit_value, clippy::type_complexity)]
pub(crate) mod parser {
    include!(concat!(env!("OUT_DIR"), "/frontend/parser.rs"));
}
