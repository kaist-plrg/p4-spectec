//! STF syntax: its model, lexer, parser, and printer
//!
//! `ast` has the statements and `name` their qualified names;
//! `lexer` tokenizes a source, `parser` (generated from `parser.lalrpop`)
//! turns the tokens into statements, and `parse` reads a file through both,
//! reporting failures as `error` diagnostics;
//! `print` writes the statements back as text.

mod name;

pub(crate) mod ast;
pub(crate) mod error;
pub(crate) mod lexer;
pub(crate) mod parse;
pub(crate) mod print;

#[allow(clippy::extra_unused_lifetimes, clippy::type_complexity)]
pub(crate) mod parser {
    include!(concat!(env!("OUT_DIR"), "/stf/syntax/parser.rs"));
}
