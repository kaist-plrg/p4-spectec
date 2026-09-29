//! Skeleton marker replacement for specification documents
//!
//! `splice_strings` replaces markers using borrowed EL and PL specifications.
//! `splice_files` renders the complete input batch before staging file writes.
//! Warnings are emitted immediately to stderr.
//! Parser failures retain skeleton positions and LaTeX failures retain EL spans.

pub mod error;
pub mod parser;
pub mod source;

pub use error::Error;

mod anchor;
mod config;
mod driver;
mod file;
mod splicer;
mod splicers;

pub use driver::{splice_files, splice_strings};
