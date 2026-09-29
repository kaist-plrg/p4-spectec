//! Skeleton marker replacement for specification documents
//!
//! `splice_strings` replaces markers using borrowed EL and PL specifications.
//! `splice_files` renders the complete input batch before staging file writes.
//! The `_with_warnings` entry points retain reports for caller-controlled output;
//! the convenience entry points discard nonfatal warnings.
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

pub use driver::{
    splice_files, splice_files_with_warnings, splice_strings, splice_strings_with_warnings,
};
