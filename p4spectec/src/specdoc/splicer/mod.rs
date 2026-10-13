//! Skeleton marker replacement for specification documents
//!
//! `splice_strings` replaces markers using borrowed EL and PL specifications.
//! `prepare_splice_files_with_warnings` stages the complete input batch.
//! Callers render its diagnostics before `PreparedSplice::commit` replaces files.
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

pub use driver::{splice_strings, splice_strings_with_warnings};
pub use file::{PreparedSplice, prepare_splice_files_with_warnings, splice_files};
