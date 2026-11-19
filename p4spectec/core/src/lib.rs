//! Specification processing and P4 execution for SpecTec
//!
//! `parse` reads source files into EL;
//! `elab`, `algo`, `structure`, and `prosify` transform them through `pass`.
//! `specdoc` renders specifications and expands skeleton documents.
//! `runner_spec_with_warnings` prepares AL, SL, or PL for execution;
//! and `run` evaluates a P4 program with the caller's externs.
//! `coverage` measures SL instruction coverage over programs.
//! Callers render returned diagnostics and accumulated warnings.

// == Shared foundations

pub mod diagnostic;
pub mod lang;
pub mod runtime;
pub mod util;

// == Specification processing

pub mod frontend;
pub mod pass;
pub mod specdoc;

// == Execution

pub mod coverage;
pub mod interface;
pub mod interp;
pub mod runner;

// == Parsing and transformation APIs

pub use frontend::parse::parse_files as parse;
pub use pass::{
    Error, SpecLang, algo, algo_with_warnings, elab, elab_with_warnings, prosify,
    prosify_with_warnings, runner_spec_with_warnings, specdoc_spec_with_warnings, structure,
    structure_with_warnings,
};

// == Document generation APIs

pub use specdoc::splicer::splice_files_with_warnings;

// == Execution APIs

pub use runner::{RunError, run};
