//! Specification processing and P4 execution for SpecTec
//!
//! `parse` reads source files into EL;
//! `elab`, `algo`, `structure`, and `prosify` transform them through `pass`.
//! `specdoc` renders specifications and expands skeleton documents.
//! `runner` executes AL, SL, or PL against P4 programs;
//! `sim_plugin` provides native architectures for STF execution.
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

pub mod interface;
pub mod interp;
pub mod runner;
#[path = "sim-plugin/mod.rs"]
pub mod sim_plugin;
pub mod stf;

// == Parsing and transformation APIs

pub use frontend::parse::parse_files as parse;
pub use pass::{
    Error, algo, algo_with_warnings, elab, elab_with_warnings, prosify, prosify_with_warnings,
    structure, structure_with_warnings,
};
