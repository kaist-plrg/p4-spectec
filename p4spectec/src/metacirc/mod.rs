//! Configured AL interpreter towers over AL or P4 inputs
//!
//! Each runner is prepared in the arena used by all of its calls.
//! Meta externs decode one level of arguments and reenter the next runner.
//! The outer interpreter may execute AL or SL; every represented script is AL.

mod build;
mod codec;
pub mod config;
mod externs;
mod patch;
mod tower;

use crate::{
    diagnostic::{Diagnostic, Report, Severity},
    lang::data::arena::Arena,
    runner,
};
pub use config::{Config, Mode};
use std::path::PathBuf;

/// A metacircular construction or execution diagnostic.
pub type Result<T> = std::result::Result<T, Box<Report>>;

/// Creates a diagnostic for invalid metacircular configuration or input.
fn error(message: impl Into<String>) -> Box<Report> {
    Box::new(
        Diagnostic::new(
            "metacirc",
            Severity::Error,
            Some("metacirc/config-invalid".into()),
            message,
            vec![],
            vec![],
        )
        .into(),
    )
}

/// Executes a program through the configured interpreter tower.
pub fn run(
    config: &Config,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    options: runner::Config,
) -> Result<()> {
    if paths_program.is_empty() {
        return Err(error("a tower requires at least one program path"));
    }
    // Keep every prepared layer and encoded input in the same arena
    let mut arena = Arena::new();
    let specs = build::TowerSpecs::new(config)?;
    let (mut boot, value_program) =
        build::tower(&mut arena, &specs, config.mode, includes, paths_program, options)?;
    // Only the native boot is entered from the command boundary
    boot.eval_program(&mut arena, specs.relation(), value_program)
        .result
        .map_err(|error| error.into_report())?;
    Ok(())
}
