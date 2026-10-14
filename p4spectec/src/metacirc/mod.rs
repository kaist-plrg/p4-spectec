//! Configured AL interpreter towers over AL or P4 inputs
//!
//! Each runner is prepared in the arena used by all of its calls.
//! Meta externs decode one level of arguments and reenter the next runner.
//! The outer interpreter may execute AL or SL; every represented script is AL.

mod build;
mod codec;
pub mod config;
mod error;
mod externs;
mod patch;
mod tower;

use crate::{lang::data::arena::Arena, runner};
pub use config::{ExecutionMode, TowerConfig};
use std::path::PathBuf;

pub use error::Result;

/// Executes a program through the configured interpreter tower.
pub fn run(
    config: &TowerConfig,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    options: runner::Config,
    cache_codec: bool,
) -> Result<()> {
    if paths_program.is_empty() {
        return Err(error::input_invalid("a tower requires at least one program path"));
    }
    // Keep every prepared layer and encoded input in the same arena
    let mut arena = Arena::new();
    let specs = build::Tower::new(config)?;
    let (mut tower, value_program) = build::tower(
        &mut arena,
        &specs,
        config.mode,
        includes,
        paths_program,
        options,
        cache_codec,
    )?;
    // Only the native base is entered from the command boundary
    tower
        .eval_program(&mut arena, specs.relation(), value_program)
        .result
        .map_err(|error| error.into_report())?;
    Ok(())
}
