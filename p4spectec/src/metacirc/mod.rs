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

use crate::{
    interface::{al::encode::Encode, p4::parse::parse_file},
    lang::data::arena::Arena,
    runner,
};
use config::ProgramConfig;
pub use config::{ExecutionMode, TowerConfig};
use std::path::PathBuf;

pub use error::Result;

pub use tower::Tower;

/// Executes a program through the configured interpreter tower.
pub fn run(
    config: &TowerConfig,
    includes: &[PathBuf],
    paths_program: &[PathBuf],
    options: runner::Config,
) -> Result<()> {
    if paths_program.is_empty() {
        return Err(error::input_invalid("a tower requires at least one program path"));
    }
    let target = config.levels.target();
    let mut arena = Arena::new();
    let mut tower;
    let value_program;

    // Prepare the input and host environment by the configured target language
    match &config.program {
        ProgramConfig::Al { spec_al } => {
            // Use the same AL definitions for builtin calls and input encoding
            tower = Tower::build(&mut arena, config, options)?;
            value_program = spec_al
                .encode(&mut arena)
                .map_err(|error| error.into_report())?;
        }
        ProgramConfig::P4 => {
            // P4 inputs contain exactly one translation unit
            let [path] = paths_program else {
                return Err(error::input_invalid("a P4 target requires exactly one program path"));
            };
            tower = Tower::build(&mut arena, config, options)?;
            value_program =
                parse_file(&mut arena, includes, path).map_err(|error| error.into_report())?;
        }
    }
    // Encoding errors propagate before entering the native base
    let value = tower.patch(&mut arena, &target.rel, value_program)?;
    tower
        .eval_program(&mut arena, &target.rel, value)
        .result
        .map_err(|error| error.into_report())?;
    Ok(())
}
