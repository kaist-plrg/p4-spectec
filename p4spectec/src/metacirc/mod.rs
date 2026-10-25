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
    interface::{meta::shared::encode::Encode, p4::parse::parse_file},
    lang::data::arena::Arena,
    runner,
};
pub use config::{ExecutionMode, TowerConfig};
use config::{Lang, ProgramConfig};
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
    let mut arena = Arena::new();
    // Prepare the input environment by the top level's configured language
    let program = match config.top_lang() {
        // AL builtins must resolve against the actual input definitions
        Lang::Al => ProgramConfig::Al { spec_al: crate::algo(paths_program)? },
        // P4 inputs contain exactly one translation unit
        Lang::P4 if paths_program.len() == 1 => ProgramConfig::P4,
        Lang::P4 => {
            return Err(error::input_invalid("a P4 top level requires exactly one program path"));
        }
    };
    let mut tower = Tower::build(&mut arena, config, &program, options)?;
    let value_program = match &program {
        // Use the same AL definitions for builtin calls and input encoding
        ProgramConfig::Al { spec_al } => spec_al
            .encode(&mut arena)
            .map_err(|error| error.into_report())?,
        ProgramConfig::P4 => parse_file(&mut arena, includes, &paths_program[0])
            .map_err(|error| error.into_report())?,
    };
    // Encoding errors propagate before entering the native base
    let value = tower.patch(&mut arena, value_program)?;
    tower
        .eval_program(&mut arena, value)
        .result
        .map_err(|error| error.into_report())?;
    Ok(())
}
