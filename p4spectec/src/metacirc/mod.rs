//! Configured interpreter towers over AL, IL, or P4 inputs
//!
//! Each runner is prepared in the arena used by all of its calls.
//! Only the base runs natively; meta externs decode one level of arguments
//! and relay them to the host of the next level.
//! The native base may execute AL or SL.

mod build;
pub mod config;
mod error;
mod host;
mod patch;
mod tower;

use crate::{
    interface::{self, p4::parse::parse_file},
    lang::data::arena::Arena,
    runner::{self, Interface},
};
use config::Lang;
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
    let lang_top = config.top_lang();
    // P4 inputs contain exactly one translation unit
    if lang_top == Lang::P4 && paths_program.len() != 1 {
        return Err(error::input_invalid("a P4 top level requires exactly one program path"));
    }
    let mut arena = Arena::new();
    let mut tower = Tower::build(&mut arena, config, options)?;
    // Parse the input in the language the top level accepts
    let value_program = match lang_top {
        Lang::Al | Lang::Il => interface::meta(lang_top)
            .parse_program(&mut arena, includes, paths_program)
            .map_err(|error| error.into_report())?,
        Lang::P4 => parse_file(&mut arena, includes, &paths_program[0])
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
