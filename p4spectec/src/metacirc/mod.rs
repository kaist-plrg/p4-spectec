//! Configured AL interpreter towers over AL or P4 inputs
//!
//! Each runner is prepared in the arena used by all of its calls.
//! Meta externs decode one level of arguments and reenter the next runner.
//! The outer interpreter may execute AL or SL; every represented script is AL.
//! A two-level SL tower over P4 instead runs as one runner backend:
//! `meta_spec` selects it, and `runner::build_meta` serves `run` and simulation.

mod build;
mod codec;
pub mod config;
mod error;
mod externs;
pub(crate) mod interp;
mod patch;
mod tower;

use crate::{
    interface::{al::encode::Encode, p4::parse::parse_file},
    lang::data::arena::Arena,
    pass::structure,
    runner,
};
pub use config::{ExecutionMode, TowerConfig};
use config::{Lang, LevelConfigs, ProgramConfig};
use std::path::PathBuf;

pub use error::Result;

pub use tower::Tower;

/// Selects the meta runner backend for a two-level SL tower over P4.
///
/// Returns `None` for towers that run through `Tower`:
/// AL execution mode, an AL target, or intermediate levels.
pub fn meta_spec(config: &TowerConfig) -> Result<Option<runner::Spec>> {
    let LevelConfigs::Nested { base, interms, target } = &config.levels else {
        return Ok(None);
    };
    if !interms.is_empty() || target.lang != Lang::P4 || !matches!(config.mode, ExecutionMode::Sl) {
        return Ok(None);
    }
    // Calls enter Call_rel directly, so a base entry would go unused
    if base.rel.is_some() {
        return Err(error::config_invalid(
            "level 0: the meta backend does not use an entry relation; remove \"rel\"",
        ));
    }
    let base_sl = structure::convert(crate::algo(std::slice::from_ref(&base.specdir))?, true)?;
    let target_al = crate::algo(std::slice::from_ref(&target.specdir))?;
    Ok(Some(runner::Spec::Meta { base: base_sl, target: target_al }))
}

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
    let relation = config.target_rel();
    // A two-level SL tower over P4 runs as a single meta runner
    if let Some(spec) = meta_spec(config)? {
        let [path_program] = paths_program else {
            return Err(error::input_invalid("a P4 target requires exactly one program path"));
        };
        return runner::run(spec, options, relation, includes, path_program).map_err(|error| {
            match error {
                runner::RunError::Build(report) => report,
                runner::RunError::Parse(error) => error.into_report(),
                runner::RunError::Eval(error) => error.into_report(),
            }
        });
    }
    let mut arena = Arena::new();
    // Prepare the input environment by the configured target language
    let program = match config.target_lang() {
        // AL builtins must resolve against the actual input definitions
        Lang::Al => ProgramConfig::Al { spec_al: crate::algo(paths_program)? },
        // P4 inputs contain exactly one translation unit
        Lang::P4 if paths_program.len() == 1 => ProgramConfig::P4,
        Lang::P4 => {
            return Err(error::input_invalid("a P4 target requires exactly one program path"));
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
    let value = tower.patch(&mut arena, relation, value_program)?;
    tower
        .eval_program(&mut arena, relation, value)
        .result
        .map_err(|error| error.into_report())?;
    Ok(())
}
