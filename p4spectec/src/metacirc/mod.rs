//! Standalone AL interpretation and runtime towers over P4
//!
//! Each runner is prepared in the arena used by all of its calls.
//! Meta externs decode one level of arguments and reenter the next runner.
//! The outer interpreter may execute AL or SL; every represented script is AL.
//! `run_standalone` executes an AL script with builtin host calls,
//! while `run_tower` patches entries from the P4 program toward the outer layer.

mod build;
mod codec;
pub mod config;
mod externs;
mod patch;
mod tower;

use crate::{
    diagnostic::{Diagnostic, Report, Severity},
    interface::al::encode::Encode,
    lang::data::arena::Arena,
    runner,
};
pub use config::{Config, Mode};
use std::path::{Path, PathBuf};

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

/// Interprets an AL script whose host operations are specification builtins.
pub fn run_standalone(
    mode: Mode,
    paths: &[PathBuf],
    relation: &str,
    paths_program: &[PathBuf],
    options: runner::Config,
) -> Result<()> {
    let mut arena = Arena::new();
    // Prepare the target from its own definitions before encoding its script
    let spec_target_al = crate::algo(paths_program)?;
    let value_program = spec_target_al
        .encode(&mut arena)
        .map_err(|error| error.into_report())?;
    let mut runner = build::standalone(&mut arena, mode, paths, spec_target_al, options)?;
    // The meta entry supplies the script's execution semantics
    runner
        .eval_program(&mut arena, relation, value_program)
        .result
        .map_err(|error| error.into_report())?;
    Ok(())
}

/// Executes a P4 program through the configured AL tower.
pub fn run_tower(
    config: &Config,
    includes: &[PathBuf],
    path_program: &Path,
    options: runner::Config,
) -> Result<()> {
    config.validate()?;
    let mut arena = Arena::new();
    // Lower once per level; both native preparation and encoding use these ASTs
    let specs_al = config
        .levels
        .iter()
        .map(|level| crate::algo(std::slice::from_ref(&level.specdir)))
        .collect::<Result<Vec<_>>>()?;
    let mut tower = build::tower(&mut arena, config, &specs_al, options)?;
    let mut value_program = tower
        .parse_program(&mut arena, includes, path_program)
        .map_err(|error| error.into_report())?;
    // Wrap the target and then each intermediate script with its own entry
    for (level, spec_al) in config.levels.iter().zip(&specs_al).skip(1).rev() {
        let spec_patched_al = patch::entry(&arena, spec_al, &level.rel, value_program)?;
        value_program = spec_patched_al
            .encode(&mut arena)
            .map_err(|error| error.into_report())?;
    }
    tower
        .eval_program(&mut arena, &config.levels[0].rel, value_program)
        .result
        .map_err(|error| error.into_report())?;
    Ok(())
}
