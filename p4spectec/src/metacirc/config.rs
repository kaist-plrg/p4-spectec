//! JSON descriptions of AL interpreter towers
//!
//! Levels run from the native base toward a top level consuming AL or P4.
//! The native backend is AL or SL; all meta interfaces represent AL scripts.
//! Specification paths are resolved relative to the JSON file.

use super::{Result, error::config_invalid};
use crate::lang::al::ast as al;
use serde::Deserialize;
use std::{
    fs,
    path::{Path, PathBuf},
};

/// Validated tower roles and native execution mode.
#[derive(Clone, Debug)]
pub struct TowerConfig {
    /// The interpreter executing the base specification.
    pub(super) mode: ExecutionMode,
    /// Nonempty, ordered from base toward top.
    pub(super) levels: Vec<LevelConfig>,
}

/// The native interpreter used for the outermost level.
#[derive(Clone, Copy, Debug, Deserialize, clap::ValueEnum, Default)]
#[serde(rename_all = "lowercase")]
pub enum ExecutionMode {
    /// Execute the algorithmic representation.
    Al,
    /// Execute the structured representation.
    #[default]
    Sl,
}

/// One specification and the relation used to enter it.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LevelConfig {
    /// A specification file or directory.
    pub specdir: PathBuf,
    /// The single-input entry relation.
    pub rel: String,
    /// The language accepted by this level.
    pub lang: Lang,
}

/// The language consumed at a specification boundary.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Lang {
    /// An AL script.
    Al,
    /// A P4 program, at the top level only.
    P4,
}

/// Supplies input definitions required by the top level's host interface.
#[derive(Clone, Debug)]
pub enum ProgramConfig {
    /// The AL input whose definitions supply the terminal builtin environment.
    Al { spec_al: al::Spec },
    /// P4 inputs use builtins from the top level's specification.
    P4,
}

impl TowerConfig {
    /// Reads tower roles from a JSON file.
    ///
    /// Input definitions are supplied when the tower is built,
    /// so a corpus can read a tower once and run many inputs through it.
    pub fn from_file(path: &Path) -> Result<Self> {
        /// Ordered levels at the JSON boundary.
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct TowerConfigJson {
            mode: ExecutionMode,
            levels: Vec<LevelConfig>,
        }

        let text = fs::read_to_string(path)
            .map_err(|cause| config_invalid(format!("{}: {cause}", path.display())))?;
        let config: TowerConfigJson = serde_json::from_str(&text)
            .map_err(|cause| config_invalid(format!("{}: {cause}", path.display())))?;
        let mut levels = config.levels;
        let path_parent = path.parent().unwrap_or(Path::new("."));
        // Resolve source paths before validating the level ordering
        for level in &mut levels {
            if !level.specdir.as_os_str().is_empty() && level.specdir.is_relative() {
                level.specdir = path_parent.join(&level.specdir);
            }
        }
        Self::new(config.mode, levels)
    }

    /// Validates ordered levels.
    ///
    /// Specification paths are relative to the current directory.
    /// A single level is both the top level and the base.
    pub fn new(mode: ExecutionMode, levels: Vec<LevelConfig>) -> Result<Self> {
        if levels.is_empty() {
            return Err(config_invalid("a tower requires at least one level"));
        }
        // Only the top level may consume a language other than AL
        let idx_top = levels.len() - 1;
        for (idx, level) in levels.iter().enumerate() {
            if idx != idx_top && level.lang != Lang::Al {
                return Err(config_invalid(format!(
                    "invalid language at level {idx}: expected Al"
                )));
            }
            if level.rel.is_empty() || level.specdir.as_os_str().is_empty() {
                return Err(config_invalid(format!(
                    "level {idx} requires a specification path and entry relation"
                )));
            }
        }
        Ok(Self { mode, levels })
    }

    pub(super) fn top(&self) -> &LevelConfig {
        self.levels.last().expect("validated nonempty levels")
    }

    /// Returns the language consumed by the top level.
    pub fn top_lang(&self) -> Lang {
        self.top().lang
    }
}
