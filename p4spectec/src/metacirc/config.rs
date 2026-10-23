//! JSON descriptions of AL interpreter towers
//!
//! Levels run from the native base toward an AL or P4 target.
//! The native backend is AL or SL; all meta interfaces represent AL scripts.
//! Specification paths are resolved relative to the JSON file.
//! Only the target must name an entry relation;
//! `Tower::build` requires one at every level of a nested tower it patches.

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
    /// Specification roles established at the JSON boundary.
    pub(super) levels: LevelConfigs,
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

/// Validated specification roles, ordered from base toward target.
#[derive(Clone, Debug)]
pub enum LevelConfigs {
    /// One target also supplies the native entry point.
    Direct(LevelConfig),
    /// A native base interprets intermediate specifications and the target.
    Nested { base: LevelConfig, interms: Vec<LevelConfig>, target: LevelConfig },
}

/// One specification and the relation used to enter it.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LevelConfig {
    /// A specification file or directory.
    pub specdir: PathBuf,
    /// The single-input entry relation; required at the target.
    #[serde(default)]
    pub rel: Option<String>,
    /// The language accepted by this level.
    pub lang: Lang,
}

/// The language consumed at a specification boundary.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Lang {
    /// An AL script.
    Al,
    /// A P4 program, at the target only.
    P4,
}

/// Supplies input definitions required by the target's host interface.
#[derive(Clone, Debug)]
pub enum ProgramConfig {
    /// The AL input whose definitions supply the terminal builtin environment.
    Al { spec_al: al::Spec },
    /// P4 inputs use builtins from the target specification.
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
    pub fn new(mode: ExecutionMode, levels: Vec<LevelConfig>) -> Result<Self> {
        Ok(Self { mode, levels: LevelConfigs::new(levels)? })
    }

    /// Returns the language consumed by the target level.
    pub fn target_lang(&self) -> Lang {
        self.levels.target().lang
    }

    /// Returns the relation configured to enter the target level.
    pub fn target_rel(&self) -> &str {
        self.levels
            .target()
            .rel
            .as_deref()
            .expect("validated target entry relation")
    }
}

impl LevelConfigs {
    pub(super) fn target(&self) -> &LevelConfig {
        match self {
            Self::Direct(target) | Self::Nested { target, .. } => target,
        }
    }

    /// Validates the ordering and separates the native base from its inputs.
    fn new(mut levels: Vec<LevelConfig>) -> Result<Self> {
        // Validate the external ordering before constructing named roles
        if levels.is_empty() {
            return Err(config_invalid("a tower requires at least one level"));
        }
        let idx_target = levels.len() - 1;
        for (idx, level) in levels.iter().enumerate() {
            if idx != idx_target && level.lang != Lang::Al {
                return Err(config_invalid(format!(
                    "invalid language at level {idx}: expected Al"
                )));
            }
            // Only the target is entered by name; other levels may omit it
            let rel_missing = match &level.rel {
                Some(rel) => rel.is_empty(),
                None => idx == idx_target,
            };
            if rel_missing || level.specdir.as_os_str().is_empty() {
                return Err(config_invalid(format!(
                    "level {idx} requires a specification path and entry relation"
                )));
            }
        }
        // A singleton target is also the base; do not duplicate its runner
        let target = levels.pop().expect("validated nonempty levels");
        Ok(if levels.is_empty() {
            Self::Direct(target)
        } else {
            let mut levels = levels.into_iter();
            let base = levels.next().expect("nonempty meta levels");
            Self::Nested { base, interms: levels.collect(), target }
        })
    }
}
