//! JSON descriptions of AL interpreter towers
//!
//! Levels run from the native base toward an AL or P4 target.
//! The native backend is AL or SL; all meta interfaces represent AL scripts.
//! Specification paths are resolved relative to the JSON file.

use super::{Result, error::config_invalid};
use serde::Deserialize;
use std::{
    fs,
    path::{Path, PathBuf},
};

/// Validated tower roles and the native execution mode.
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
    /// A P4 program, at the target only.
    P4,
}

impl TowerConfig {
    /// Reads a tower, resolves its paths, and separates its level roles.
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
        // Validate the external ordering before constructing named roles
        if levels.is_empty() {
            return Err(config_invalid("a tower requires at least one level"));
        }
        let idx_target = levels.len() - 1;
        let path_parent = path.parent().unwrap_or(Path::new("."));
        for (idx, level) in levels.iter_mut().enumerate() {
            if idx != idx_target && level.lang != Lang::Al {
                return Err(config_invalid(format!(
                    "invalid language at level {idx}: expected Al"
                )));
            }
            if level.rel.is_empty() || level.specdir.as_os_str().is_empty() {
                return Err(config_invalid(format!(
                    "level {idx} requires a specification path and entry relation"
                )));
            }
            if level.specdir.is_relative() {
                level.specdir = path_parent.join(&level.specdir);
            }
        }
        // A singleton target is also the base; do not duplicate its runner
        let target = levels.pop().expect("validated nonempty levels");
        let levels = if levels.is_empty() {
            LevelConfigs::Direct(target)
        } else {
            let mut levels = levels.into_iter();
            let base = levels.next().expect("nonempty meta levels");
            LevelConfigs::Nested { base, interms: levels.collect(), target }
        };
        Ok(Self { mode: config.mode, levels })
    }
}
