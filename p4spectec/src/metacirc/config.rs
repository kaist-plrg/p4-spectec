//! JSON descriptions of AL interpreter towers
//!
//! Levels run from the native base toward a top level consuming AL, IL, P4, or KORE.
//! Each level names the language it accepts:
//! the level above it is passed to it as AL or IL.
//! The native backend is AL or SL, or a K library compiled from the base (`k`).
//! Specification and library paths are resolved relative to the JSON file.

use super::{Result, error::config_invalid};
pub use crate::interface::Lang;
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
    /// Execute a K base specification through the library `kompile` made of it.
    K,
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
    /// The library compiled from a K base specification, for a `k` tower's base only.
    #[serde(default)]
    pub kompiled: Option<PathBuf>,
}

impl TowerConfig {
    /// Reads tower roles from a JSON file.
    ///
    /// Inputs are supplied only after the tower is built,
    /// so a corpus can build a tower once and run many inputs through it.
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
        // Resolve source and library paths before validating the level ordering
        for level in &mut levels {
            if !level.specdir.as_os_str().is_empty() && level.specdir.is_relative() {
                level.specdir = path_parent.join(&level.specdir);
            }
            if let Some(kompiled) = &mut level.kompiled
                && kompiled.is_relative()
            {
                *kompiled = path_parent.join(&*kompiled);
            }
        }
        Self::new(config.mode, levels)
    }

    /// Validates ordered levels.
    ///
    /// Specification paths are relative to the current directory.
    /// A single level is both the top level and the base.
    /// A `k` tower's base accepts AL and names its compiled library;
    /// KORE is accepted only by a single level, since no level stacks over K yet.
    pub fn new(mode: ExecutionMode, levels: Vec<LevelConfig>) -> Result<Self> {
        if levels.is_empty() {
            return Err(config_invalid("a tower requires at least one level"));
        }
        // A level below another receives the level above as a script
        let idx_top = levels.len() - 1;
        for (idx, level) in levels.iter().enumerate() {
            if idx != idx_top && !level.lang.is_script() {
                return Err(config_invalid(format!(
                    "invalid language at level {idx}: expected al or il"
                )));
            }
            if level.rel.is_empty() || level.specdir.as_os_str().is_empty() {
                return Err(config_invalid(format!(
                    "level {idx} requires a specification path and entry relation"
                )));
            }
            // Only the base of a k tower runs from a compiled library
            if level.kompiled.is_some() && (idx != 0 || !matches!(mode, ExecutionMode::K)) {
                return Err(config_invalid(format!(
                    "kompiled at level {idx}: only the base of a k tower takes a library"
                )));
            }
        }
        // A K base interprets AL, from the library compiled from it
        let base = &levels[0];
        if matches!(mode, ExecutionMode::K) && (base.lang != Lang::Al || base.kompiled.is_none()) {
            return Err(config_invalid(
                "a k tower's base accepts al and requires kompiled, its compiled library",
            ));
        }
        // Stacking a level over a K definition is not implemented
        if levels[idx_top].lang == Lang::Kore && idx_top != 0 {
            return Err(config_invalid(
                "kore is accepted only by a single-level tower; stacking over K is not implemented",
            ));
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
