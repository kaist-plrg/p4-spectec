//! JSON descriptions of AL interpreter towers
//!
//! Levels run from the native boot toward an AL or P4 target.
//! The native backend is AL or SL; all meta interfaces represent AL scripts.
//! Specification paths are resolved relative to the JSON file.

use super::{Result, error};
use serde::Deserialize;
use std::{
    fs,
    path::{Path, PathBuf},
};

/// The native interpreter used for the outermost level.
#[derive(Clone, Copy, Debug, Deserialize, clap::ValueEnum, Default)]
#[serde(rename_all = "lowercase")]
pub enum Mode {
    /// Execute the algorithmic representation.
    Al,
    /// Execute the structured representation.
    #[default]
    Sl,
}

/// The language consumed at a specification boundary.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Interface {
    /// An AL script.
    Al,
    /// A P4 program, at the target only.
    P4,
}

/// One specification and the relation used to enter it.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Level {
    /// A specification file or directory.
    pub specdir: PathBuf,
    /// The single-input entry relation.
    pub rel: String,
    /// The language accepted by this level.
    pub interface: Interface,
}

/// Validated specification roles, ordered from boot toward target.
#[derive(Clone, Debug)]
pub enum Levels {
    /// One target also supplies the native entry point.
    Single(Level),
    /// A native boot interprets intermediate specifications and the target.
    Layered { boot: Level, interms: Vec<Level>, target: Level },
}

/// Validated tower roles and the native execution mode.
#[derive(Clone, Debug)]
pub struct Config {
    /// The interpreter executing the boot specification.
    pub(super) mode: Mode,
    /// Specification roles established at the JSON boundary.
    pub(super) levels: Levels,
}

/// Ordered levels at the JSON boundary.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ConfigJson {
    mode: Mode,
    levels: Vec<Level>,
}

impl Config {
    /// Reads a tower, resolves its paths, and separates its level roles.
    pub fn from_file(path: &Path) -> Result<Self> {
        let text = fs::read_to_string(path)
            .map_err(|cause| error(format!("{}: {cause}", path.display())))?;
        let config: ConfigJson = serde_json::from_str(&text)
            .map_err(|cause| error(format!("{}: {cause}", path.display())))?;
        let mut levels = config.levels;
        // Validate the external ordering before constructing named roles
        if levels.is_empty() {
            return Err(error("a tower requires at least one level"));
        }
        let idx_target = levels.len() - 1;
        let path_parent = path.parent().unwrap_or(Path::new("."));
        for (idx, level) in levels.iter_mut().enumerate() {
            if idx != idx_target && level.interface != Interface::Al {
                return Err(error(format!("invalid interface at level {idx}: expected Al")));
            }
            if level.rel.is_empty() || level.specdir.as_os_str().is_empty() {
                return Err(error(format!(
                    "level {idx} requires a specification path and entry relation"
                )));
            }
            if level.specdir.is_relative() {
                level.specdir = path_parent.join(&level.specdir);
            }
        }
        // A singleton target is also the boot; do not duplicate its runner
        let target = levels.pop().expect("validated nonempty levels");
        let levels = if levels.is_empty() {
            Levels::Single(target)
        } else {
            let mut levels = levels.into_iter();
            let boot = levels.next().expect("nonempty meta levels");
            Levels::Layered { boot, interms: levels.collect(), target }
        };
        Ok(Self { mode: config.mode, levels })
    }
}
