//! JSON descriptions of AL towers over a P4 target
//!
//! Levels run from the outer interpreter toward the P4 specification.
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

/// Ordered levels and the outer native execution mode.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Config {
    /// The outer native backend.
    pub mode: Mode,
    /// At least one AL meta level followed by the P4 target.
    pub levels: Vec<Level>,
}

impl Config {
    /// Reads a configuration and resolves its specification paths.
    pub fn from_file(path: &Path) -> Result<Self> {
        let text = fs::read_to_string(path)
            .map_err(|cause| error(format!("{}: {cause}", path.display())))?;
        let mut config: Self = serde_json::from_str(&text)
            .map_err(|cause| error(format!("{}: {cause}", path.display())))?;
        config.validate()?;
        // Keep CLI program/include paths independent of the configuration location
        let path_parent = path.parent().unwrap_or(Path::new("."));
        for level in &mut config.levels {
            if level.specdir.is_relative() {
                level.specdir = path_parent.join(&level.specdir);
            }
        }
        Ok(config)
    }

    /// Rejects unsupported interfaces and incomplete tower boundaries.
    pub fn validate(&self) -> Result<()> {
        // A tower must contain both an interpreter specification and a target
        if self.levels.len() < 2 {
            return Err(error("a tower requires at least two levels: AL meta and P4 target"));
        }
        // Only the final boundary accepts P4 programs
        for (idx, level) in self.levels.iter().enumerate() {
            let interface =
                if idx + 1 == self.levels.len() { Interface::P4 } else { Interface::Al };
            if level.interface != interface {
                return Err(error(format!(
                    "invalid interface at level {idx}: expected {interface:?}"
                )));
            }
            if level.rel.is_empty() || level.specdir.as_os_str().is_empty() {
                return Err(error(format!(
                    "level {idx} requires a specification path and entry relation"
                )));
            }
        }
        Ok(())
    }
}
