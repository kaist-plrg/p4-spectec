//! Test infrastructure shared by every suite
//!
//! `root` and `collect` locate test inputs,
//! and `Outcome` names what a case must reach.
//! `corpus` runs corpora,
//! `fixture` lists a fixture directory under `tests/p4spectec/` and checks its snapshots,
//! and `output` silences interpreter stdout.
//! Nothing here depends on a particular suite.

use std::{
    fs,
    path::{Path, PathBuf},
};

pub mod corpus;
pub mod fixture;
pub mod output;

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;

/// Outcome of a case: the one a suite requires, or the one a case reached.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Pass,
    Fail,
}

impl Outcome {
    /// Spells the outcome as run logs record it.
    pub fn name(self) -> &'static str {
        match self {
            Self::Pass => "pass",
            Self::Fail => "fail",
        }
    }
}

/// Returns the repository root, against which test paths are spelled.
pub fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// Lists the files under `dir` ending in `suffix`, recursively and sorted by path.
pub fn collect(dir: &Path, suffix: &str) -> Result<Vec<PathBuf>> {
    let mut paths = fs::read_dir(dir)?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::io::Result<Vec<_>>>()?;
    paths.sort();
    let mut files = Vec::new();
    for path in paths {
        if fs::metadata(&path)?.is_dir() {
            files.extend(collect(&path, suffix)?);
        } else if path.to_str().is_some_and(|path| path.ends_with(suffix)) {
            files.push(path);
        }
    }
    Ok(files)
}
