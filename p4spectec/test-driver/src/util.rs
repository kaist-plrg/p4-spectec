//! Helpers shared by the snapshot, corpus, and diagnostic runners
//!
//! Each runner reads its registration from `suites/<name>.json` with `load`.
//! Corpus paths are relative to the repository, expectations to the driver,
//! and diagnostic inputs and expectations to `testdata/spec`.

use std::{
    fs,
    path::{Path, PathBuf},
};

use indicatif::{ProgressBar, ProgressStyle};
use serde::{Deserialize, de::DeserializeOwned};

use crate::{Error, Result};

/// Selects the production interpreter used by a corpus suite.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq, clap::Subcommand)]
#[serde(rename_all = "lowercase")]
pub enum Language {
    Al,
    Sl,
    Pl,
}

impl Language {
    /// Returns the language's registration spelling.
    pub fn name(self) -> &'static str {
        match self {
            Self::Al => "al",
            Self::Sl => "sl",
            Self::Pl => "pl",
        }
    }
}

/// Reads the registration `suites/<name>.json` with its path attached to errors.
pub fn load<T: DeserializeOwned>(name: &str) -> Result<T> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(format!("suites/{name}.json"));
    let text = fs::read_to_string(&path)
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    serde_json::from_str(&text).map_err(|error| Error::from(format!("{}: {error}", path.display())))
}

/// Returns the specification sources shared by every runner.
pub fn spec() -> [PathBuf; 1] {
    [PathBuf::from("spec")]
}

/// Resolves an expectation relative to the driver.
pub fn expected_path(path: &Path) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(path)
}

/// Creates a progress bar over `len` tests.
pub fn progress_bar(len: usize) -> Result<ProgressBar> {
    let style = ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")?;
    Ok(ProgressBar::new(len as u64).with_style(style))
}
