//! Test infrastructure shared by every suite
//!
//! `load` reads a suite's registration from `suites/<name>.toml`.
//! `corpus` discovers sources and compares per-file results,
//! `interpreter` builds runners, `log` writes timing logs,
//! `output` silences interpreter stdout,
//! and `worker` runs tests in a process killed after a timeout.
//! Nothing here depends on a particular suite.

pub mod corpus;
pub mod interpreter;
pub mod log;
pub mod output;
pub mod worker;

use std::{fs, path::Path};

use serde::de::DeserializeOwned;

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;

/// Reads the registration `suites/<name>.toml` with its path attached to errors.
pub fn load<T: DeserializeOwned>(name: &str) -> Result<T> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(format!("suites/{name}.toml"));
    let text = fs::read_to_string(&path)
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    toml::from_str(&text).map_err(|error| Error::from(format!("{}: {error}", path.display())))
}
