//! K definitions run through a K-in-ST tower and compared with `krun`,
//! registered in `suites/metacirc/k-in-st.toml`
//!
//! A suite names a tower whose single level is spec-k over KORE,
//! and `root`, a directory of cases. A case is a K definition compiled
//! to KORE (`definition.kore`), an initial term `krun` made for a program,
//! and the final configuration `krun` printed for it.
//! `run` would run each case with `p4spectec::metacirc::krun`
//! and compare its output with `krun`'s, as #395's `spec-k/tests/difftest.py` does.
//! The comparison is a slot that the K-in-ST implementation fills,
//! together with the case layout under `root`;
//! `docs/metacirc/k-in-st.md` says how.

use std::path::PathBuf;

use serde::Deserialize;

use super::config::{ByBase, Configuration};
use crate::harness::{self, Error, Result};

/// Supplies configurations and suites of K-in-ST runs.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub configs: Vec<Configuration>,
    pub suites: Vec<KInStSuite>,
}

/// Registers the cases under `root`, run through one tower per base.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct KInStSuite {
    pub name: String,
    pub towers: ByBase<PathBuf>,
    pub root: PathBuf,
}

/// Runs the suites under the named configuration, comparing each case with `krun`.
///
/// `name_suite` restricts the run to the registered suite of that name.
pub fn run(name_config: &str, name_suite: Option<&str>) -> Result<()> {
    let registration: Registration = harness::load("metacirc/k-in-st")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    if config.skip_pending("k-in-st") {
        return Ok(());
    }
    if let Some(name) = name_suite
        && !registration.suites.iter().any(|suite| suite.name == name)
    {
        return Err(Error::from(format!("unknown K-in-ST suite {name}")));
    }
    Err(Error::from("comparing K-in-ST runs with krun is not implemented yet"))
}
