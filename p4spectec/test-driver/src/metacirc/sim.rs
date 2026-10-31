//! Registered simulation suites through a metacircular tower
//!
//! `run` takes every suite of the main driver's `suites/sim.toml`,
//! or the one named on the command line,
//! and runs them with the main driver's `sim::run_suites`
//! on simulators built from the configuration's P4 tower,
//! so each suite compares with its native expected file.
//! With a timeout, a worker per architecture runs each test instead.

use std::{
    path::{Path, PathBuf},
    time::Duration,
};

use p4spectec::{
    lang::data::encoding::Encoding,
    metacirc::{self, TowerConfig},
};

use serde::Deserialize;

use super::config::{ByBase, Configuration};
use crate::{
    harness::{self, Error, Result, worker::Channel},
    suite::sim,
};

/// Names the P4 tower per base and the configurations.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub towers: ByBase<PathBuf>,
    pub configs: Vec<Configuration>,
}

/// Runs the simulation suites of `suites/sim.toml` under the named configuration.
///
/// `name_suite` restricts the run to the registered suite of that name,
/// a test still running `timeout` after it starts is recorded as a timeout,
/// and `path_log` names the directory of the timing log.
pub fn run(
    name_config: &str,
    name_suite: Option<&str>,
    timeout: Option<Duration>,
    path_log: Option<&Path>,
) -> Result<()> {
    let registration: Registration = harness::load("metacirc/sim")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    if config.skip_pending("sim") {
        return Ok(());
    }
    let path_tower = registration.towers.get(config.base).ok_or_else(|| {
        Error::from(format!("no {} simulation tower registered", config.base.name()))
    })?;
    let registration_sim: sim::Registration = harness::load("sim")?;
    // Restrict the registered suites to the selected name
    let suites = registration_sim
        .suites
        .iter()
        .filter(|suite| name_suite.is_none_or(|name| suite.name == name))
        .collect::<Vec<_>>();
    if let (Some(name), true) = (name_suite, suites.is_empty()) {
        return Err(Error::from(format!("unknown simulation suite {name}")));
    }
    let config_tower = TowerConfig::from_file(path_tower)?;
    let text_mode = format!("metacirc sim {}", config.describe());
    let name_log = format!("metacirc-sim-{}", config.name);
    let timeout =
        timeout.map(|timeout| (timeout, vec!["worker".into(), "sim".into(), config.name.clone()]));
    sim::run_suites(&registration_sim, &suites, &text_mode, &name_log, timeout, path_log, |arch| {
        Ok(metacirc::build_simulator(&config_tower, arch, config.runner(), Encoding::default())?)
    })
}

/// Serves `worker sim <config> <arch>` on the configuration's tower.
pub fn serve(name_config: &str, arch: &str) -> Result<()> {
    let channel = Channel::open()?;
    let registration: Registration = harness::load("metacirc/sim")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let path_tower = registration.towers.get(config.base).ok_or_else(|| {
        Error::from(format!("no {} simulation tower registered", config.base.name()))
    })?;
    let config_tower = TowerConfig::from_file(path_tower)?;
    let simulator =
        metacirc::build_simulator(&config_tower, arch, config.runner(), Encoding::default())?;
    let registration_sim: sim::Registration = harness::load("sim")?;
    sim::serve_simulator(channel, &registration_sim.includes, simulator)
}
