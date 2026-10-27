//! Registered simulation suites through a metacircular tower
//!
//! `run` takes every suite of the main driver's simulation registration,
//! or the one named on the command line,
//! and runs them with the main driver's `sim::run_suites`
//! on simulators built from the conversion's P4 tower,
//! so each suite compares with its native expected file.

use p4spectec::{
    lang::data::encoding::Encoding,
    metacirc::{self, TowerConfig},
};

use super::suite::{self as metacirc_suite, Conversion};
use crate::{Error, ExecutionOptions, Result, sim, suite};

/// Runs the simulation suites of `config_sim` through the tower of `conversion`.
///
/// `name` restricts the run to the registered suite of that name.
pub fn run(
    config_sim: &suite::SimConfig,
    config: &metacirc_suite::SimConfig,
    name: Option<&str>,
    conversion: Conversion,
    options: ExecutionOptions,
) -> Result<()> {
    let path_tower = config.towers.get(conversion).ok_or_else(|| {
        Error::Invalid(format!("no {} simulation tower registered", conversion.name()))
    })?;
    // Restrict the registered suites to the selected name
    let suites = config_sim
        .suites
        .iter()
        .filter(|suite| name.is_none_or(|name| suite.name == name))
        .collect::<Vec<_>>();
    if let (Some(name), true) = (name, suites.is_empty()) {
        return Err(Error::Invalid(format!("unknown simulation suite {name}")));
    }
    let config_tower =
        TowerConfig::from_file(path_tower).map_err(|error| Error::Invalid(error.to_string()))?;
    sim::run_suites(config_sim, &suites, options, |arch, config_runner| {
        metacirc::build_simulator(&config_tower, arch, config_runner, Encoding::default())
            .map_err(|error| Error::Invalid(error.to_string()))
    })
}
