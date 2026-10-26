//! Registered simulation suites through a metacircular tower
//!
//! `run` looks up the suites named by the metacirc registration
//! in the main driver's simulation registration,
//! and runs them with the main driver's `sim::run_suites`
//! on simulators built from the conversion's P4 tower,
//! so each suite compares with its native expected file.

use p4spectec::{
    lang::data::encoding::Encoding,
    metacirc::{self, TowerConfig},
};

use super::suite::MetacircSim;
use crate::{Error, ExecutionOptions, Result, sim, suite::SimConfig};

/// Runs the named simulation suites through the simulation tower.
pub fn run(
    config_sim: &SimConfig,
    metacirc_sim: &MetacircSim,
    options: ExecutionOptions,
) -> Result<()> {
    // Look up each named suite in the simulation registration
    let suites = metacirc_sim
        .suites
        .iter()
        .map(|name| {
            config_sim
                .suites
                .iter()
                .find(|suite| &suite.name == name)
                .ok_or_else(|| Error::Invalid(format!("unknown simulation suite {name}")))
        })
        .collect::<Result<Vec<_>>>()?;
    let config_tower = TowerConfig::from_file(&metacirc_sim.tower)
        .map_err(|error| Error::Invalid(error.to_string()))?;
    sim::run_suites(config_sim, &suites, options, |arch, config_runner| {
        metacirc::build_simulator(&config_tower, arch, config_runner, Encoding::default())
            .map_err(|error| Error::Invalid(error.to_string()))
    })
}
