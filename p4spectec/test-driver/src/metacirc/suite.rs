//! Typed registration files of the metacircular driver
//!
//! `load_metacirc` reads the corpus, simulation, and snapshot registrations
//! for one IL-to-AL conversion, resolving towers against the registration file;
//! `load_metaparse` reads the codec round-trip inputs.
//! Shared path and name checks come from the common `suite` module.

use std::path::{Path, PathBuf};

use p4spectec::metacirc::{TowerConfig, config::Lang};
use serde::Deserialize;

use crate::{
    Error, Result, diagnostic,
    suite::{directories_exist, expectation_exists, load, names_valid, paths_exist},
};

/// Registers specification inputs whose IL and AL codecs must round-trip.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MetaparseSuite {
    pub name: String,
    pub inputs: Vec<PathBuf>,
}

/// Selects how IL is converted to AL at the base of a metacircular tower.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, clap::ValueEnum)]
pub enum Conversion {
    /// The native `algo` pass converts the base specification and AL inputs.
    #[default]
    Native,
    /// The base level converts its IL input with the meta specification's `Algo_IL`.
    Meta,
}

/// Names a value for each conversion; a missing value skips that conversion.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ByConversion<T> {
    pub native: Option<T>,
    pub meta: Option<T>,
}

impl<T> ByConversion<T> {
    fn take(self, conversion: Conversion) -> Option<T> {
        match conversion {
            Conversion::Native => self.native,
            Conversion::Meta => self.meta,
        }
    }
}

/// Registers a metacircular corpus run through one tower per conversion.
///
/// The selected tower's top level decides the entry relation
/// and whether inputs are P4 programs or AL or IL scripts.
/// P4 suites compare against an `expected` file of per-program results;
/// script suites give every script under `root` one `outcome`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct MetacircSuiteJson {
    name: String,
    towers: ByConversion<PathBuf>,
    root: PathBuf,
    expected: Option<PathBuf>,
    outcome: Option<diagnostic::Outcome>,
}

/// A metacircular suite with the tower of the selected conversion.
#[derive(Debug)]
pub struct MetacircSuite {
    pub name: String,
    pub tower: PathBuf,
    pub root: PathBuf,
    pub expected: Option<PathBuf>,
    pub outcome: Option<diagnostic::Outcome>,
}

/// Registers simulation suites run through one P4 tower per conversion.
///
/// Suites are named from the simulation registration,
/// whose corpus pairing, exclusions, and expected files they reuse.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct MetacircSimJson {
    towers: ByConversion<PathBuf>,
    suites: Vec<String>,
}

/// Simulation suite names with the tower of the selected conversion.
#[derive(Debug)]
pub struct MetacircSim {
    pub tower: PathBuf,
    pub suites: Vec<String>,
}

/// Names the snapshot registrations of the meta specification by stage.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MetacircSnapshots {
    pub elab: PathBuf,
    pub algo: PathBuf,
    pub structure: PathBuf,
    pub prose: PathBuf,
}

/// Supplies metacircular corpus inputs, towers, expectations, snapshots,
/// and the meta codec round-trip inputs.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct MetacircConfigJson {
    includes: Vec<PathBuf>,
    excludes: ByConversion<Vec<PathBuf>>,
    snapshots: MetacircSnapshots,
    metaparse: PathBuf,
    suites: Vec<MetacircSuiteJson>,
    sim: MetacircSimJson,
}

/// Metacircular registrations for the selected conversion.
#[derive(Debug)]
pub struct MetacircConfig {
    pub includes: Vec<PathBuf>,
    pub excludes: Vec<PathBuf>,
    pub snapshots: MetacircSnapshots,
    pub metaparse: PathBuf,
    pub suites: Vec<MetacircSuite>,
    /// Simulation through a tower, absent when the conversion registers none.
    pub sim: Option<MetacircSim>,
}

/// Loads codec round-trip inputs.
pub fn load_metaparse(path: &Path) -> Result<Vec<MetaparseSuite>> {
    let suites: Vec<MetaparseSuite> = load(path)?;
    names_valid(suites.iter().map(|suite| suite.name.as_str()))?;
    for suite in &suites {
        paths_exist(&suite.inputs, &suite.name)?;
    }
    Ok(suites)
}

/// Loads metacircular registrations for one conversion,
/// with towers resolved against this file.
///
/// Suites without a tower for the conversion are left out.
pub fn load_metacirc(path: &Path, conversion: Conversion) -> Result<MetacircConfig> {
    let config: MetacircConfigJson = load(path)?;
    let excludes = config.excludes.take(conversion).unwrap_or_default();
    // Check include directories and exclusion lists before individual suites
    directories_exist(&config.includes, "execution include directories")?;
    for path in &excludes {
        directories_exist(std::slice::from_ref(path), "execution exclusions")?;
    }
    names_valid(config.suites.iter().map(|suite| suite.name.as_str()))?;
    let path_parent = path.parent().unwrap_or_else(|| Path::new("."));
    // Resolve snapshot and codec registrations relative to this registration file
    let mut snapshots = config.snapshots;
    for path in
        [&mut snapshots.elab, &mut snapshots.algo, &mut snapshots.structure, &mut snapshots.prose]
    {
        *path = path_parent.join(&*path);
    }
    let metaparse = path_parent.join(config.metaparse);
    let mut suites = Vec::new();
    for suite in config.suites {
        let MetacircSuiteJson { name, towers, root, expected, outcome } = suite;
        let Some(tower) = towers.take(conversion) else {
            continue;
        };
        // Resolve each tower relative to this registration file
        let suite = MetacircSuite { name, tower: path_parent.join(tower), root, expected, outcome };
        let config_tower = TowerConfig::from_file(&suite.tower)
            .map_err(|error| Error::Invalid(format!("{}: {error}", suite.name)))?;
        directories_exist(std::slice::from_ref(&suite.root), &suite.name)?;
        // The tower's input language selects the kind of verdict
        match (config_tower.top_lang(), &suite.expected, suite.outcome) {
            (Lang::P4, Some(path_expected), None) => expectation_exists(path_expected)?,
            (Lang::Al | Lang::Il, None, Some(_)) => {}
            (Lang::P4, _, _) => {
                return Err(Error::Invalid(format!(
                    "{}: a P4 suite needs an expected file and no outcome",
                    suite.name
                )));
            }
            (Lang::Al | Lang::Il, _, _) => {
                return Err(Error::Invalid(format!(
                    "{}: a script suite needs an outcome and no expected file",
                    suite.name
                )));
            }
        }
        suites.push(suite);
    }
    let MetacircSimJson { towers: towers_sim, suites: names_sim } = config.sim;
    let sim = match towers_sim.take(conversion) {
        Some(tower) => {
            // Resolve the simulation tower relative to this registration file
            let tower = path_parent.join(tower);
            let config_tower = TowerConfig::from_file(&tower)
                .map_err(|error| Error::Invalid(format!("sim: {error}")))?;
            // Simulation enters the P4 specification at the top level
            if !matches!(config_tower.top_lang(), Lang::P4) {
                return Err(Error::Invalid("sim: the tower must take P4 programs".into()));
            }
            names_valid(names_sim.iter().map(String::as_str))?;
            Some(MetacircSim { tower, suites: names_sim })
        }
        // A conversion without a tower registers no simulation
        None => None,
    };
    Ok(MetacircConfig { includes: config.includes, excludes, snapshots, metaparse, suites, sim })
}
