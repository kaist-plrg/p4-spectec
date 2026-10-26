//! Typed registration files of the metacircular driver
//!
//! `Index::load` resolves module filenames from `suites/metacirc/metacirc.json`.
//! Each module loader reads its own registration type and validates its paths,
//! resolving towers against the registration file.
//! Suites name a tower per IL-to-AL conversion;
//! runners select the suites of the requested conversion.

use std::path::{Path, PathBuf};

use p4spectec::metacirc::{TowerConfig, config::Lang};
use serde::Deserialize;

use crate::{
    Error, Result, diagnostic,
    suite::{
        directories_exist, expectation_exists, load, load_snapshots, load_structure, names_valid,
        paths_exist,
    },
};

/// Selects how IL is converted to AL at the base of a metacircular tower.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, clap::ValueEnum)]
pub enum Conversion {
    /// The native `algo` pass converts the base specification and AL inputs.
    #[default]
    Native,
    /// The base level converts its IL input with the meta specification's `Algo_IL`.
    Meta,
}

impl Conversion {
    /// Returns the conversion's command-line spelling.
    pub fn name(self) -> &'static str {
        match self {
            Self::Native => "native",
            Self::Meta => "meta",
        }
    }
}

/// Names a value for each conversion; a missing value skips that conversion.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ByConversion<T> {
    pub native: Option<T>,
    pub meta: Option<T>,
}

impl<T> ByConversion<T> {
    /// Returns the value of `conversion`, if registered.
    pub fn get(&self, conversion: Conversion) -> Option<&T> {
        match conversion {
            Conversion::Native => self.native.as_ref(),
            Conversion::Meta => self.meta.as_ref(),
        }
    }

    fn iter(&self) -> impl Iterator<Item = &T> {
        [self.native.as_ref(), self.meta.as_ref()]
            .into_iter()
            .flatten()
    }

    fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        [self.native.as_mut(), self.meta.as_mut()]
            .into_iter()
            .flatten()
    }
}

/// Registers specification inputs whose IL and AL codecs must round-trip.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MetaparseSuite {
    pub name: String,
    pub inputs: Vec<PathBuf>,
}

/// Registers a corpus run through one tower per conversion.
///
/// The tower's top level decides the entry relation
/// and whether inputs are P4 programs or AL or IL scripts.
/// P4 suites compare against an `expected` file of per-program results;
/// script suites give every script under `root` one `outcome`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RunSuite {
    pub name: String,
    pub towers: ByConversion<PathBuf>,
    pub root: PathBuf,
    pub expected: Option<PathBuf>,
    pub outcome: Option<diagnostic::Outcome>,
}

/// Supplies execution includes, exclusions per conversion, and corpus registrations.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RunConfig {
    pub includes: Vec<PathBuf>,
    pub excludes: ByConversion<Vec<PathBuf>>,
    pub suites: Vec<RunSuite>,
}

/// Names the main driver's simulation suites run through one P4 tower per conversion.
///
/// The suites keep their corpus pairing, exclusions, and expected files.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SimConfig {
    pub towers: ByConversion<PathBuf>,
    pub suites: Vec<String>,
}

/// Names each runner's registration file without storing its tests.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Modules {
    pub run: PathBuf,
    pub sim: PathBuf,
    pub elab: PathBuf,
    pub algo: PathBuf,
    pub structure: PathBuf,
    pub prose: PathBuf,
    pub metaparse: PathBuf,
}

/// Indexes module registration filenames without test execution settings.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Index {
    pub suites: Modules,
}

/// Resolves a tower relative to its registration file and reads its configuration.
fn tower_resolve(path_parent: &Path, tower: &mut PathBuf, role: &str) -> Result<TowerConfig> {
    *tower = path_parent.join(&*tower);
    TowerConfig::from_file(tower).map_err(|error| Error::Invalid(format!("{role}: {error}")))
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

/// Loads corpus and script registrations of every conversion.
pub fn load_run(path: &Path) -> Result<RunConfig> {
    let mut config: RunConfig = load(path)?;
    // Check include directories and every conversion's exclusions before suites
    directories_exist(&config.includes, "execution include directories")?;
    for path in config.excludes.iter().flatten() {
        directories_exist(std::slice::from_ref(path), "execution exclusions")?;
    }
    names_valid(config.suites.iter().map(|suite| suite.name.as_str()))?;
    let path_parent = path.parent().unwrap_or_else(|| Path::new("."));
    for suite in &mut config.suites {
        directories_exist(std::slice::from_ref(&suite.root), &suite.name)?;
        for tower in suite.towers.iter_mut() {
            let config_tower = tower_resolve(path_parent, tower, &suite.name)?;
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
        }
    }
    Ok(config)
}

/// Loads the simulation suite names and their P4 towers.
pub fn load_sim(path: &Path) -> Result<SimConfig> {
    let mut config: SimConfig = load(path)?;
    names_valid(config.suites.iter().map(String::as_str))?;
    let path_parent = path.parent().unwrap_or_else(|| Path::new("."));
    for tower in config.towers.iter_mut() {
        let config_tower = tower_resolve(path_parent, tower, "simulation tower")?;
        // Simulation enters the P4 specification at the top level
        if !matches!(config_tower.top_lang(), Lang::P4) {
            return Err(Error::Invalid(format!(
                "{}: a simulation tower must take P4 programs",
                tower.display()
            )));
        }
    }
    Ok(config)
}

impl Index {
    /// Resolves module paths relative to the index without loading their settings.
    pub fn load(path: &Path) -> Result<Self> {
        let mut index: Self = load(path)?;
        let path_parent = path.parent().unwrap_or_else(|| Path::new("."));
        let modules = &mut index.suites;
        for path in [
            &mut modules.run,
            &mut modules.sim,
            &mut modules.elab,
            &mut modules.algo,
            &mut modules.structure,
            &mut modules.prose,
            &mut modules.metaparse,
        ] {
            *path = path_parent.join(&*path);
        }
        Ok(index)
    }

    /// Prints typed module registrations without executing their tests.
    pub fn list(&self) -> Result<()> {
        // Validate and display each module through its normal loader
        println!("elab: {:#?}", load_snapshots(&self.suites.elab)?);
        println!("algo: {:#?}", load_snapshots(&self.suites.algo)?);
        println!("structure: {:#?}", load_structure(&self.suites.structure)?);
        println!("prose: {:#?}", load_snapshots(&self.suites.prose)?);
        println!("run: {:#?}", load_run(&self.suites.run)?);
        println!("sim: {:#?}", load_sim(&self.suites.sim)?);
        println!("metaparse: {:#?}", load_metaparse(&self.suites.metaparse)?);
        Ok(())
    }
}
