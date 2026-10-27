//! Typed registration files for individual acceptance runners
//!
//! `Index::load` resolves module filenames from the root index.
//! Each runner reads its own registration type.
//! Corpus paths are relative to the repository, expectations to the driver,
//! and diagnostic inputs and expectations to `testdata/spec`.

use std::{
    fs,
    path::{Path, PathBuf},
};

use serde::{Deserialize, de::DeserializeOwned};

use crate::{Error, Result, diagnostic};

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

/// Registers a transformation snapshot owned by its module.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Snapshot {
    pub name: String,
    pub inputs: Vec<PathBuf>,
    pub expected: PathBuf,
}

/// Registers a structured snapshot with its rule-group setting.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct StructureSnapshot {
    pub name: String,
    pub inputs: Vec<PathBuf>,
    pub expected: PathBuf,
    #[serde(default)]
    pub without_rule_groups: bool,
}

/// Groups EL and PL document snapshots by their rendering entry points.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AdocSuite {
    pub el: Vec<Snapshot>,
    pub pl: Vec<StructureSnapshot>,
}

/// Registers P4 parsing and roundtrip inputs.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ParseSuite {
    pub name: String,
    pub roots: Vec<PathBuf>,
    pub expected: PathBuf,
}

/// Registers a P4 execution corpus and its entry relation.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RunSuite {
    pub name: String,
    pub root: PathBuf,
    pub relation: String,
    pub expected: PathBuf,
    pub languages: Vec<Language>,
}

/// Registers paired P4/STF corpus discovery and optional patches.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SimSuite {
    pub name: String,
    pub arch: String,
    pub p4: PathBuf,
    pub stf: PathBuf,
    pub patches: Option<PathBuf>,
    pub expected: PathBuf,
    pub languages: Vec<Language>,
}

/// Supplies the parser's specification, includes, and corpus registrations.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ParseConfig {
    pub spec: Vec<PathBuf>,
    pub includes: Vec<PathBuf>,
    pub suites: Vec<ParseSuite>,
}

/// Supplies execution dependencies and corpus registrations.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RunConfig {
    pub spec: Vec<PathBuf>,
    pub includes: Vec<PathBuf>,
    pub excludes: Vec<PathBuf>,
    pub suites: Vec<RunSuite>,
}

/// Supplies simulation dependencies and P4/STF registrations.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SimConfig {
    pub spec: Vec<PathBuf>,
    pub includes: Vec<PathBuf>,
    pub excludes: Vec<PathBuf>,
    pub suites: Vec<SimSuite>,
}

/// Names a stage of the spec-language pipeline, in pipeline order.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "lowercase")]
pub enum Stage {
    Parse,
    Elab,
    Algo,
    Structure,
    Prose,
    Specdoc,
    Run,
}

impl Stage {
    /// Returns the stage's registration spelling.
    pub fn name(self) -> &'static str {
        match self {
            Self::Parse => "parse",
            Self::Elab => "elab",
            Self::Algo => "algo",
            Self::Structure => "structure",
            Self::Prose => "prose",
            Self::Specdoc => "specdoc",
            Self::Run => "run",
        }
    }
}

/// Lists the pipeline stages each interpreter language runs.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MicroStages {
    pub al: Vec<Stage>,
    pub sl: Vec<Stage>,
    pub pl: Vec<Stage>,
}

impl MicroStages {
    /// Returns the stages of one language.
    pub fn get(&self, language: Language) -> &[Stage] {
        match language {
            Language::Al => &self.al,
            Language::Sl => &self.sl,
            Language::Pl => &self.pl,
        }
    }
}

/// Registers a directory of spec-language sources with one verdict.
///
/// With outcome success, every source passes the stages up to `stage`;
/// with outcome failure, every source passes the earlier stages
/// and fails at `stage`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MicroSuite {
    pub name: String,
    pub root: PathBuf,
    pub outcome: diagnostic::Outcome,
    pub stage: Stage,
}

/// Supplies language stages, exclusions, and micro suites.
///
/// A missing `excludes` directory holds no entries.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MicroConfig {
    pub excludes: PathBuf,
    pub stages: MicroStages,
    pub suites: Vec<MicroSuite>,
}

/// Names each runner's registration file without storing its tests.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Modules {
    pub elab: PathBuf,
    pub algo: PathBuf,
    pub structure: PathBuf,
    pub prose: PathBuf,
    pub adoc: PathBuf,
    pub p4parse: PathBuf,
    pub run: PathBuf,
    pub sim: PathBuf,
    pub micro: PathBuf,
    pub diagnostics: diagnostic::Modules,
}

/// Indexes module registration filenames without test execution settings.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Index {
    pub suites: Modules,
}

/// Reads typed JSON with its file path attached to errors.
pub fn load<T: DeserializeOwned>(path: &Path) -> Result<T> {
    let text = fs::read_to_string(path)
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    serde_json::from_str(&text).map_err(|error| Error::from(format!("{}: {error}", path.display())))
}

/// Resolves an expectation relative to the driver.
pub fn expected_path(path: &Path) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(path)
}

impl Index {
    /// Resolves module paths relative to the index without loading their settings.
    pub fn load(path: &Path) -> Result<Self> {
        let mut index: Self = load(path)?;
        let path_parent = path.parent().unwrap_or_else(|| Path::new("."));
        // Resolve filenames without loading unrelated modules
        let modules = &mut index.suites;
        for path in [
            &mut modules.elab,
            &mut modules.algo,
            &mut modules.structure,
            &mut modules.prose,
            &mut modules.adoc,
            &mut modules.p4parse,
            &mut modules.run,
            &mut modules.sim,
            &mut modules.micro,
        ]
        .into_iter()
        .chain(modules.diagnostics.paths_mut())
        {
            *path = path_parent.join(&*path);
        }
        Ok(index)
    }

    /// Prints typed module registrations without executing their tests.
    pub fn list(&self) -> Result<()> {
        println!("elab: {:#?}", load::<Vec<Snapshot>>(&self.suites.elab)?);
        println!("algo: {:#?}", load::<Vec<Snapshot>>(&self.suites.algo)?);
        println!("structure: {:#?}", load::<Vec<StructureSnapshot>>(&self.suites.structure)?);
        println!("prose: {:#?}", load::<Vec<Snapshot>>(&self.suites.prose)?);
        println!("adoc: {:#?}", load::<AdocSuite>(&self.suites.adoc)?);
        println!("p4parse: {:#?}", load::<ParseConfig>(&self.suites.p4parse)?);
        println!("run: {:#?}", load::<RunConfig>(&self.suites.run)?);
        println!("sim: {:#?}", load::<SimConfig>(&self.suites.sim)?);
        println!("micro: {:#?}", load::<MicroConfig>(&self.suites.micro)?);
        self.suites.diagnostics.list()
    }
}
