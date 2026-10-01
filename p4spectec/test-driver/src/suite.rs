//! Typed registration files for individual acceptance runners
//!
//! `Index::load` resolves module filenames from the root index.
//! Each module loader reads its own registration type and validates its paths.
//! Corpus paths are relative to the repository;
//! diagnostic inputs and expectations are relative to the driver.

use std::{
    collections::BTreeSet,
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
        .map_err(|error| Error::Invalid(format!("{}: {error}", path.display())))?;
    serde_json::from_str(&text)
        .map_err(|error| Error::Invalid(format!("{}: {error}", path.display())))
}

/// Resolves an expectation or diagnostic input relative to the driver.
pub fn expected_path(path: &Path) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(path)
}

/// Requires each registered path to exist before any test can run.
fn paths_exist(paths: &[PathBuf], role: &str) -> Result<()> {
    if paths.is_empty() {
        return Err(Error::Invalid(format!("{role}: no paths registered")));
    }
    for path in paths {
        if !path.exists() {
            return Err(Error::Invalid(format!("{role}: missing {}", path.display())));
        }
    }
    Ok(())
}

/// Requires a regular expectation file instead of an absent baseline.
fn expectation_exists(path: &Path) -> Result<()> {
    if !expected_path(path).is_file() {
        return Err(Error::Invalid(format!("missing expectation {}", path.display())));
    }
    Ok(())
}

/// Requires corpus and include roots to be directories.
fn directories_exist(paths: &[PathBuf], role: &str) -> Result<()> {
    paths_exist(paths, role)?;
    for path in paths {
        if !path.is_dir() {
            return Err(Error::Invalid(format!("{role}: not a directory {}", path.display())));
        }
    }
    Ok(())
}

/// Rejects empty or repeated interpreter selections.
fn languages_valid(languages: &[Language]) -> Result<()> {
    if languages.is_empty() {
        return Err(Error::Invalid("suite has no interpreter languages".into()));
    }
    let mut names = BTreeSet::new();
    for lang in languages {
        if !names.insert(lang.name()) {
            return Err(Error::Invalid(format!("repeated language {}", lang.name())));
        }
    }
    Ok(())
}

/// Rejects empty or duplicate names within a module's registrations.
fn names_valid<'a>(names: impl IntoIterator<Item = &'a str>) -> Result<()> {
    let mut names_seen = BTreeSet::new();
    // Check names in registration order before any execution
    for name in names {
        if name.is_empty() || !names_seen.insert(name) {
            return Err(Error::Invalid(format!("empty or duplicate registration {name}")));
        }
    }
    if names_seen.is_empty() {
        return Err(Error::Invalid("no tests registered".into()));
    }
    Ok(())
}

/// Loads source snapshots directly for elab, algo, or prose.
pub fn load_snapshots(path: &Path) -> Result<Vec<Snapshot>> {
    let snapshots: Vec<Snapshot> = load(path)?;
    names_valid(snapshots.iter().map(|snapshot| snapshot.name.as_str()))?;
    // Validate only the selected module's sources and expectations
    for snapshot in &snapshots {
        paths_exist(&snapshot.inputs, &snapshot.name)?;
        expectation_exists(&snapshot.expected)?;
    }
    Ok(snapshots)
}

/// Loads structured snapshots with module-owned rule-group options.
pub fn load_structure(path: &Path) -> Result<Vec<StructureSnapshot>> {
    let snapshots: Vec<StructureSnapshot> = load(path)?;
    names_valid(snapshots.iter().map(|snapshot| snapshot.name.as_str()))?;
    // Validate source snapshots before structural conversion
    for snapshot in &snapshots {
        paths_exist(&snapshot.inputs, &snapshot.name)?;
        expectation_exists(&snapshot.expected)?;
    }
    Ok(snapshots)
}

/// Loads EL and PL document registrations without a stage selector.
pub fn load_adoc(path: &Path) -> Result<AdocSuite> {
    let suites: AdocSuite = load(path)?;
    names_valid(
        suites
            .el
            .iter()
            .map(|snapshot| snapshot.name.as_str())
            .chain(suites.pl.iter().map(|snapshot| snapshot.name.as_str())),
    )?;
    // Both document modes require complete source and expectation pairs
    for (name, inputs, expected) in suites
        .el
        .iter()
        .map(|snapshot| (&snapshot.name, &snapshot.inputs, &snapshot.expected))
        .chain(
            suites
                .pl
                .iter()
                .map(|snapshot| (&snapshot.name, &snapshot.inputs, &snapshot.expected)),
        )
    {
        paths_exist(inputs, name)?;
        expectation_exists(expected)?;
    }
    Ok(suites)
}

/// Loads parser dependencies and roundtrip corpus registrations.
pub fn load_parsing(path: &Path) -> Result<ParseConfig> {
    let config: ParseConfig = load(path)?;
    // Validate dependencies declared by this module before corpus discovery
    paths_exist(&config.spec, "parser specification")?;
    directories_exist(&config.includes, "parser include directories")?;
    let suites = &config.suites;
    names_valid(suites.iter().map(|suite| suite.name.as_str()))?;
    // Parsing discovers files only within the registered directories
    for suite in suites {
        directories_exist(&suite.roots, &suite.name)?;
        expectation_exists(&suite.expected)?;
    }
    Ok(config)
}

/// Loads execution dependencies and corpus registrations.
pub fn load_execution(path: &Path) -> Result<RunConfig> {
    let config: RunConfig = load(path)?;
    // Validate dependencies declared by this module before corpus discovery
    paths_exist(&config.spec, "execution specification")?;
    directories_exist(&config.includes, "execution include directories")?;
    for path in &config.excludes {
        directories_exist(std::slice::from_ref(path), "execution exclusions")?;
    }
    let suites = &config.suites;
    names_valid(suites.iter().map(|suite| suite.name.as_str()))?;
    // Entry relations and interpreter selections belong to each corpus
    for suite in suites {
        directories_exist(std::slice::from_ref(&suite.root), &suite.name)?;
        languages_valid(&suite.languages)?;
        if suite.relation.is_empty() {
            return Err(Error::Invalid(format!("{}: empty entry relation", suite.name)));
        }
        expectation_exists(&suite.expected)?;
    }
    Ok(config)
}

/// Loads simulation dependencies and explicit P4/STF corpus pairing.
pub fn load_simulation(path: &Path) -> Result<SimConfig> {
    let config: SimConfig = load(path)?;
    // Validate dependencies declared by this module before corpus discovery
    paths_exist(&config.spec, "simulation specification")?;
    directories_exist(&config.includes, "simulation include directories")?;
    for path in &config.excludes {
        directories_exist(std::slice::from_ref(path), "simulation exclusions")?;
    }
    let suites = &config.suites;
    names_valid(suites.iter().map(|suite| suite.name.as_str()))?;
    // Validate simulation inputs and optional patches before discovery
    for suite in suites {
        directories_exist(&[suite.p4.clone(), suite.stf.clone()], &suite.name)?;
        if let Some(path) = &suite.patches {
            directories_exist(std::slice::from_ref(path), &suite.name)?;
        }
        languages_valid(&suite.languages)?;
        if !matches!(suite.arch.as_str(), "v1model" | "ebpf" | "psa") {
            return Err(Error::Invalid(format!(
                "{}: unknown architecture {}",
                suite.name, suite.arch
            )));
        }
        expectation_exists(&suite.expected)?;
    }
    Ok(config)
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
        // Validate and display each module through its normal loader
        println!("elab: {:#?}", load_snapshots(&self.suites.elab)?);
        println!("algo: {:#?}", load_snapshots(&self.suites.algo)?);
        println!("structure: {:#?}", load_structure(&self.suites.structure)?);
        println!("prose: {:#?}", load_snapshots(&self.suites.prose)?);
        println!("adoc: {:#?}", load_adoc(&self.suites.adoc)?);
        println!("p4parse: {:#?}", load_parsing(&self.suites.p4parse)?);
        println!("run: {:#?}", load_execution(&self.suites.run)?);
        println!("sim: {:#?}", load_simulation(&self.suites.sim)?);
        self.suites.diagnostics.list()
    }
}
