//! File registration for the driver's acceptance suites
//!
//! `Registry::load` reads module registrations listed in the root index.
//! It validates source and expectation paths before execution.
//! Corpus paths are relative to the repository; diagnostic inputs and all
//! expectations are relative to the driver. Typed kinds select existing runners.

use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
};

use serde::{Deserialize, de::DeserializeOwned};

use crate::{Error, Result, diagnostic};

/// Selects the production interpreter used by a corpus suite.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
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

/// Selects the output compared by a full specification snapshot.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum Stage {
    Elab,
    Algo,
    Structure,
    Prose,
    AdocEl,
    AdocPl,
}

/// Registers one transformation or document snapshot.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Snapshot {
    pub name: String,
    pub stage: Stage,
    pub inputs: Vec<PathBuf>,
    pub expected: PathBuf,
    #[serde(default)]
    pub without_rule_groups: bool,
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

/// Distinguishes registration data consumed by each runner.
#[derive(Debug, Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case", deny_unknown_fields)]
pub enum Suite {
    Snapshot(Snapshot),
    P4parse(ParseSuite),
    Run(RunSuite),
    Sim(SimSuite),
    Negative { name: String, stage: diagnostic::Suite, cases: Vec<diagnostic::Case> },
}

impl Suite {
    /// Returns the unique registration identifier.
    pub fn name(&self) -> &str {
        match self {
            Self::Snapshot(suite) => &suite.name,
            Self::P4parse(suite) => &suite.name,
            Self::Run(suite) => &suite.name,
            Self::Sim(suite) => &suite.name,
            Self::Negative { name, .. } => name,
        }
    }
}

/// Lists shared configuration and module files relative to the root index.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Registration {
    spec: Vec<PathBuf>,
    includes: Vec<PathBuf>,
    excludes_static: Vec<PathBuf>,
    excludes_dynamic: Vec<PathBuf>,
    suites: Vec<PathBuf>,
}

/// Owns the shared corpus configuration and loaded suite registrations.
#[derive(Debug)]
pub struct Registry {
    pub spec: Vec<PathBuf>,
    pub includes: Vec<PathBuf>,
    pub excludes_static: Vec<PathBuf>,
    pub excludes_dynamic: Vec<PathBuf>,
    pub suites: Vec<Suite>,
}

/// Reads typed JSON with its file path attached to errors.
fn read_registration<T: DeserializeOwned>(path: &Path) -> Result<T> {
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

impl Registry {
    /// Loads module registrations and validates their sources and expectations.
    pub fn load(path: &Path) -> Result<Self> {
        let registration: Registration = read_registration(path)?;
        let path_parent = path.parent().unwrap_or_else(|| Path::new("."));
        let mut suites = Vec::new();
        // Resolve module files from the index without changing input namespaces
        for path_module in &registration.suites {
            let suites_module: Vec<Suite> = read_registration(&path_parent.join(path_module))?;
            suites.extend(suites_module);
        }
        // Validate the complete registration after preserving module order
        let registry = Self {
            spec: registration.spec,
            includes: registration.includes,
            excludes_static: registration.excludes_static,
            excludes_dynamic: registration.excludes_dynamic,
            suites,
        };
        registry.validate()?;
        Ok(registry)
    }

    /// Rejects duplicate registrations and unavailable acceptance inputs.
    fn validate(&self) -> Result<()> {
        paths_exist(&self.spec, "specification")?;
        directories_exist(&self.includes, "include directories")?;
        for path in self.excludes_static.iter().chain(&self.excludes_dynamic) {
            directories_exist(std::slice::from_ref(path), "exclusions")?;
        }
        if self.suites.is_empty() {
            return Err(Error::Invalid("no suites registered".into()));
        }
        let mut names = BTreeSet::new();
        for suite in &self.suites {
            if suite.name().is_empty() || !names.insert(suite.name()) {
                return Err(Error::Invalid(format!("empty or duplicate suite {}", suite.name())));
            }
            match suite {
                // Transformation sources must exist before snapshot execution
                Suite::Snapshot(suite) => {
                    paths_exist(&suite.inputs, &suite.name)?;
                    if suite.without_rule_groups
                        && !matches!(suite.stage, Stage::Structure | Stage::AdocPl)
                    {
                        return Err(Error::Invalid(format!(
                            "{}: rule-group mode is unavailable for this stage",
                            suite.name
                        )));
                    }
                    expectation_exists(&suite.expected)?;
                }
                // Parsing suites discover only their registered corpus roots
                Suite::P4parse(suite) => {
                    directories_exist(&suite.roots, &suite.name)?;
                    expectation_exists(&suite.expected)?;
                }
                // Execution requires a relation and at least one interpreter
                Suite::Run(suite) => {
                    directories_exist(std::slice::from_ref(&suite.root), &suite.name)?;
                    languages_valid(&suite.languages)?;
                    if suite.relation.is_empty() {
                        return Err(Error::Invalid(format!(
                            "{}: empty entry relation",
                            suite.name
                        )));
                    }
                    expectation_exists(&suite.expected)?;
                }
                // Simulation retains explicit architecture and corpus pairing
                Suite::Sim(suite) => {
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
                // Every negative has an explicit input and its own expectation
                Suite::Negative { name, stage, cases } => {
                    if cases.is_empty() {
                        return Err(Error::Invalid(format!("{name}: no negative cases")));
                    }
                    let mut names_case = BTreeSet::new();
                    for case in cases {
                        // Command and run stages consume exact CLI argument files
                        if matches!(stage, diagnostic::Suite::Command | diagnostic::Suite::Run)
                            && !case.uses_cli()
                        {
                            return Err(Error::Invalid(format!(
                                "{name}: {} requires an .args input",
                                case.name
                            )));
                        }
                        if case.name.is_empty() || !names_case.insert(&case.name) {
                            return Err(Error::Invalid(format!(
                                "{name}: empty or duplicate case {}",
                                case.name
                            )));
                        }
                        if !expected_path(&case.input).is_file() {
                            return Err(Error::Invalid(format!(
                                "{name}: missing input {}",
                                case.input.display()
                            )));
                        }
                        for path in &case.inputs {
                            paths_exist(&[expected_path(path)], name)?;
                        }
                        expectation_exists(&case.expected)?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Selects the registered snapshots for a transformation stage.
    pub fn snapshots(&self, stage: Stage) -> Vec<&Snapshot> {
        self.suites
            .iter()
            .filter_map(|suite| match suite {
                Suite::Snapshot(suite) if suite.stage == stage => Some(suite),
                _ => None,
            })
            .collect()
    }

    /// Selects P4 parsing suites in registration order.
    pub fn parsing(&self) -> Vec<&ParseSuite> {
        self.suites
            .iter()
            .filter_map(|suite| match suite {
                Suite::P4parse(suite) => Some(suite),
                _ => None,
            })
            .collect()
    }

    /// Selects execution corpora supporting the requested interpreter.
    pub fn execution(&self, lang: Language) -> Vec<&RunSuite> {
        self.suites
            .iter()
            .filter_map(|suite| match suite {
                Suite::Run(suite) if suite.languages.contains(&lang) => Some(suite),
                _ => None,
            })
            .collect()
    }

    /// Selects simulation corpora supporting the requested interpreter.
    pub fn simulation(&self, lang: Language) -> Vec<&SimSuite> {
        self.suites
            .iter()
            .filter_map(|suite| match suite {
                Suite::Sim(suite) if suite.languages.contains(&lang) => Some(suite),
                _ => None,
            })
            .collect()
    }

    /// Prints every suite's sources and expectations without executing it.
    pub fn list(&self) {
        println!("specification: {:?}", self.spec);
        println!("includes: {:?}", self.includes);
        println!("static exclusions: {:?}", self.excludes_static);
        println!("dynamic exclusions: {:?}", self.excludes_dynamic);
        for suite in &self.suites {
            println!("{}", suite.name());
            match suite {
                Suite::Snapshot(suite) => println!(
                    "  snapshot {:?}: {:?} -> {}",
                    suite.stage,
                    suite.inputs,
                    suite.expected.display()
                ),
                Suite::P4parse(suite) => {
                    println!("  p4parse {:?} -> {}", suite.roots, suite.expected.display())
                }
                Suite::Run(suite) => println!(
                    "  run {:?} {} {} -> {}",
                    suite.languages,
                    suite.root.display(),
                    suite.relation,
                    suite.expected.display()
                ),
                Suite::Sim(suite) => println!(
                    "  sim {:?} {} {} / {} patches={:?} -> {}",
                    suite.languages,
                    suite.arch,
                    suite.p4.display(),
                    suite.stf.display(),
                    suite.patches,
                    suite.expected.display()
                ),
                Suite::Negative { stage, cases, .. } => {
                    println!("  {}: {} cases", stage.name(), cases.len());
                    for case in cases {
                        println!(
                            "    {}: {} -> {}",
                            case.name,
                            case.input.display(),
                            case.expected.display()
                        );
                        if !case.inputs.is_empty() {
                            println!("      inputs: {:?}", case.inputs);
                        }
                        if !case.args.is_empty() {
                            println!("      args: {:?}", case.args);
                        }
                        if let Some(code) = &case.code {
                            println!("      diagnostic: {code}");
                        }
                        if case.allow_success {
                            println!("      allows success: reference control");
                        }
                    }
                }
            }
        }
    }
}
