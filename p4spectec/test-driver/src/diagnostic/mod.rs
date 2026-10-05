//! Registered negative diagnostic acceptance
//!
//! Module registrations supply source fixtures and expectations.
//! CLI argument files contain their complete invocation and input paths.
//! Reports render from a stable fixture directory before full-text comparison.
//! Each stage rejects failures from preceding passes as setup errors.

mod cli;
mod syntax;

mod frontend;

mod elab;

mod algo;

mod structure;

mod prose;

mod specdoc;
mod splice;

mod interp;

mod command;
mod run;
mod sim;

use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
};

use clap::ValueEnum;
use expect_test::expect_file;
use indicatif::ProgressBar;
use serde::Deserialize;

use p4spectec::diagnostic::{RenderConfig, Renderer, Report};

use crate::{Error, Result};

fn failure(name: &str, message: impl std::fmt::Display) -> Error {
    Error::Invalid(format!("{name}: {message}"))
}

/// Selects a diagnostic module from the driver's command line.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub enum Suite {
    #[value(alias = "parse")]
    Frontend,
    Elab,
    Algo,
    Structure,
    Prose,
    Specdoc,
    Interp,
    Command,
    Run,
    Sim,
}

/// Names diagnostic registration files by their owning Rust modules.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Modules {
    pub frontend: PathBuf,
    pub elab: PathBuf,
    pub algo: PathBuf,
    pub structure: PathBuf,
    pub prose: PathBuf,
    pub specdoc: PathBuf,
    pub splice: PathBuf,
    pub interp: PathBuf,
    pub command: PathBuf,
    pub run: PathBuf,
    pub sim: PathBuf,
}

impl Modules {
    /// Returns module filenames for resolution relative to the root index.
    pub fn paths_mut(&mut self) -> [&mut PathBuf; 11] {
        [
            &mut self.frontend,
            &mut self.elab,
            &mut self.algo,
            &mut self.structure,
            &mut self.prose,
            &mut self.specdoc,
            &mut self.splice,
            &mut self.interp,
            &mut self.command,
            &mut self.run,
            &mut self.sim,
        ]
    }

    /// Prints each module through the same validation used for execution.
    pub fn list(&self) -> Result<()> {
        println!("diagnostics/frontend: {:#?}", load(&self.frontend, false)?);
        println!("diagnostics/elab: {:#?}", load(&self.elab, false)?);
        println!("diagnostics/algo: {:#?}", load(&self.algo, false)?);
        println!("diagnostics/structure: {:#?}", load(&self.structure, false)?);
        println!("diagnostics/prose: {:#?}", load(&self.prose, false)?);
        println!("diagnostics/specdoc: {:#?}", load(&self.specdoc, false)?);
        println!("diagnostics/splice: {:#?}", load(&self.splice, false)?);
        println!("diagnostics/interp: {:#?}", load(&self.interp, false)?);
        println!("diagnostics/command: {:#?}", load(&self.command, true)?);
        println!("diagnostics/run: {:#?}", load(&self.run, true)?);
        println!("diagnostics/sim: {:#?}", load(&self.sim, false)?);
        Ok(())
    }
}

/// Runs the selected module or all modules in registration order.
pub fn run(modules: &Modules, suite: Option<Suite>, path_cli: Option<&Path>) -> Result<()> {
    // The CLI selector chooses modules without inspecting their registrations
    if suite.is_none_or(|suite| suite == Suite::Frontend) {
        frontend::run(&modules.frontend, path_cli)?;
    }
    if suite.is_none_or(|suite| suite == Suite::Elab) {
        elab::run(&modules.elab, path_cli)?;
    }
    if suite.is_none_or(|suite| suite == Suite::Algo) {
        algo::run(&modules.algo, path_cli)?;
    }
    if suite.is_none_or(|suite| suite == Suite::Structure) {
        structure::run(&modules.structure, path_cli)?;
    }
    if suite.is_none_or(|suite| suite == Suite::Prose) {
        prose::run(&modules.prose, path_cli)?;
    }
    // Specdoc owns both rendered document and skeleton splicing diagnostics
    if suite.is_none_or(|suite| suite == Suite::Specdoc) {
        specdoc::run(&modules.specdoc, path_cli)?;
    }
    if suite.is_none_or(|suite| suite == Suite::Specdoc) {
        splice::run(&modules.splice, path_cli)?;
    }
    if suite.is_none_or(|suite| suite == Suite::Interp) {
        interp::run(&modules.interp, path_cli)?;
    }
    if suite.is_none_or(|suite| suite == Suite::Command) {
        command::run(&modules.command, path_cli)?;
    }
    if suite.is_none_or(|suite| suite == Suite::Run) {
        run::run(&modules.run, path_cli)?;
    }
    if suite.is_none_or(|suite| suite == Suite::Sim) {
        sim::run(&modules.sim, path_cli)?;
    }
    Ok(())
}

/// Supplies fixture paths relative to the test-driver manifest directory.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Case {
    pub name: String,
    pub input: PathBuf,
    pub expected: PathBuf,
    #[serde(default)]
    pub allow_success: bool,
    #[serde(default)]
    pub code: Option<String>,
    #[serde(default)]
    pub inputs: Vec<PathBuf>,
    #[serde(default)]
    pub args: Vec<String>,
}

/// Registers diagnostics separately from transformation and corpus tests.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Group {
    pub name: String,
    pub cases: Vec<Case>,
}

/// Loads one module and validates its source/expectation pairs.
fn load(path: &Path, cli_only: bool) -> Result<Vec<Group>> {
    let groups: Vec<Group> = crate::suite::load(path)?;
    let mut names = BTreeSet::new();
    if groups.is_empty() {
        return Err(failure("diagnostics", "no groups registered"));
    }
    // Validate every registered pair before running this module's diagnostics
    for group in &groups {
        if group.name.is_empty() || !names.insert(&group.name) {
            return Err(failure(&group.name, "empty or duplicate diagnostic group"));
        }
        if group.cases.is_empty() {
            return Err(failure(&group.name, "no negative cases"));
        }
        let mut names_case = BTreeSet::new();
        for case in &group.cases {
            if case.name.is_empty() || !names_case.insert(&case.name) {
                return Err(failure(&group.name, "empty or duplicate case"));
            }
            // Complete invocations belong in the argument file
            if case.uses_cli() && (!case.inputs.is_empty() || !case.args.is_empty()) {
                return Err(failure(&case.name, "arguments and inputs belong in the .args file"));
            }
            if cli_only && !case.uses_cli() {
                return Err(failure(&case.name, "requires an .args input"));
            }
            for path in [&case.input, &case.expected] {
                if !crate::suite::expected_path(path).is_file() {
                    return Err(failure(&case.name, format!("missing file {}", path.display())));
                }
            }
            // API fixtures may declare additional source inputs
            for path in &case.inputs {
                if !crate::suite::expected_path(path).exists() {
                    return Err(failure(&case.name, format!("missing input {}", path.display())));
                }
            }
        }
    }
    Ok(groups)
}

impl Case {
    /// Selects subprocess acceptance for an exact argument-file input.
    pub fn uses_cli(&self) -> bool {
        self.input.extension().is_some_and(|ext| ext == "args")
    }

    /// Resolves an input from the diagnostic fixture working directory.
    fn path_input(&self) -> PathBuf {
        fixture_path(&self.input)
    }

    /// Resolves all auxiliary sources from the fixture working directory.
    fn paths_input(&self) -> Vec<PathBuf> {
        self.inputs.iter().map(|path| fixture_path(path)).collect()
    }
}

/// Keeps source identities stable across checkout locations.
fn fixture_path(path: &Path) -> PathBuf {
    if let Ok(path) = path.strip_prefix("expected/diagnostic") {
        path.to_owned()
    } else {
        Path::new("../..").join(path)
    }
}

/// Restores the caller's working directory after diagnostic rendering.
struct Directory(PathBuf);

impl Drop for Directory {
    fn drop(&mut self) {
        let _ = std::env::set_current_dir(&self.0);
    }
}

type RunCase = fn(&Case) -> Result<Vec<Report>>;

/// Compares module-owned fixture output with its registered expectations.
fn run_registered(
    name: &str,
    groups: &[Group],
    path_cli: Option<&Path>,
    run_case: Option<RunCase>,
    config: RenderConfig,
) -> Result<()> {
    // Resolve subprocess paths before entering the stable fixture directory
    let path_cli = path_cli.map(std::path::absolute).transpose()?;
    if groups
        .iter()
        .any(|group| group.cases.iter().any(Case::uses_cli))
        && path_cli.is_none()
    {
        return Err(failure(name, "--cli is required for argument-file acceptance"));
    }
    let _directory = Directory(std::env::current_dir()?);
    let path_manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    std::env::set_current_dir(path_manifest.join("expected/diagnostic"))?;

    // Preserve group and case order from this module's registration file
    for group in groups {
        let progress = ProgressBar::new(group.cases.len() as u64);
        for case in &group.cases {
            let text = if case.uses_cli() {
                // Argument files compare product stderr verbatim
                cli::run(path_cli.as_deref().expect("CLI admission checked"), case)?
            } else {
                // The owning module supplies its source runner and report style
                let reports = run_case.expect("source runner supplied by owning module")(case)?;
                let mut text = String::new();
                for report in reports {
                    let rendered = Renderer::new(config.clone())
                        .render_to_string(&report)
                        .map_err(|error| failure(&case.name, error))?;
                    text.push_str(&rendered);
                }
                text
            };
            // Every comparison uses the explicit registered expectation
            let path = path_manifest.join(&case.expected);
            expect_file![path].assert_eq(&text);
            progress.inc(1);
        }
        progress.finish_and_clear();
        eprintln!("diagnostics/{name}: {} cases passed", group.cases.len());
    }
    Ok(())
}
