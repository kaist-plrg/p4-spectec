//! Registered negative diagnostic acceptance
//!
//! Module registrations supply source fixtures and expectations,
//! relative to the registration's `root`.
//! CLI argument files contain their complete invocation and input paths.
//! Reports render from a stable fixture directory before full-text comparison.
//! Each stage rejects failures from preceding passes as setup errors.

mod cli;
mod syntax;

mod parse;

mod elab;

mod algo;

mod structure;

mod prose;

mod specdoc;
mod splice;

mod interp;

mod sim;

use std::path::{Path, PathBuf};

use clap::ValueEnum;
use expect_test::expect_file;
use indicatif::ProgressBar;
use serde::Deserialize;

use p4spectec::diagnostic::{DisplayStyle, RenderConfig, Renderer, Report, ReportKind};

use crate::harness::{Error, Result, interpreter::Language};

fn failure(name: &str, message: impl std::fmt::Display) -> Error {
    Error::from(format!("{name}: {message}"))
}

/// Selects a diagnostic module from the driver's command line.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub enum Suite {
    Parse,
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

type RunCase = fn(&Case) -> Result<Vec<Report>>;

/// Lists each registration in run order with its selector and source runner,
/// and whether reports render runtime frames in the short style.
///
/// Modules without a source runner register only `.args` command cases.
const MODULES: [(Suite, &str, Option<RunCase>, bool); 11] = [
    (Suite::Parse, "parse", Some(parse::run_case), false),
    (Suite::Elab, "elab", Some(elab::run_case), false),
    (Suite::Algo, "algo", Some(algo::run_case), false),
    (Suite::Structure, "structure", Some(structure::run_case), false),
    (Suite::Prose, "prose", Some(prose::run_case), false),
    (Suite::Specdoc, "specdoc", Some(specdoc::run_case), false),
    (Suite::Specdoc, "splice", Some(splice::run_case), false),
    (Suite::Interp, "interp", Some(interp::run_case), true),
    (Suite::Command, "command", None, false),
    (Suite::Run, "run", None, false),
    (Suite::Sim, "sim", Some(sim::run_case), true),
];

/// Prints each module's registered cases.
pub fn list() -> Result<()> {
    for (_, name, _, _) in MODULES {
        println!("diagnostics/{name}: {:#?}", load(name)?);
    }
    Ok(())
}

/// Runs the selected module or all modules in registration order.
pub fn run(suite: Option<Suite>, path_cli: Option<&Path>) -> Result<()> {
    for (suite_module, name, run_case, frame_short) in MODULES {
        if suite.is_some_and(|suite| suite != suite_module) {
            continue;
        }
        let config = RenderConfig {
            frame_style: frame_short.then_some(DisplayStyle::Short),
            ..Default::default()
        };
        let registration = load(name)?;
        run_registered(name, &registration, path_cli, run_case, config)?;
    }
    Ok(())
}

/// Selects the expected result independently of diagnostic severity.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Outcome {
    Success,
    #[default]
    Failure,
}

/// Supplies fixture paths relative to the fixture directory, `testdata/spec`.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Case {
    pub name: String,
    pub input: PathBuf,
    pub expected: PathBuf,
    #[serde(default)]
    pub outcome: Outcome,
    #[serde(default)]
    pub code: Option<String>,
    #[serde(default)]
    pub inputs: Vec<PathBuf>,
    /// Expands the case into one case per language, each with `language` set.
    #[serde(default)]
    pub languages: Vec<Language>,
    #[serde(skip)]
    pub language: Option<Language>,
    #[serde(default)]
    pub relation: Option<String>,
    #[serde(default)]
    pub nats: Vec<u64>,
    #[serde(default)]
    pub kind: Option<interp::FailureKind>,
    #[serde(default)]
    pub det: bool,
    #[serde(default)]
    pub without_rule_groups: bool,
    #[serde(default)]
    pub arch: Option<String>,
}

/// Lists the cases of one diagnostic registration file and their fixture root.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Registration {
    root: PathBuf,
    cases: Vec<Case>,
}

/// Loads the cases registered in `suites/diagnostic/<name>.toml`.
fn load(name: &str) -> Result<Registration> {
    let registration: Registration = crate::harness::load(&format!("diagnostic/{name}"))?;
    let mut cases = Vec::new();
    for case in registration.cases {
        // Expand a case listing languages into one case per language
        if case.languages.is_empty() {
            cases.push(case);
            continue;
        }
        for &language in &case.languages {
            let name = format!("{}-{}", case.name, language.name());
            cases.push(Case { name, language: Some(language), ..case.clone() });
        }
    }
    Ok(Registration { root: registration.root, cases })
}

impl Case {
    /// Checks the operation result against its registered expectation.
    fn check_outcome(&self, success: bool) -> Result<()> {
        let outcome = if success { Outcome::Success } else { Outcome::Failure };
        if outcome != self.outcome {
            return Err(failure(
                &self.name,
                format!("expected {:?}, got {outcome:?}", self.outcome),
            ));
        }
        Ok(())
    }

    /// Selects subprocess acceptance for an exact argument-file input.
    pub fn uses_cli(&self) -> bool {
        self.input.extension().is_some_and(|ext| ext == "args")
    }
}

/// Collects diagnostic codes from a report and its nested causes.
fn codes(report: &Report) -> Vec<&str> {
    let mut codes = Vec::new();
    let mut reports_pending = vec![report];
    while let Some(report) = reports_pending.pop() {
        // Inspect causes without depending on rendered text
        if let ReportKind::Cause(diagnostic) = &report.kind
            && let Some(code) = diagnostic.code.as_deref()
        {
            codes.push(code);
        }
        // Include causes nested under operation frames
        reports_pending.extend(&report.children);
    }
    codes
}

/// Restores the caller's working directory after diagnostic rendering.
struct Directory(PathBuf);

impl Drop for Directory {
    fn drop(&mut self) {
        let _ = std::env::set_current_dir(&self.0);
    }
}

/// Compares module-owned fixture output with its registered expectations.
fn run_registered(
    name: &str,
    registration: &Registration,
    path_cli: Option<&Path>,
    run_case: Option<RunCase>,
    config: RenderConfig,
) -> Result<()> {
    // Resolve subprocess paths before entering the stable fixture directory
    let path_cli = path_cli.map(std::path::absolute).transpose()?;
    let cases = &registration.cases;
    if cases.iter().any(Case::uses_cli) && path_cli.is_none() {
        return Err(failure(name, "--cli is required for argument-file acceptance"));
    }
    let _directory = Directory(std::env::current_dir()?);
    // Render from the fixture root so reports show stable relative paths
    let path_fixture = std::path::absolute(&registration.root)?;
    std::env::set_current_dir(&path_fixture)?;

    // Preserve case order from this module's registration file
    let progress = ProgressBar::new(cases.len() as u64);
    for case in cases {
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
        let path = path_fixture.join(&case.expected);
        expect_file![path].assert_eq(&text);
        progress.inc(1);
    }
    progress.finish_and_clear();
    eprintln!("diagnostics/{name}: {} cases passed", cases.len());
    Ok(())
}
