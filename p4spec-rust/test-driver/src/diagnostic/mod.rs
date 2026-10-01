//! Registered negative diagnostic acceptance
//!
//! Module registrations supply source fixtures and expectations.
//! CLI argument files contain their complete invocation and input paths.
//! Reports render from a stable fixture directory before full-text comparison.
//! Each stage rejects failures from preceding passes as setup errors.

mod algo;
mod cli;
mod elab;
mod interp;
mod parse;
mod prose;
mod sim;
mod specdoc;
mod splice;
mod structure;
mod syntax;

use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
};

use clap::ValueEnum;
use expect_test::expect_file;
use indicatif::ProgressBar;
use serde::Deserialize;

use p4spec_rust::diagnostic::{DisplayStyle, RenderConfig, Renderer, Report};

use crate::{Error, Result};

fn failure(name: &str, message: impl std::fmt::Display) -> Error {
    Error::Invalid(format!("{name}: {message}"))
}

/// Selects the product stage exercised by a registered fixture.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq, ValueEnum)]
#[serde(rename_all = "kebab-case")]
pub enum Suite {
    #[serde(alias = "parse")]
    #[value(alias = "parse")]
    Frontend,
    Elab,
    Algo,
    Structure,
    Prose,
    Interp,
    Specdoc,
    Command,
    Run,
    Sim,
}

impl Suite {
    /// Returns the registry's stage name.
    pub fn name(self) -> &'static str {
        match self {
            Self::Frontend => "frontend",
            Self::Elab => "elab",
            Self::Algo => "algo",
            Self::Structure => "structure",
            Self::Prose => "prose",
            Self::Interp => "interp",
            Self::Specdoc => "specdoc",
            Self::Command => "command",
            Self::Run => "run",
            Self::Sim => "sim",
        }
    }
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
    pub stage: Suite,
    pub cases: Vec<Case>,
}

/// Loads diagnostic modules and validates their source/expectation pairs.
pub fn load(paths: &[PathBuf]) -> Result<Vec<Group>> {
    let mut groups = Vec::new();
    let mut names = BTreeSet::new();
    // Preserve module and case order within diagnostic acceptance
    for path in paths {
        let groups_module: Vec<Group> = crate::suite::load(path)?;
        for group in groups_module {
            if group.name.is_empty() || !names.insert(group.name.clone()) {
                return Err(failure(&group.name, "empty or duplicate diagnostic group"));
            }
            if group.cases.is_empty() {
                return Err(failure(&group.name, "no negative cases"));
            }
            let mut names_case = BTreeSet::new();
            // Validate every registered pair before running any diagnostic
            for case in &group.cases {
                if case.name.is_empty() || !names_case.insert(&case.name) {
                    return Err(failure(&group.name, "empty or duplicate case"));
                }
                if case.uses_cli() && (!case.inputs.is_empty() || !case.args.is_empty()) {
                    return Err(failure(
                        &case.name,
                        "arguments and inputs belong in the .args file",
                    ));
                }
                if matches!(group.stage, Suite::Command | Suite::Run) && !case.uses_cli() {
                    return Err(failure(&case.name, "requires an .args input"));
                }
                for path in [&case.input, &case.expected] {
                    if !crate::suite::expected_path(path).is_file() {
                        return Err(failure(
                            &case.name,
                            format!("missing file {}", path.display()),
                        ));
                    }
                }
                for path in &case.inputs {
                    if !crate::suite::expected_path(path).exists() {
                        return Err(failure(
                            &case.name,
                            format!("missing input {}", path.display()),
                        ));
                    }
                }
            }
            groups.push(group);
        }
    }
    if groups.is_empty() {
        return Err(failure("diagnostics", "no groups registered"));
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

/// Executes exactly the registered inputs and compares their full output.
pub fn run_registered(stage: Suite, cases: &[Case], path_cli: Option<&Path>) -> Result<()> {
    // Resolve subprocess paths before entering the stable fixture directory
    let path_cli = path_cli.map(std::path::absolute).transpose()?;
    if cases.iter().any(Case::uses_cli) && path_cli.is_none() {
        return Err(failure(stage.name(), "--cli is required for argument-file acceptance"));
    }
    let _directory = Directory(std::env::current_dir()?);
    let path_manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    std::env::set_current_dir(path_manifest.join("expected/diagnostic"))?;
    let progress = ProgressBar::new(cases.len() as u64);

    // Match CLI frame presentation for runtime and simulation failures
    let config = RenderConfig {
        frame_style: matches!(stage, Suite::Interp | Suite::Sim).then_some(DisplayStyle::Short),
        ..Default::default()
    };
    for case in cases {
        // Argument-file expectations compare stderr independently of the stage
        let text = if case.uses_cli() {
            cli::run(path_cli.as_deref().expect("CLI admission checked"), case)?
        } else {
            let reports = run_case(stage, case)?;
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
    eprintln!("diagnostics/{}: {} cases passed", stage.name(), cases.len());
    Ok(())
}

fn run_case(stage: Suite, case: &Case) -> Result<Vec<Report>> {
    match stage {
        Suite::Frontend => parse::run(case),
        Suite::Elab => elab::run(case),
        Suite::Algo => algo::run(case),
        Suite::Structure => structure::run(case),
        Suite::Prose => prose::run(case),
        Suite::Interp if case.input.extension().is_some_and(|ext| ext == "p4") => syntax::run(case),
        Suite::Interp => interp::run(case),
        Suite::Specdoc if case.input.extension().is_some_and(|ext| ext == "adoc") => {
            splice::run(case)
        }
        Suite::Specdoc => specdoc::run(case),
        Suite::Sim => sim::run(case),
        Suite::Command | Suite::Run => unreachable!("argument-file stages use subprocess output"),
    }
}
