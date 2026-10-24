//! Execution entry point for the AL metacircular specification

use clap::{Parser, ValueEnum};
use p4spectec::metacirc::{TowerConfig, config::Lang};
use p4spectec_test::{
    Error, ExecutionOptions, Result, metacirc,
    suite::{self, Index},
};
use std::{path::PathBuf, process::ExitCode};

/// Selects suites by the input language of their tower's top level.
#[derive(Clone, Copy, PartialEq, Eq, ValueEnum)]
enum Input {
    /// AL scripts
    Al,
    /// P4 programs
    P4,
}

/// Compares P4 corpus and AL script outcomes through metacircular towers.
#[derive(Parser)]
struct Cli {
    /// Suite registration file, relative to the invoking directory.
    #[arg(long = "registry")]
    path_registry: Option<PathBuf>,
    /// Run only the registered suite with this name.
    #[arg(long, value_name = "NAME")]
    suite: Option<String>,
    /// Run only the suites whose tower takes this input.
    #[arg(long, value_enum)]
    input: Option<Input>,
    #[command(flatten)]
    options: ExecutionOptions,
}

/// Resolves caller paths and runs the selected suites from the repository root.
fn execute(cli: Cli) -> Result<()> {
    // Refuse baseline promotion before discovering or executing any inputs
    if std::env::var_os("UPDATE_EXPECT").is_some() {
        return Err(Error::Invalid("UPDATE_EXPECT is not supported for metacirc execution".into()));
    }
    let path_registry = cli
        .path_registry
        .as_deref()
        .map(std::path::absolute)
        .transpose()?
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("suites.json"));
    // Resolve caller paths before selecting the corpus repository as working directory
    std::env::set_current_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../.."))?;
    let index = Index::load(&path_registry)?;
    let mut config = suite::load_metacirc(&index.suites.metacirc)?;
    // Restrict the registered suites to the selected name
    if let Some(name) = &cli.suite {
        config.suites.retain(|suite| &suite.name == name);
        if config.suites.is_empty() {
            return Err(Error::Invalid(format!("unknown metacirc suite {name}")));
        }
    }
    // Restrict the registered suites to the selected input language
    if let Some(input) = cli.input {
        let mut suites = Vec::new();
        for suite in config.suites {
            let config_tower = TowerConfig::from_file(&suite.tower)
                .map_err(|error| Error::Invalid(format!("{}: {error}", suite.name)))?;
            let input_suite = match config_tower.top_lang() {
                Lang::Al => Input::Al,
                Lang::P4 => Input::P4,
            };
            if input_suite == input {
                suites.push(suite);
            }
        }
        if suites.is_empty() {
            return Err(Error::Invalid("no selected metacirc suite takes this input".into()));
        }
        config.suites = suites;
    }
    metacirc::run(&config, cli.options)
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    let result = std::thread::Builder::new()
        .name("metacirc-driver".to_owned())
        .stack_size(64 * 1024 * 1024)
        .spawn(move || execute(cli));
    match result {
        Ok(thread) => match thread.join() {
            Ok(Ok(())) => ExitCode::SUCCESS,
            Ok(Err(error)) => {
                eprintln!("{error}");
                ExitCode::FAILURE
            }
            Err(_) => {
                eprintln!("test driver panicked; expected validation failed");
                ExitCode::FAILURE
            }
        },
        Err(error) => {
            eprintln!("cannot start test driver: {error}");
            ExitCode::FAILURE
        }
    }
}
