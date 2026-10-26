//! Execution entry point for the AL metacircular specification

use clap::{Parser, Subcommand, ValueEnum};
use p4spectec::metacirc::{TowerConfig, config::Lang};
use p4spectec_test::{
    Error, ExecutionOptions, Result, algo, elab,
    metacirc::{
        self, metaparse,
        suite::{Conversion, RunConfig},
    },
    prose, structure,
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

/// Selects an execution through towers,
/// or a snapshot or codec check of the meta specification.
#[derive(Clone, Subcommand)]
enum Command {
    /// Compare registered P4 corpus and script outcomes through towers
    Run {
        /// Run only the registered suite with this name.
        #[arg(long, value_name = "NAME")]
        suite: Option<String>,
        /// Run only the suites whose tower takes this input.
        #[arg(long, value_enum)]
        input: Option<Input>,
        /// Convert IL to AL natively or by the meta specification's `Algo_IL`.
        #[arg(long = "algo", value_enum, default_value_t)]
        conversion: Conversion,
        #[command(flatten)]
        options: ExecutionOptions,
    },
    /// Compare registered simulation suites through a tower with native expectations
    Sim {
        /// Convert IL to AL natively or by the meta specification's `Algo_IL`.
        #[arg(long = "algo", value_enum, default_value_t)]
        conversion: Conversion,
        #[command(flatten)]
        options: ExecutionOptions,
    },
    /// Compare the elaborated meta specification with expected output
    Elab,
    /// Compare the algorithmic meta specification with expected output
    Algo,
    /// Compare the structured meta specification in both rule-group modes
    Structure,
    /// Compare the prose meta specification with expected output
    Prose,
    /// Check that IL and AL encodings are fixed points of decode-then-encode
    Metaparse,
    /// List all metacirc registrations, inputs, and expectation files
    List,
}

/// Compares P4 corpus and AL script outcomes through metacircular towers.
#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
    /// Suite registration file, relative to the invoking directory.
    #[arg(long = "registry", global = true)]
    path_registry: Option<PathBuf>,
}

/// Resolves caller paths and runs the selected command from the repository root.
fn execute(cli: Cli) -> Result<()> {
    // Refuse baseline promotion before discovering or executing any inputs
    if !matches!(cli.command, Command::Elab | Command::Algo | Command::Prose)
        && std::env::var_os("UPDATE_EXPECT").is_some()
    {
        return Err(Error::Invalid(
            "UPDATE_EXPECT is supported only for metacirc elab, algo, and prose".into(),
        ));
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
    // The main index names the metacirc index, which names each metacirc module
    let index_metacirc = metacirc::suite::Index::load(&index.suites.metacirc)?;
    let modules = &index_metacirc.suites;
    match cli.command {
        Command::Run { suite, input, conversion, options } => {
            let config = metacirc::suite::load_execution(&modules.run)?;
            run(config, suite, input, conversion, options)
        }
        Command::Sim { conversion, options } => {
            let config_sim = suite::load_simulation(&index.suites.sim)?;
            let config = metacirc::suite::load_simulation(&modules.sim)?;
            metacirc::sim::run(&config_sim, &config, conversion, options)
        }
        // Snapshot and codec checks compile the meta specification without executing it
        Command::Elab => elab::run(&suite::load_snapshots(&modules.elab)?),
        Command::Algo => algo::run(&suite::load_snapshots(&modules.algo)?),
        Command::Structure => structure::run(&suite::load_structure(&modules.structure)?),
        Command::Prose => prose::run(&suite::load_snapshots(&modules.prose)?),
        Command::Metaparse => metaparse::run(&metacirc::suite::load_metaparse(&modules.metaparse)?),
        Command::List => index_metacirc.list(),
    }
}

/// Runs the corpus suites of `conversion`, restricted by name and input language.
fn run(
    mut config: RunConfig,
    name: Option<String>,
    input: Option<Input>,
    conversion: Conversion,
    options: ExecutionOptions,
) -> Result<()> {
    // Restrict the registered suites to the selected name
    if let Some(name) = &name {
        config.suites.retain(|suite| &suite.name == name);
        if config.suites.is_empty() {
            return Err(Error::Invalid(format!("unknown metacirc suite {name}")));
        }
    }
    // Restrict the registered suites to the selected input language
    if let Some(input) = input {
        let mut suites = Vec::new();
        for suite in config.suites {
            // A suite without a tower for the conversion takes no input
            let Some(path_tower) = suite.towers.get(conversion) else {
                continue;
            };
            let config_tower = TowerConfig::from_file(path_tower)
                .map_err(|error| Error::Invalid(format!("{}: {error}", suite.name)))?;
            let input_suite = match config_tower.top_lang() {
                Lang::Al | Lang::Il => Input::Al,
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
    metacirc::run::run(&config, conversion, options)
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
