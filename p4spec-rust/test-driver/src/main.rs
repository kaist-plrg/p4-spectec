mod adoc;
mod algo;
mod corpus;
mod diagnostic;
mod elab;
mod p4parse;
mod prose;
mod run;
mod sim;
mod snapshot;
mod structure;
mod suite;

use std::{path::PathBuf, process::ExitCode};

use clap::{Args, Parser, Subcommand};

use suite::{Config, Language};

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Invalid(String),
}
type Result<T> = std::result::Result<T, Error>;

/// Source and expected-file acceptance for the Rust implementation
#[derive(Parser)]
struct Cli {
    /// Suite registration file, relative to the invoking directory
    #[arg(long = "registry", global = true)]
    path_registry: Option<PathBuf>,
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// List all registered suites, inputs, and expectation files
    List,
    /// Compare diagnostic output with stored snapshots
    Diagnostics {
        #[arg(long, value_enum)]
        suite: Option<diagnostic::Suite>,
        /// Product executable used by registered argument-file inputs
        #[arg(long = "cli", value_name = "PATH")]
        path_cli: Option<PathBuf>,
    },
    /// Compare P4 parse/unparse/parse roundtrips with stored file results
    P4parse,
    /// Compare the elaborated P4 specification with expected output
    Elab,
    /// Compare the algorithmic P4 specification with expected output
    Algo,
    /// Compare the structured P4 specification in both rule-group modes
    Structure,
    /// Compare the rendered prose specification with expected output
    Prose,
    /// Compare AsciiDoc snapshots and check anchor determinism
    Adoc {
        /// Save EL and PL documents in this directory for external validation
        #[arg(long = "output")]
        path_output: Option<PathBuf>,
    },
    /// Compare execution outcomes with stored file results
    Run {
        #[command(subcommand)]
        language: Language,
        #[command(flatten)]
        options: ExecutionOptions,
    },
    /// Compare simulation outcomes and matched packets
    Sim {
        #[command(subcommand)]
        language: Language,
        #[command(flatten)]
        options: ExecutionOptions,
    },
}

/// Configures interpreter execution consistently for run and sim.
#[derive(Clone, Copy, Args)]
struct ExecutionOptions {
    /// Reject nondeterministic candidate selection
    #[arg(long, global = true)]
    det: bool,
    /// Enable or disable interpreter caching
    #[arg(long, global = true, default_value_t = true, action = clap::ArgAction::Set)]
    cache_on: bool,
}

fn execute(mut cli: Cli) -> Result<()> {
    let command = &mut cli.command;
    if matches!(
        command,
        Command::P4parse | Command::Structure | Command::Run { .. } | Command::Sim { .. }
    ) && std::env::var_os("UPDATE_EXPECT").is_some()
    {
        return Err(Error::Invalid(
            "UPDATE_EXPECT is supported only for elab, algo, prose, adoc, and diagnostics"
                .to_owned(),
        ));
    }
    // Resolve output paths before changing to the specification repository
    if let Command::Adoc { path_output: Some(path_output) } = &mut *command {
        *path_output = std::path::absolute(&*path_output)?;
    }
    if let Command::Diagnostics { path_cli: Some(path_cli), .. } = &mut *command {
        *path_cli = std::path::absolute(&*path_cli)?;
    }
    let path_registry = cli
        .path_registry
        .as_deref()
        .map(std::path::absolute)
        .transpose()?
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("suites.json"));
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()?;
    std::env::set_current_dir(&root)?;
    let config = Config::load(&path_registry)?;
    match command {
        Command::List => config.list(),
        Command::Diagnostics { suite, path_cli } => {
            let groups = diagnostic::load(&config.suites.diagnostics)?;
            let suites: Vec<_> = groups
                .iter()
                .filter(|group| suite.is_none_or(|selected| selected == group.stage))
                .map(|group| (group.stage, &group.cases))
                .collect();
            if suites.is_empty() {
                return Err(Error::Invalid("no diagnostic suites selected".into()));
            }
            if path_cli.is_none()
                && suites
                    .iter()
                    .any(|(_, cases)| cases.iter().any(diagnostic::Case::uses_cli))
            {
                return Err(Error::Invalid(
                    "--cli is required for argument-file diagnostic acceptance".into(),
                ));
            }
            for (stage, cases) in suites {
                diagnostic::run_registered(stage, cases, path_cli.as_deref())?;
            }
            Ok(())
        }
        Command::P4parse => p4parse::run(&config, &suite::load_parsing(&config.suites.p4parse)?),
        Command::Elab => elab::run(&suite::load_snapshots(&config.suites.elab)?),
        Command::Algo => algo::run(&suite::load_snapshots(&config.suites.algo)?),
        Command::Structure => structure::run(&suite::load_structure(&config.suites.structure)?),
        Command::Prose => prose::run(&suite::load_snapshots(&config.suites.prose)?),
        Command::Adoc { path_output } => {
            adoc::run(&suite::load_adoc(&config.suites.adoc)?, path_output.as_deref())
        }
        Command::Run { language, options } => {
            run::run(&config, &suite::load_execution(&config.suites.run)?, *language, *options)
        }
        Command::Sim { language, options } => {
            sim::run(&config, &suite::load_simulation(&config.suites.sim)?, *language, *options)
        }
    }
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    let result = std::thread::Builder::new()
        .name("expected-driver".to_owned())
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
