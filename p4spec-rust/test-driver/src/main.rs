mod corpus;
mod snapshot;
mod suite;

mod elab;

mod algo;

mod structure;

mod prose;

mod adoc;

mod p4parse;
mod run;
mod sim;

mod diagnostic;

use std::{path::PathBuf, process::ExitCode};

use clap::{Args, Parser, Subcommand};

use suite::{Index, Language};

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
    /// Compare P4 parse/unparse/parse roundtrips with stored file results
    P4parse,
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
    /// Compare diagnostic output with stored snapshots
    Diagnostics {
        #[arg(long, value_enum)]
        suite: Option<diagnostic::Suite>,
        /// Product executable used by registered argument-file inputs
        #[arg(long = "cli", value_name = "PATH")]
        path_cli: Option<PathBuf>,
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
        Command::Structure | Command::P4parse | Command::Run { .. } | Command::Sim { .. }
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
    let index = Index::load(&path_registry)?;
    match command {
        Command::List => index.list(),
        Command::Elab => elab::run(&suite::load_snapshots(&index.suites.elab)?),
        Command::Algo => algo::run(&suite::load_snapshots(&index.suites.algo)?),
        Command::Structure => structure::run(&suite::load_structure(&index.suites.structure)?),
        Command::Prose => prose::run(&suite::load_snapshots(&index.suites.prose)?),
        Command::Adoc { path_output } => {
            adoc::run(&suite::load_adoc(&index.suites.adoc)?, path_output.as_deref())
        }
        Command::P4parse => p4parse::run(&suite::load_parsing(&index.suites.p4parse)?),
        Command::Run { language, options } => {
            run::run(&suite::load_execution(&index.suites.run)?, *language, *options)
        }
        Command::Sim { language, options } => {
            sim::run(&suite::load_simulation(&index.suites.sim)?, *language, *options)
        }
        Command::Diagnostics { suite, path_cli } => {
            diagnostic::run(&index.suites.diagnostics, *suite, path_cli.as_deref())
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
