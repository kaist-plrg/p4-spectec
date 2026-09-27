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

use clap::{Parser, Subcommand};
use std::{path::PathBuf, process::ExitCode};

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Invalid(String),
}
type Result<T> = std::result::Result<T, Error>;

/// Native expected tests, independent of the product's unit tests
#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compare diagnostic output with stored snapshots
    Diagnostics {
        #[arg(long, value_enum)]
        suite: Option<diagnostic::Suite>,
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
    /// Compare the full P4 corpus with stored file results (cache on, det off)
    RunAl,
    /// Compare native SL outcomes with source-derived results (cache on)
    RunSl {
        #[arg(long)]
        det: bool,
    },
    /// Compare native PL outcomes with source-derived results (cache on)
    RunPl {
        #[arg(long)]
        det: bool,
    },
    /// Compare simulation outcomes and matched outputs (cache on)
    SimAl {
        #[arg(long)]
        det: bool,
    },
    /// Compare native SL simulation outcomes and matched outputs (cache on)
    SimSl {
        #[arg(long)]
        det: bool,
    },
    /// Compare native PL simulation outcomes and matched outputs (cache on)
    SimPl {
        #[arg(long)]
        det: bool,
    },
}

fn execute(mut command: Command) -> Result<()> {
    if matches!(
        command,
        Command::P4parse
            | Command::Structure
            | Command::RunAl
            | Command::RunSl { .. }
            | Command::RunPl { .. }
            | Command::SimAl { .. }
            | Command::SimSl { .. }
            | Command::SimPl { .. }
    ) && std::env::var_os("UPDATE_EXPECT").is_some()
    {
        return Err(Error::Invalid(
            "UPDATE_EXPECT is supported only for elab, algo, prose, adoc, and diagnostics"
                .to_owned(),
        ));
    }
    // Resolve output paths before changing to the specification repository
    if let Command::Adoc { path_output: Some(path_output) } = &mut command {
        *path_output = std::path::absolute(&*path_output)?;
    }
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()?;
    std::env::set_current_dir(&root)?;
    match command {
        Command::Diagnostics { suite } => diagnostic::run(suite),
        Command::P4parse => p4parse::run(),
        Command::Elab => elab::run(),
        Command::Algo => algo::run(),
        Command::Structure => structure::run(),
        Command::Prose => prose::run(),
        Command::Adoc { path_output } => adoc::run(path_output.as_deref()),
        Command::RunAl => run::run(),
        Command::RunSl { det } => run::run_sl(det),
        Command::RunPl { det } => run::run_pl(det),
        Command::SimAl { det } => sim::run(det),
        Command::SimSl { det } => sim::run_sl(det),
        Command::SimPl { det } => sim::run_pl(det),
    }
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    let result = std::thread::Builder::new()
        .name("expected-driver".to_owned())
        .stack_size(64 * 1024 * 1024)
        .spawn(move || execute(cli.command));
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
