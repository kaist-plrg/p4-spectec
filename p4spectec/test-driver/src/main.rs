mod util;

mod snapshot;

mod elab;

mod algo;

mod structure;

mod prose;

mod adoc;

mod corpus;

mod diagnostic;

use std::{path::PathBuf, process::ExitCode};

use clap::{Args, Parser, Subcommand};

use corpus::{micro, p4parse, run, sim};
use util::Language;

type Error = Box<dyn std::error::Error>;
type Result<T> = std::result::Result<T, Error>;

/// Source and expected-file acceptance for the Rust implementation
#[derive(Parser)]
struct Cli {
    /// Write a per-test timing log of p4parse, run, sim, or micro into a new file in this directory
    #[arg(long = "log", global = true, value_name = "DIR")]
    path_log: Option<PathBuf>,
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
    /// Run micro tests: standalone spec-language sources by pipeline stage
    Micro {
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
    /// Disable deterministic execution checks
    #[arg(long, global = true)]
    no_det: bool,
    /// Enable or disable interpreter caching
    #[arg(long, global = true, default_value_t = true, action = clap::ArgAction::Set)]
    cache_on: bool,
}

fn execute(mut cli: Cli) -> Result<()> {
    let command = &mut cli.command;
    if matches!(
        command,
        Command::Structure
            | Command::P4parse
            | Command::Run { .. }
            | Command::Sim { .. }
            | Command::Micro { .. }
    ) && std::env::var_os("UPDATE_EXPECT").is_some()
    {
        return Err(Error::from(
            "UPDATE_EXPECT is supported only for elab, algo, prose, adoc, and diagnostics",
        ));
    }
    // Resolve output paths before changing to the specification repository
    let path_log = cli
        .path_log
        .as_deref()
        .map(std::path::absolute)
        .transpose()?;
    if let Command::Adoc { path_output: Some(path_output) } = &mut *command {
        *path_output = std::path::absolute(&*path_output)?;
    }
    if let Command::Diagnostics { path_cli: Some(path_cli), .. } = &mut *command {
        *path_cli = std::path::absolute(&*path_cli)?;
    }
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()?;
    std::env::set_current_dir(&root)?;
    match command {
        Command::List => list(),
        Command::Elab => elab::run(),
        Command::Algo => algo::run(),
        Command::Structure => structure::run(),
        Command::Prose => prose::run(),
        Command::Adoc { path_output } => adoc::run(path_output.as_deref()),
        Command::P4parse => p4parse::run(path_log.as_deref()),
        Command::Run { language, options } => run::run(*language, *options, path_log.as_deref()),
        Command::Sim { language, options } => sim::run(*language, *options, path_log.as_deref()),
        Command::Micro { language, options } => {
            micro::run(*language, *options, path_log.as_deref())
        }
        Command::Diagnostics { suite, path_cli } => diagnostic::run(*suite, path_cli.as_deref()),
    }
}

/// Prints every registration without running its tests.
fn list() -> Result<()> {
    println!("elab: {:#?}", util::load::<Vec<snapshot::Snapshot>>("elab")?);
    println!("algo: {:#?}", util::load::<Vec<snapshot::Snapshot>>("algo")?);
    println!("structure: {:#?}", util::load::<Vec<structure::StructureSnapshot>>("structure")?);
    println!("prose: {:#?}", util::load::<Vec<snapshot::Snapshot>>("prose")?);
    println!("adoc: {:#?}", util::load::<adoc::AdocSuite>("adoc")?);
    println!("p4parse: {:#?}", util::load::<p4parse::ParseConfig>("p4parse")?);
    println!("run: {:#?}", util::load::<run::RunConfig>("run")?);
    println!("sim: {:#?}", util::load::<sim::SimConfig>("sim")?);
    println!("micro: {:#?}", util::load::<micro::MicroConfig>("micro")?);
    diagnostic::list()
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    let result = std::thread::Builder::new()
        .name("expected-driver".to_owned())
        .stack_size(64 * 1024 * 1024)
        .spawn(move || execute(cli).map_err(|error| error.to_string()));
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
