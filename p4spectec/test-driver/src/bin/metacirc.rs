//! Execution entry point for the AL metacircular specification

use std::{path::PathBuf, process::ExitCode};

use clap::{Parser, Subcommand};

use p4spectec_test::{
    harness::{self, Error, Result},
    metacirc::{k_in_st, metaparse, run, sim},
    suite::{snapshot, snapshot::Stage},
};

/// Selects an execution through towers,
/// or a snapshot or codec check of the meta specification.
#[derive(Clone, Subcommand)]
enum Command {
    /// Compare registered P4 corpus and script outcomes through towers
    Run {
        /// Configuration name in `suites/metacirc/run.toml`
        config: String,
        /// Run only the registered suite with this name.
        #[arg(long = "suite", value_name = "NAME")]
        name_suite: Option<String>,
        /// Run only the suites whose tower takes this input.
        #[arg(long, value_enum)]
        input: Option<run::Input>,
    },
    /// Compare registered simulation suites through a tower with native expectations
    Sim {
        /// Configuration name in `suites/metacirc/sim.toml`
        config: String,
        /// Run only the registered simulation suite with this name.
        #[arg(long = "suite", value_name = "NAME")]
        name_suite: Option<String>,
    },
    /// Compare K definitions run through spec-k with krun
    KInSt {
        /// Configuration name in `suites/metacirc/k-in-st.toml`
        config: String,
        /// Run only the registered suite with this name.
        #[arg(long = "suite", value_name = "NAME")]
        name_suite: Option<String>,
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
    /// List every metacirc registration
    List,
}

/// Compares P4 corpus and AL script outcomes through metacircular towers.
#[derive(Parser)]
struct Cli {
    /// Write a per-test timing log of run or sim into a new file in this directory
    #[arg(long = "log", global = true, value_name = "DIR")]
    path_log: Option<PathBuf>,
    #[command(subcommand)]
    command: Command,
}

/// Runs the selected command from the repository root.
fn execute(cli: Cli) -> Result<()> {
    // Refuse baseline promotion before discovering or executing any inputs
    if !matches!(
        cli.command,
        Command::Elab | Command::Algo | Command::Prose | Command::KInSt { .. }
    ) && std::env::var_os("UPDATE_EXPECT").is_some()
    {
        return Err(Error::from(
            "UPDATE_EXPECT is supported only for metacirc elab, algo, prose, and k-in-st",
        ));
    }
    // Resolve the log directory before changing to the specification repository
    let path_log = cli
        .path_log
        .as_deref()
        .map(std::path::absolute)
        .transpose()?;
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()?;
    std::env::set_current_dir(&root)?;
    match cli.command {
        Command::Run { config, name_suite, input } => {
            run::run(&config, name_suite.as_deref(), input, path_log.as_deref())
        }
        Command::Sim { config, name_suite } => {
            sim::run(&config, name_suite.as_deref(), path_log.as_deref())
        }
        Command::KInSt { config, name_suite } => k_in_st::run(&config, name_suite.as_deref()),
        // Snapshot and codec checks compile the meta specification without executing it
        Command::Elab => snapshot::run("metacirc/snapshot", &[Stage::Elab], None),
        Command::Algo => snapshot::run("metacirc/snapshot", &[Stage::Algo], None),
        Command::Structure => snapshot::run("metacirc/snapshot", &[Stage::Structure], None),
        Command::Prose => snapshot::run("metacirc/snapshot", &[Stage::Prose], None),
        Command::Metaparse => metaparse::run(),
        Command::List => list(),
    }
}

/// Prints every metacirc registration without running its tests.
fn list() -> Result<()> {
    println!("snapshot: {:#?}", harness::load::<snapshot::Registration>("metacirc/snapshot")?);
    println!("run: {:#?}", harness::load::<run::Registration>("metacirc/run")?);
    println!("sim: {:#?}", harness::load::<sim::Registration>("metacirc/sim")?);
    println!("k-in-st: {:#?}", harness::load::<k_in_st::Registration>("metacirc/k-in-st")?);
    println!("metaparse: {:#?}", harness::load::<metaparse::Registration>("metacirc/metaparse")?);
    Ok(())
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    let result = std::thread::Builder::new()
        .name("metacirc-driver".to_owned())
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
