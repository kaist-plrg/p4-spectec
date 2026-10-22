//! Execution entry point for the AL metacircular specification

use clap::Parser;
use p4spectec_test::{
    Error, ExecutionOptions, Result, metacirc,
    suite::{self, Index},
};
use std::{path::PathBuf, process::ExitCode};

/// Compares P4 corpus outcomes through the metacircular tower.
#[derive(Parser)]
struct Cli {
    /// Suite registration file, relative to the invoking directory.
    #[arg(long = "registry")]
    path_registry: Option<PathBuf>,
    #[command(flatten)]
    options: ExecutionOptions,
}

/// Resolves caller paths and runs the registered corpus from the repository root.
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
    let (config, config_tower) = suite::load_metacirc(&index.suites.metacirc)?;
    metacirc::run(&config, &config_tower, cli.options)
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
