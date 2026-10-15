//! Command-line AL metacircular execution
//!
//! The run command requires a tower configuration for AL and P4 inputs.
//! Structured diagnostics are rendered at the CLI boundary.
//! Success is printed only after the configured outer entry finishes.

use clap::{Args, Parser, Subcommand};
use p4spectec::{
    diagnostic::{DisplayStyle, RenderConfig, Renderer},
    metacirc::{self, TowerConfig},
    runner,
};
use std::{path::PathBuf, process::ExitCode};

#[derive(Parser)]
#[command(version, about = "Execute AL metacircular specifications")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Execute a program through a JSON-configured interpreter tower.
    Run(RunArgs),
}

#[derive(Args)]
struct Options {
    /// Disable interpreter call and AL encode/decode caching.
    #[arg(long)]
    no_cache: bool,
    /// Check deterministic execution.
    #[arg(long)]
    det: bool,
    /// Check types at native call boundaries.
    #[arg(long)]
    guard: bool,
}

impl Options {
    fn config(&self) -> runner::Config {
        runner::Config::new(!self.no_cache, self.det, self.guard)
    }
}

#[derive(Args)]
struct RunArgs {
    /// Tower configuration, ordered from native base to target.
    config: PathBuf,
    /// Program source paths in processing order; P4 requires exactly one.
    #[arg(short = 'p', required = true)]
    program: Vec<PathBuf>,
    /// Include search directories for P4 inputs.
    #[arg(short = 'i')]
    includes: Vec<PathBuf>,
    #[command(flatten)]
    options: Options,
}

/// Executes the configured tower through the shared library.
fn run(cli: Cli) -> metacirc::Result<()> {
    match cli.command {
        Command::Run(args) => {
            let config = TowerConfig::from_file(&args.config)?;
            metacirc::run(&config, &args.includes, &args.program, args.options.config())
        }
    }
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match run(cli) {
        Ok(()) => {
            println!("passed");
            ExitCode::SUCCESS
        }
        Err(report) => {
            let config =
                RenderConfig { frame_style: Some(DisplayStyle::Short), ..RenderConfig::default() };
            if let Err(error) = Renderer::new(config).render_to_stderr(&report) {
                eprintln!("{report}\ndiagnostic rendering failed: {error}");
            }
            ExitCode::FAILURE
        }
    }
}
