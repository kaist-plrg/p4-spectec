//! Command-line AL metacircular execution
//!
//! Standalone scripts use run; runtime towers use a JSON configuration.
//! Both routes share p4spectec::metacirc and render structured diagnostics here.
//! Success is printed only after the configured outer entry finishes.

use clap::{Args, Parser, Subcommand};
use p4spectec::{
    diagnostic::{DisplayStyle, RenderConfig, Renderer},
    metacirc::{self, Config, Mode},
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
    /// Interpret an AL script with builtin host calls.
    Run(RunArgs),
    /// Run a P4 program through a JSON-configured AL tower.
    Tower(TowerArgs),
}

#[derive(Args)]
struct Options {
    /// Disable interpreter call caching.
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
    /// Meta specification paths in processing order.
    #[arg(required = true)]
    paths: Vec<PathBuf>,
    /// Native backend for the meta specification.
    #[arg(long, value_enum, default_value = "sl")]
    mode: Mode,
    /// Entry relation in the meta specification.
    #[arg(long = "rel", default_value = "Entry")]
    relation: String,
    /// Source paths of the AL script to interpret.
    #[arg(short = 'p', required = true)]
    program: Vec<PathBuf>,
    #[command(flatten)]
    options: Options,
}

#[derive(Args)]
struct TowerArgs {
    /// JSON configuration, ordered from outer interpreter to P4 target.
    config: PathBuf,
    /// P4 program to execute.
    #[arg(short = 'p')]
    program: PathBuf,
    /// P4 include search directories.
    #[arg(short = 'i')]
    includes: Vec<PathBuf>,
    #[command(flatten)]
    options: Options,
}

/// Executes the selected mode through the shared library.
fn run(cli: Cli) -> metacirc::Result<()> {
    match cli.command {
        Command::Run(args) => metacirc::run_standalone(
            args.mode,
            &args.paths,
            &args.relation,
            &args.program,
            args.options.config(),
        ),
        Command::Tower(args) => {
            let config = Config::from_file(&args.config)?;
            metacirc::run_tower(&config, &args.includes, &args.program, args.options.config())
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
