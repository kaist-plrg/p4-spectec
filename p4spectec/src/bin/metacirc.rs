//! Command-line specification transformation and AL metacircular execution
//!
//! `algo`, `struct`, and `prose` transform ordered specification paths natively.
//! `run-meta` requires a tower configuration for AL, IL, and P4 inputs.
//! `sim` runs an STF test on an SL or K tower of two or more levels over P4,
//! printing each matched packet as `p4spectec sim` does.
//! Structured diagnostics are rendered at the CLI boundary.
//! Success is printed only after the configured outer entry finishes.

use clap::{Args, Parser, Subcommand};
use p4spectec::{
    diagnostic::{DisplayStyle, RenderConfig, Renderer, Report},
    lang::{data::encoding::Encoding, traits::print::Print},
    metacirc::{self, TowerConfig},
    runner,
};
use std::{path::PathBuf, process::ExitCode};

#[derive(Parser)]
#[command(version, about = "Transform specifications and execute AL interpreter towers")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Convert specifications and print the algorithmic representation.
    Algo(SpecArgs),
    /// Structure specifications and print them without rule groups.
    Struct(SpecArgs),
    /// Convert specifications and print the prose representation.
    Prose(SpecArgs),
    /// Execute a program through a JSON-configured interpreter tower.
    RunMeta(RunMetaArgs),
    /// Simulate a P4 program and STF test through an SL or K tower over P4.
    Sim(SimArgs),
}

#[derive(Args)]
struct SpecArgs {
    /// Specification files in processing order.
    #[arg(required = true, value_name = "PATH")]
    paths: Vec<PathBuf>,
}

#[derive(Args)]
struct Options {
    /// Disable interpreter call and AL encode/decode caching.
    #[arg(long)]
    no_cache: bool,
    /// Disable deterministic execution checks in the native base interpreter.
    #[arg(long)]
    no_det: bool,
    /// Check types at native call boundaries.
    #[arg(long)]
    guard: bool,
}

impl Options {
    fn config(&self) -> runner::Config {
        runner::Config::new(!self.no_cache, !self.no_det, self.guard)
    }
}

#[derive(Args)]
struct RunMetaArgs {
    /// Tower configuration, ordered from native base to top level.
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

#[derive(Args)]
struct SimArgs {
    /// Tower configuration: an SL-executed or K meta specification over a P4 top level.
    config: PathBuf,
    /// Target architecture: ebpf, psa, or v1model.
    #[arg(long, value_name = "ARCH")]
    arch: String,
    /// P4 program to simulate.
    #[arg(short = 'p', value_name = "PROGRAM")]
    program: PathBuf,
    /// STF test to execute.
    #[arg(long, value_name = "STF")]
    stf: PathBuf,
    /// Include search directories for the P4 program.
    #[arg(short = 'i', value_name = "DIR")]
    includes: Vec<PathBuf>,
    #[command(flatten)]
    options: Options,
}

/// Renders reports without changing their structured payloads.
fn render_report(renderer: &mut Renderer, report: &Report) {
    if let Err(error) = renderer.render_to_stderr(report) {
        eprintln!("{report}\ndiagnostic rendering failed: {error}");
    }
}

/// Prints a transformed specification after rendering accumulated warnings.
fn print_spec<Spec: Print>(
    (result, warnings): (Result<Spec, p4spectec::Error>, Vec<Report>),
) -> metacirc::Result<()> {
    let mut renderer = Renderer::new(RenderConfig::default());
    // Preserve warnings even when a later transformation fails
    for report in warnings {
        render_report(&mut renderer, &report);
    }
    // Emit the specification only after conversion succeeds
    println!("{}", Print::to_string(&result?));
    Ok(())
}

/// Dispatches specification transformations and tower execution.
fn run(cli: Cli) -> metacirc::Result<()> {
    match cli.command {
        Command::Algo(args) => print_spec(p4spectec::algo_with_warnings(&args.paths)),
        Command::Struct(args) => print_spec(p4spectec::structure_with_warnings(&args.paths, true)),
        Command::Prose(args) => print_spec(p4spectec::prosify_with_warnings(&args.paths)),
        Command::RunMeta(args) => {
            let config = TowerConfig::from_file(&args.config)?;
            metacirc::run(&config, &args.includes, &args.program, args.options.config())?;
            println!("passed");
            Ok(())
        }
        Command::Sim(args) => sim(args),
    }
}

/// Runs an STF test through the tower, printing packets as they match.
fn sim(args: SimArgs) -> metacirc::Result<()> {
    let config = TowerConfig::from_file(&args.config)?;
    let mut simulator =
        metacirc::build_simulator(&config, &args.arch, args.options.config(), Encoding::default())?;
    simulator.run_stf_test(&args.includes, &args.program, &args.stf, |tx| {
        println!("[PASS] Transmitted {tx}");
    })?;
    println!("passed");
    Ok(())
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match run(cli) {
        Ok(()) => ExitCode::SUCCESS,
        Err(report) => {
            let config =
                RenderConfig { frame_style: Some(DisplayStyle::Short), ..RenderConfig::default() };
            render_report(&mut Renderer::new(config), &report);
            ExitCode::FAILURE
        }
    }
}
