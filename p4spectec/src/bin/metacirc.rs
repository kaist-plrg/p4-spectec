//! Command-line specification transformation and AL metacircular execution
//!
//! `algo`, `struct`, and `prose` transform ordered specification paths natively.
//! `run-meta` requires a tower configuration for AL, IL, P4, and KORE inputs;
//! a KORE input prints its run as `krun` does, bounded by `--depth`.
//! `sim` runs an STF test on an SL, K, or Redex tower of two or more levels over P4,
//! printing each matched packet as `p4spectec sim` does.
//! `cover-run` and `cover-sim` report the SL instruction coverage of the base
//! of an SL tower over many inputs, as `p4spectec` does on a P4 specification:
//! `cover-run` evaluates P4 programs or scripts, by the tower's top level,
//! and `cover-sim` runs STF tests on a tower over P4.
//! Structured diagnostics are rendered at the CLI boundary.
//! Success is printed only after the configured outer entry finishes.

use clap::{Args, CommandFactory, Parser, Subcommand, error::ErrorKind};
use indicatif::{ProgressBar, ProgressStyle};
use p4spectec::{
    coverage::{self, Outcome, Progress},
    diagnostic::{Diagnostic, DisplayStyle, RenderConfig, Renderer, Report, Severity},
    interface::Lang,
    lang::{data::encoding::Encoding, traits::print::Print},
    metacirc::{self, TowerConfig},
    runner,
};
use std::{
    path::{Path, PathBuf},
    process::ExitCode,
};

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
    /// Simulate a P4 program and STF test through an SL, K, or Redex tower over P4.
    Sim(SimArgs),
    /// Measure the base's SL instruction coverage of running programs through a tower.
    CoverRun(CoverRunArgs),
    /// Measure the base's SL instruction coverage of simulating STF tests through a tower.
    CoverSim(CoverSimArgs),
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
        runner::Config::new(!self.no_cache, !self.no_det, self.guard, true)
    }

    fn cover_config(&self) -> runner::Config {
        // Tail calls would drop the result instructions coverage must see
        runner::Config::new(!self.no_cache, !self.no_det, self.guard, false)
    }

    fn describe(&self) -> String {
        format!("config: cache={} det={} guard={}", !self.no_cache, !self.no_det, self.guard)
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
    /// Rewriting steps to take at most for a KORE input, as `krun --depth`.
    #[arg(long, value_name = "N")]
    depth: Option<u64>,
    #[command(flatten)]
    options: Options,
}

#[derive(Args)]
struct SimArgs {
    /// Tower configuration: an SL-executed, K, or Redex meta specification over a P4 top level.
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

#[derive(Args)]
struct CoverRunArgs {
    /// Tower configuration: an SL tower over a P4 or script top level.
    config: PathBuf,
    /// Programs in the top level's language, or directories searched recursively for them.
    #[arg(short = 'p', long = "program", required = true, value_name = "PATH")]
    paths_program: Vec<PathBuf>,
    /// Include search directories for P4 programs.
    #[arg(short = 'i', value_name = "DIR")]
    includes: Vec<PathBuf>,
    /// Directories of .exclude files naming skipped programs.
    #[arg(short = 'e', long = "exclude", value_name = "DIR")]
    excludes: Vec<PathBuf>,
    /// Report file; the report goes to stdout without it.
    #[arg(long = "cov", value_name = "PATH")]
    path_cov: Option<PathBuf>,
    #[command(flatten)]
    options: Options,
}

#[derive(Args)]
struct CoverSimArgs {
    /// Tower configuration: an SL tower of two or more levels over a P4 top level.
    config: PathBuf,
    /// Target architecture: ebpf, psa, or v1model.
    #[arg(long, value_name = "ARCH")]
    arch: String,
    /// Native plugin state encoding: arena-relative or arena-independent.
    #[arg(long, default_value_t = Encoding::default(), value_name = "ENCODING")]
    plugin_encoding: Encoding,
    /// P4 programs, or directories searched recursively for them.
    #[arg(long = "p4", required = true, value_name = "PATH")]
    paths_p4: Vec<PathBuf>,
    /// STF tests, or directories searched recursively for them.
    #[arg(long = "stf", required = true, value_name = "PATH")]
    paths_stf: Vec<PathBuf>,
    /// Directories of P4 and STF files replacing inputs with the same stem.
    #[arg(long = "patch", value_name = "DIR")]
    dirs_patch: Vec<PathBuf>,
    /// Include search directories for the P4 programs.
    #[arg(short = 'i', value_name = "DIR")]
    includes: Vec<PathBuf>,
    /// Directories of .exclude files naming skipped P4 or STF files.
    #[arg(short = 'e', long = "exclude", value_name = "DIR")]
    excludes: Vec<PathBuf>,
    /// Report file; the report goes to stdout without it.
    #[arg(long = "cov", value_name = "PATH")]
    path_cov: Option<PathBuf>,
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
            let options = args.options.config();
            if args.depth.is_some() && config.top_lang() != Lang::Kore {
                Cli::command()
                    .error(ErrorKind::ArgumentConflict, "--depth applies only to a KORE input")
                    .exit();
            }
            // A KORE input prints its run, as krun does; any other passes
            let output =
                metacirc::run(&config, &args.includes, &args.program, options, args.depth)?;
            println!("{}", output.as_deref().unwrap_or("passed"));
            Ok(())
        }
        Command::Sim(args) => sim(args),
        Command::CoverRun(args) => cover_run(args),
        Command::CoverSim(args) => cover_sim(args),
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

// = Coverage

/// Structures the base specification of a coverable tower, rendering its warnings.
fn cover_spec(config: &TowerConfig) -> metacirc::Result<p4spectec::lang::sl::ast::Spec> {
    let specdir = metacirc::cover::base_specdir(config)?.to_path_buf();
    let (result, warnings) = p4spectec::structure_with_warnings(&[specdir], true);
    let mut renderer = Renderer::new(RenderConfig::default());
    for report in warnings {
        render_report(&mut renderer, &report);
    }
    Ok(result?)
}

/// Creates a progress bar over the inputs, drawn on stderr only for a terminal.
fn progress_bar(labels: &[String]) -> ProgressBar {
    let style = ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")
        .expect("progress template must parse");
    ProgressBar::new(labels.len() as u64).with_style(style)
}

/// Advances the bar and keeps a `[k/N] outcome path` line for each input that did not pass.
fn print_progress(bar: &ProgressBar, progress: Progress, labels: &[String]) {
    match progress {
        Progress::Start(index) => bar.set_message(labels[index].clone()),
        Progress::Finish(index, outcome) => {
            if outcome != Outcome::Pass {
                let line = format!(
                    "[{}/{}] {} {}",
                    index + 1,
                    labels.len(),
                    outcome.name(),
                    labels[index]
                );
                bar.suspend(|| eprintln!("{line}"));
            }
            bar.inc(1);
        }
    }
}

/// Warns that an explicitly given program is skipped for another architecture.
fn cover_input_filtered(path: &Path, arch: &str) -> Report {
    Diagnostic::new(
        "command",
        Severity::Warning,
        Some("command/cover-input-filtered".to_owned()),
        format!("{} does not include the {arch} model and is skipped", path.display()),
        Vec::new(),
        Vec::new(),
    )
    .into()
}

/// Evaluates programs through the tower and reports the base's instruction coverage.
fn cover_run(args: CoverRunArgs) -> metacirc::Result<()> {
    let config = TowerConfig::from_file(&args.config)?;
    // Collect inputs before the slower specification loading
    let suffix = metacirc::cover::program_suffix(&config)?;
    let excludes = coverage::corpus::collect_excludes(&args.excludes)?;
    let programs = coverage::corpus::collect_programs(&args.paths_program, suffix, &excludes)?;
    let spec_sl = cover_spec(&config)?;

    // Evaluate every program on one tower
    let labels: Vec<_> = programs
        .items
        .iter()
        .map(|path| path.display().to_string())
        .collect();
    let bar = progress_bar(&labels);
    let coverage = metacirc::cover::cover_run(
        &config,
        &spec_sl,
        args.options.cover_config(),
        &args.includes,
        &programs.items,
        |progress| print_progress(&bar, progress, &labels),
    );
    bar.finish_and_clear();
    let coverage = coverage?;

    let summary = format!("programs {}", coverage.describe_outcomes(programs.excluded));
    let comments =
        [format!("tower: {}", args.config.display()), args.options.describe(), summary.clone()];
    coverage::log::write_report_to(args.path_cov.as_deref(), &spec_sl, &coverage.cover, &comments)?;
    eprintln!("{}", coverage::log::summary_line("cover-run", &summary, &coverage.cover));
    Ok(())
}

/// Runs STF tests through the tower and reports the base's instruction coverage.
fn cover_sim(args: CoverSimArgs) -> metacirc::Result<()> {
    let config = TowerConfig::from_file(&args.config)?;
    // Collect inputs before the slower specification loading
    let excludes = coverage::corpus::collect_excludes(&args.excludes)?;
    let pairs = coverage::corpus::collect_pairs(
        &args.arch,
        &args.paths_p4,
        &args.paths_stf,
        &args.dirs_patch,
        &excludes,
    )?;
    let mut renderer = Renderer::new(RenderConfig::default());
    for path in &pairs.filtered {
        render_report(&mut renderer, &cover_input_filtered(path, &args.arch));
    }
    let spec_sl = cover_spec(&config)?;

    // Run every pair on one simulator
    let labels: Vec<_> = pairs
        .items
        .iter()
        .map(|(path_p4, path_stf)| format!("{} {}", path_p4.display(), path_stf.display()))
        .collect();
    let bar = progress_bar(&labels);
    let coverage = metacirc::cover::cover_sim(
        &config,
        &spec_sl,
        &args.arch,
        args.options.cover_config(),
        args.plugin_encoding,
        &args.includes,
        &pairs.items,
        |progress| print_progress(&bar, progress, &labels),
    );
    bar.finish_and_clear();
    let coverage = coverage?;

    let summary = format!("tests {}", coverage.describe_outcomes(pairs.excluded));
    let comments = [
        format!("tower: {}", args.config.display()),
        format!("arch: {}", args.arch),
        args.options.describe(),
        summary.clone(),
    ];
    coverage::log::write_report_to(args.path_cov.as_deref(), &spec_sl, &coverage.cover, &comments)?;
    eprintln!("{}", coverage::log::summary_line("cover-sim", &summary, &coverage.cover));
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
