//! Command-line specification transformation and execution
//!
//! Commands render accumulated warnings before their result or error.
//! `run` and `sim` choose how their execution failures are rendered.
//! [`main`] turns the command result into a process exit code.

mod error;

use std::{path::PathBuf, process::ExitCode};

use clap::{Args, Parser, Subcommand};

use p4spectec::lang::{data::encoding::Encoding, traits::print::Print};

use p4spectec::diagnostic::{DisplayStyle, RenderConfig, Renderer, Report};

use p4spectec::runner::{self, RunError};

// = Helpers

// - Diagnostic output

/// Renders reports without changing their structured payloads.
fn render_report_with_config(report: &Report, config: RenderConfig) {
    let mut renderer = Renderer::new(config);
    if let Err(error) = renderer.render_to_stderr(report) {
        eprintln!("{report}\ndiagnostic rendering failed: {error}");
    }
}

/// Renders a report with the default diagnostic presentation.
fn render_report(report: &Report) {
    render_report_with_config(report, RenderConfig::default());
}

/// Renders execution frames compactly while retaining rich causes.
fn render_exec_report(report: &Report) {
    let config = RenderConfig { frame_style: Some(DisplayStyle::Short), ..RenderConfig::default() };
    render_report_with_config(report, config);
}

// = Specification loading

/// Renders accumulated warnings before returning the command result or error.
fn report_warnings<Value, Error>(
    (result, warnings): (Result<Value, Error>, Vec<Report>),
) -> Result<Value, Error> {
    for report in warnings {
        render_report(&report);
    }
    result
}

// = Elab command

#[derive(Args)]
/// Arguments of the `elab` command.
struct ElabArgs {
    /// Specification files in processing order.
    #[arg(required = true, value_name = "PATH")]
    paths: Vec<PathBuf>,
}

/// Elaborates the specifications and prints the internal language.
fn elab_command(args: ElabArgs) -> Result<(), ()> {
    let spec_il = report_warnings(p4spectec::elab_with_warnings(&args.paths))
        .map_err(|report| render_report(&report))?;
    println!("{}", Print::to_string(&spec_il));
    Ok(())
}

// = Algo command

#[derive(Args)]
/// Arguments of the `algo` command.
struct AlgoArgs {
    /// Specification files in processing order.
    #[arg(required = true, value_name = "PATH")]
    paths: Vec<PathBuf>,
}

/// Converts the specifications and prints the algorithmic language.
fn algo_command(args: AlgoArgs) -> Result<(), ()> {
    let spec_al = report_warnings(p4spectec::algo_with_warnings(&args.paths))
        .map_err(|report| render_report(&report))?;
    println!("{}", Print::to_string(&spec_al));
    Ok(())
}

// = Struct command

#[derive(Args)]
/// Arguments of the `struct` command.
struct StructArgs {
    /// Specification files in processing order.
    #[arg(required = true, value_name = "PATH")]
    paths: Vec<PathBuf>,
}

/// Structures the specifications and prints them without rule groups.
fn struct_command(args: StructArgs) -> Result<(), ()> {
    let spec_sl = report_warnings(p4spectec::structure_with_warnings(&args.paths, true))
        .map_err(|report| render_report(&report))?;
    println!("{}", Print::to_string(&spec_sl));
    Ok(())
}

// = Prose command

#[derive(Args)]
/// Arguments of the `prose` command.
struct ProseArgs {
    /// Specification files in processing order.
    #[arg(required = true, value_name = "PATH")]
    paths: Vec<PathBuf>,
}

/// Converts the specifications and prints the prose language.
fn prose_command(args: ProseArgs) -> Result<(), ()> {
    let spec_pl = report_warnings(p4spectec::prosify_with_warnings(&args.paths))
        .map_err(|report| render_report(&report))?;
    println!("{}", Print::to_string(&spec_pl));
    Ok(())
}

// = Splice command

#[derive(Args)]
/// Arguments of the `splice` command.
struct SpliceArgs {
    /// Specification files in processing order.
    #[arg(required = true, value_name = "PATH")]
    paths: Vec<PathBuf>,
    /// Skeleton documents in processing order.
    #[arg(long = "splice", value_name = "PATH")]
    paths_input: Vec<PathBuf>,
    /// Output files paired with skeletons in order.
    #[arg(long = "out", value_name = "PATH")]
    paths_output: Vec<PathBuf>,
    /// Replace each skeleton in place; cannot be combined with --out.
    #[arg(long)]
    inplace: bool,
}

/// Expands skeleton documents using the source and prose specifications.
fn splice_command(args: SpliceArgs) -> Result<(), ()> {
    // Reject ambiguous destinations before checking input availability
    if args.inplace && !args.paths_output.is_empty() {
        render_report(&error::splice_output_conflict());
        return Err(());
    }
    // Require at least one skeleton in either output mode
    if args.paths_input.is_empty() {
        render_report(&error::splice_input_required());
        return Err(());
    }
    // Reject mismatched lists before zip can omit unpaired paths
    if !args.inplace && args.paths_input.len() != args.paths_output.len() {
        render_report(&error::splice_file_count_mismatch(
            args.paths_input.len(),
            args.paths_output.len(),
        ));
        return Err(());
    }
    // Resolve output paths before loading specifications or touching documents
    let path_pairs: Vec<_> = if args.inplace {
        args.paths_input
            .into_iter()
            .map(|path| (path.clone(), path))
            .collect()
    } else {
        args.paths_input
            .into_iter()
            .zip(args.paths_output)
            .collect()
    };
    // Retain source definitions alongside the annotated prose representation
    let (spec_el, spec_pl) = report_warnings(p4spectec::specdoc_spec_with_warnings(&args.paths))
        .map_err(|report| render_report(&report))?;
    // Render accumulated splice warnings before propagating the file result
    report_warnings(p4spectec::splice_files_with_warnings(&spec_el, &spec_pl, &path_pairs))
        .map_err(|report| render_report(&report))?;
    Ok(())
}

// = Run command

#[derive(Args)]
#[group(multiple = false)]
/// Selects which language the run and sim commands execute.
struct InterpArgs {
    /// Execute the algorithmic representation.
    #[arg(long)]
    al: bool,
    /// Execute the structured representation (default).
    #[arg(long)]
    sl: bool,
    /// Execute the prose representation.
    #[arg(long)]
    pl: bool,
}

impl InterpArgs {
    fn spec_lang(&self) -> p4spectec::SpecLang {
        if self.al {
            p4spectec::SpecLang::Al
        } else if self.pl {
            p4spectec::SpecLang::Pl
        } else {
            p4spectec::SpecLang::Sl
        }
    }
}

#[derive(Args)]
/// Arguments of the `run` command.
struct RunArgs {
    #[command(flatten)]
    interpreter: InterpArgs,
    /// Specification files in processing order.
    #[arg(required = true, value_name = "PATH")]
    paths: Vec<PathBuf>,
    /// Entry relation to evaluate.
    #[arg(long = "rel", value_name = "RELATION")]
    relation: String,
    /// P4 program to execute.
    #[arg(short = 'p', value_name = "PROGRAM")]
    program: PathBuf,
    /// Include directories for the P4 program.
    #[arg(short = 'i', value_name = "DIR")]
    includes: Vec<PathBuf>,
    /// Disable interpreter call caching.
    #[arg(long)]
    no_cache: bool,
    /// Check deterministic execution.
    #[arg(long)]
    det: bool,
    /// Check interpreter guards.
    #[arg(long)]
    guard: bool,
}

/// Prepares and runs a program, rendering diagnostics at the CLI boundary.
fn run_command(args: RunArgs) -> Result<(), ()> {
    // Render specification warnings before constructing the runner
    let spec = report_warnings(p4spectec::runner_spec_with_warnings(
        args.interpreter.spec_lang(),
        &args.paths,
    ))
    .map_err(|report| render_report(&report))?;

    // Execute with the requested interpreter controls
    let config = runner::Config::new(!args.no_cache, args.det, args.guard);
    p4spectec::run(spec, config, &args.relation, &args.includes, &args.program).map_err(
        |error| match error {
            RunError::Build(report) => {
                // Render runner preparation with the default presentation
                render_report(&report);
            }
            RunError::Parse(error) => {
                // Keep P4 input failures rich
                render_report(&error.into_report());
            }
            RunError::Eval(error) => {
                // Compact only the execution frames
                render_exec_report(&error.into_report());
            }
        },
    )?;

    // Print success only after evaluation completes
    println!("passed");
    Ok(())
}

// = Sim command

#[derive(Args)]
/// Arguments of the `sim` command.
struct SimArgs {
    #[command(flatten)]
    interpreter: InterpArgs,
    /// Specification files in processing order.
    #[arg(required = true, value_name = "PATH")]
    paths: Vec<PathBuf>,
    /// Target architecture: ebpf, psa, or v1model.
    #[arg(long, value_name = "ARCH")]
    arch: String,
    /// Native plugin state encoding: arena-relative or arena-independent.
    #[arg(long, default_value_t = Encoding::default(), value_name = "ENCODING")]
    plugin_encoding: Encoding,
    /// P4 program to simulate.
    #[arg(short = 'p', value_name = "PROGRAM")]
    program: PathBuf,
    /// STF test to execute.
    #[arg(long, value_name = "STF")]
    stf: PathBuf,
    /// Include directories for the P4 program.
    #[arg(short = 'i', value_name = "DIR")]
    includes: Vec<PathBuf>,
    /// Disable interpreter call caching.
    #[arg(long)]
    no_cache: bool,
    /// Check deterministic execution.
    #[arg(long)]
    det: bool,
    /// Check interpreter guards.
    #[arg(long)]
    guard: bool,
}

/// Prepares a simulator and runs its STF test with CLI progress output.
fn sim_command(args: SimArgs) -> Result<(), ()> {
    // Render specification warnings before constructing the simulator
    let spec = report_warnings(p4spectec::runner_spec_with_warnings(
        args.interpreter.spec_lang(),
        &args.paths,
    ))
    .map_err(|report| render_report(&report))?;

    // Build the requested native architecture
    let config = runner::Config::new(!args.no_cache, args.det, args.guard);
    let mut simulator = p4spectec::build_simulator(spec, &args.arch, config, args.plugin_encoding)
        .map_err(|report| render_report(&report))?;

    // Print matched packets as execution proceeds
    simulator
        .run_stf_test(&args.includes, &args.program, &args.stf, |tx| {
            println!("[PASS] Transmitted {tx}");
        })
        .map_err(|report| render_exec_report(&report))?;

    println!("passed");
    Ok(())
}

// = Entry point

/// Elaborate and convert P4 specifications.
#[derive(Parser)]
#[command(version)]
/// The command-line interface.
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
/// The available subcommands.
enum Command {
    /// Elaborate specifications and print the internal language.
    Elab(ElabArgs),
    /// Convert specifications and print the algorithmic representation.
    Algo(AlgoArgs),
    /// Structure specifications and print them without rule groups.
    Struct(StructArgs),
    /// Convert specifications and print the prose representation.
    Prose(ProseArgs),
    /// Expand skeleton documents using specification fragments.
    Splice(SpliceArgs),
    /// Run a P4 program with the selected interpreter.
    Run(RunArgs),
    /// Simulate a P4 program and STF test on a target architecture.
    Sim(SimArgs),
}

/// Dispatches the parsed command.
fn run(cli: Cli) -> Result<(), ()> {
    match cli.command {
        Command::Elab(args) => elab_command(args),
        Command::Algo(args) => algo_command(args),
        Command::Struct(args) => struct_command(args),
        Command::Prose(args) => prose_command(args),
        Command::Splice(args) => splice_command(args),
        Command::Run(args) => run_command(args),
        Command::Sim(args) => sim_command(args),
    }
}

/// Runs the command and turns a failure into one diagnostic and exit code.
fn main() -> ExitCode {
    // Help and version retain clap's successful output and exit behavior
    let cli = match Cli::try_parse() {
        Ok(cli) => cli,
        Err(error) => {
            if let Err(error) = error.print() {
                eprintln!("command output failed: {error}");
                return ExitCode::FAILURE;
            }
            return ExitCode::from(error.exit_code() as u8);
        }
    };
    match run(cli) {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}
