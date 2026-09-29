//! Command-line specification transformation and execution
//!
//! Commands render accumulated warnings before their result or error.
//! [`run`] propagates typed failures to [`main`],
//! which renders source reports and chooses the process exit code.

use std::{path::PathBuf, process::ExitCode};

use clap::{Args, Parser, Subcommand};

use p4spec_rust::{
    backend_specdoc::splicer,
    diagnostic::{RenderConfig, Renderer, Report},
    interface::p4::{error::P4Error, parse::parse_file},
    interp::shared::backtrack::Failure as InterpError,
    lang::{data::value::external::Encoding, traits::print::Print},
    runner::{self, BuiltinInterface, Interpreter, Runner},
    sim_plugin::{self, dummy::Dummy},
};

// = Helpers

// - Diagnostic output

/// Renders reports without changing their structured payloads.
fn render_report(report: &Report) {
    let mut renderer = Renderer::new(RenderConfig::default());
    if let Err(error) = renderer.render_to_stderr(report) {
        eprintln!("{report}\ndiagnostic rendering failed: {error}");
    }
}

// = Errors

/// A command failure with its user-facing diagnostic category.
#[derive(Debug, thiserror::Error)]
/// A command failure with its user-facing diagnostic category.
enum CliError {
    #[error("command error: {0}")]
    Command(String),
    #[error(transparent)]
    Splice(#[from] splicer::Error),
    #[error(transparent)]
    Spec(#[from] p4spec_rust::Error),
    #[error(transparent)]
    Runner(#[from] runner::BuildError),
    #[error(transparent)]
    Simulator(#[from] sim_plugin::BuildError),
    #[error(transparent)]
    Simulation(#[from] sim_plugin::runner::Error),
    #[error("syntax error: {0}")]
    Syntax(#[from] P4Error),
    #[error("runtime error: {0}")]
    Runtime(#[from] InterpError),
}

// = Specification loading

/// Renders accumulated warnings before returning the command result or error.
fn report_warnings<Value, Error>(
    (result, warnings): (Result<Value, Error>, Vec<Report>),
) -> Result<Value, CliError>
where
    CliError: From<Error>,
{
    for report in warnings {
        render_report(&report);
    }
    result.map_err(CliError::from)
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
fn elab_command(args: ElabArgs) -> Result<(), CliError> {
    let spec_il = report_warnings(p4spec_rust::elab_with_warnings(&args.paths))?;
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
fn algo_command(args: AlgoArgs) -> Result<(), CliError> {
    let spec_al = report_warnings(p4spec_rust::algo_with_warnings(&args.paths))?;
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
fn struct_command(args: StructArgs) -> Result<(), CliError> {
    let spec_sl = report_warnings(p4spec_rust::structure_with_warnings(&args.paths, true))?;
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
fn prose_command(args: ProseArgs) -> Result<(), CliError> {
    let spec_pl = report_warnings(p4spec_rust::prosify_with_warnings(&args.paths))?;
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
    /// Replace each skeleton in place, ignoring output paths.
    #[arg(long)]
    inplace: bool,
}

/// Expands skeleton documents using the source and prose specifications.
fn splice_command(args: SpliceArgs) -> Result<(), CliError> {
    // Resolve output paths before loading specifications or touching documents
    let path_pairs: Vec<_> = if args.inplace {
        args.paths_input
            .into_iter()
            .map(|path| (path.clone(), path))
            .collect()
    } else {
        // Reject mismatched lists before zip can omit unpaired paths
        if args.paths_input.len() != args.paths_output.len() {
            return Err(CliError::Command(
                "number of input and output files must match".to_owned(),
            ));
        }
        args.paths_input
            .into_iter()
            .zip(args.paths_output)
            .collect()
    };
    // Retain source definitions alongside the annotated prose representation
    let spec_el = p4spec_rust::parse(&args.paths)?;
    let spec_pl = report_warnings(p4spec_rust::prosify_with_warnings(&args.paths))?;
    // Render accumulated splice warnings before propagating the file result
    report_warnings(splicer::splice_files_with_warnings(&spec_el, &spec_pl, &path_pairs))
}

// = Run command

#[derive(Args)]
#[group(required = true, multiple = false)]
/// Selects which language the run and sim commands execute.
struct InterpreterArgs {
    /// Execute the algorithmic representation.
    #[arg(long)]
    al: bool,
    /// Execute the structured representation.
    #[arg(long)]
    sl: bool,
    /// Execute the prose representation.
    #[arg(long)]
    pl: bool,
}

/// Converts the specifications up to the selected interpreter's language.
fn interp_spec(paths: &[PathBuf], interpreter: &InterpreterArgs) -> Result<runner::Spec, CliError> {
    // Each pipeline stops at the language selected by the command
    if interpreter.al {
        report_warnings(p4spec_rust::algo_with_warnings(paths)).map(runner::Spec::Al)
    } else if interpreter.sl {
        report_warnings(p4spec_rust::structure_with_warnings(paths, true)).map(runner::Spec::Sl)
    } else {
        report_warnings(p4spec_rust::prosify_with_warnings(paths)).map(runner::Spec::Pl)
    }
}

#[derive(Args)]
/// Arguments of the `run` command.
struct RunArgs {
    #[command(flatten)]
    interpreter: InterpreterArgs,
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

/// Builds the selected interpreter and runs the program entry relation.
fn run_command(args: RunArgs) -> Result<(), CliError> {
    // Convert the specification before assembling its runner
    let spec = interp_spec(&args.paths, &args.interpreter)?;
    let config = runner::Config::new(!args.no_cache, args.det, args.guard);
    // Each runner uses the same P4 frontend and dummy extern implementation
    match spec {
        runner::Spec::Al(spec) => {
            let runner = runner::build_al(spec, config, Dummy)?;
            run_program(runner, &args)
        }
        runner::Spec::Sl(spec) => {
            let runner = runner::build_sl(spec, config, Dummy)?;
            run_program(runner, &args)
        }
        runner::Spec::Pl(spec) => {
            let runner = runner::build_pl(spec, config, Dummy)?;
            run_program(runner, &args)
        }
    }
}

/// Parses the P4 program and evaluates the entry relation.
fn run_program<Interp>(
    mut runner: Runner<Interp, BuiltinInterface, Dummy>,
    args: &RunArgs,
) -> Result<(), CliError>
where
    Interp: Interpreter<BuiltinInterface, Dummy, Error = InterpError>,
{
    let program = parse_file(runner.arena_mut(), &args.includes, &args.program)?;
    runner.eval_program(&args.relation, program)?;
    println!("passed");
    Ok(())
}

// = Sim command

#[derive(Args)]
/// Arguments of the `sim` command.
struct SimArgs {
    #[command(flatten)]
    interpreter: InterpreterArgs,
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

/// Builds the target simulator and runs the STF test.
fn sim_command(args: SimArgs) -> Result<(), CliError> {
    let spec = interp_spec(&args.paths, &args.interpreter)?;
    let config = runner::Config::new(!args.no_cache, args.det, args.guard);
    let simulator = sim_plugin::build(spec, &args.arch, config, args.plugin_encoding)?;
    simulate(simulator, &args)
}

/// Runs the STF test on the simulator, printing each transmitted packet.
fn simulate(mut simulator: sim_plugin::Simulator, args: &SimArgs) -> Result<(), CliError> {
    simulator.run_stf_test(&args.includes, &args.program, &args.stf, |tx| {
        println!("[PASS] Transmitted {tx}");
    })?;
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
    /// Run a P4 program with the algorithmic interpreter.
    Run(RunArgs),
    /// Simulate a P4 program and STF test on a target architecture.
    Sim(SimArgs),
}

/// Dispatches the parsed command.
fn run(cli: Cli) -> Result<(), CliError> {
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
    match run(Cli::parse()) {
        // Successful commands have already written their output
        Ok(()) => ExitCode::SUCCESS,
        // Preserve source diagnostics across the completed transformation stages
        Err(CliError::Spec(
            p4spec_rust::Error::Frontend(report)
            | p4spec_rust::Error::Elab(report)
            | p4spec_rust::Error::Algo(report)
            | p4spec_rust::Error::Structure(report),
        ))
        | Err(CliError::Splice(report)) => {
            render_report(&report);
            ExitCode::FAILURE
        }
        // Preserve runtime failure reports until execution has ended
        Err(CliError::Runtime(failure))
        | Err(CliError::Simulation(sim_plugin::runner::Error::Runtime(failure))) => {
            render_report(&failure.into_report());
            ExitCode::FAILURE
        }
        // Loading failures have no recoverable control state
        Err(CliError::Runner(runner::BuildError::Interp(report))) => {
            render_report(&report);
            ExitCode::FAILURE
        }
        // Report other typed failures once at the process boundary
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}
