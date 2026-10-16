//! Command-line emission of K and PLT Redex inputs
//!
//! `kast` and `sexp` emit a SpecTec program or specification as a `script`;
//! `kast-p4` and `sexp-p4` emit a P4 program as a `val`.
//! Output goes to `-o FILE` or stdout, with a trailing newline.
//! Structured diagnostics are rendered at the CLI boundary.

use clap::{Args, Parser, Subcommand};
use p4spectec::{
    diagnostic::{DisplayStyle, RenderConfig, Renderer, Report},
    lang::data::{arena::Arena, value::flat::Value},
    metacross::{self, kast, sexp},
};
use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
    process::ExitCode,
};

#[derive(Parser)]
#[command(version, about = "Emit K and PLT Redex inputs from P4-SpecTec sources")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Emit a SpecTec program or specification as a KAST `Script` term.
    Kast(SpecArgs),
    /// Emit a P4 program as the KAST `someP4(val)` term.
    #[command(name = "kast-p4")]
    KastP4(P4Args),
    /// Emit a SpecTec program or specification as a Redex `script` s-expression.
    Sexp(SpecArgs),
    /// Emit a P4 program as a Redex `val` s-expression.
    #[command(name = "sexp-p4")]
    SexpP4(P4Args),
}

#[derive(Args)]
struct SpecArgs {
    /// A .watsup file, or a directory of them.
    path: PathBuf,
    /// Write the term to this file instead of stdout.
    #[arg(short = 'o', value_name = "FILE")]
    output: Option<PathBuf>,
}

#[derive(Args)]
struct P4Args {
    /// P4 program to emit.
    #[arg(short = 'p', value_name = "FILE")]
    program: PathBuf,
    /// Include directories for the P4 program.
    #[arg(short = 'i', value_name = "DIR")]
    includes: Vec<PathBuf>,
    /// Write the term to this file instead of stdout.
    #[arg(short = 'o', value_name = "FILE")]
    output: Option<PathBuf>,
}

fn render(report: &Report) {
    let config = RenderConfig { frame_style: Some(DisplayStyle::Short), ..RenderConfig::default() };
    if let Err(error) = Renderer::new(config).render_to_stderr(report) {
        eprintln!("{report}\ndiagnostic rendering failed: {error}");
    }
}

/// Writes the text and a newline to the output file or stdout.
fn emit(output: Option<&PathBuf>, mut text: String) -> io::Result<()> {
    text.push('\n');
    match output {
        Some(path) => fs::write(path, text),
        None => io::stdout().lock().write_all(text.as_bytes()),
    }
}

/// The text of a `script` term, rendering elaboration warnings first.
fn script(
    args: &SpecArgs,
    emitter: fn(&Arena, Value) -> metacross::Result<String>,
) -> metacross::Result<String> {
    let mut arena = Arena::new();
    let (result, warnings) = metacross::encode_script(&mut arena, std::slice::from_ref(&args.path));
    for warning in &warnings {
        render(warning);
    }
    emitter(&arena, result?)
}

/// The text of a `val` term of a P4 program.
fn p4(
    args: &P4Args,
    emitter: fn(&Arena, Value) -> metacross::Result<String>,
) -> metacross::Result<String> {
    let mut arena = Arena::new();
    let value = metacross::encode_p4(&mut arena, &args.includes, &args.program)?;
    emitter(&arena, value)
}

fn kast_text(arena: &Arena, value: Value) -> metacross::Result<String> {
    Ok(kast::script(arena, value)?.to_string())
}

fn kast_p4_text(arena: &Arena, value: Value) -> metacross::Result<String> {
    Ok(kast::p4(arena, value)?.to_string())
}

fn run(cli: Cli) -> metacross::Result<()> {
    let (text, output) = match &cli.command {
        Command::Kast(args) => (script(args, kast_text)?, args.output.as_ref()),
        Command::KastP4(args) => (p4(args, kast_p4_text)?, args.output.as_ref()),
        Command::Sexp(args) => (script(args, sexp::script)?, args.output.as_ref()),
        Command::SexpP4(args) => (p4(args, sexp::val)?, args.output.as_ref()),
    };
    emit(output, text).map_err(|error| metacross::output_failed(error.to_string()))
}

fn main() -> ExitCode {
    match run(Cli::parse()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(report) => {
            render(&report);
            ExitCode::FAILURE
        }
    }
}
