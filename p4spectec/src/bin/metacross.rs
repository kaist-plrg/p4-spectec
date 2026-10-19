//! Command-line emission of K and PLT Redex inputs
//!
//! `kast` and `sexp` emit a SpecTec program or specification as a `script`;
//! `kast-p4` and `sexp-p4` emit a P4 program as a `val`.
//! Output goes to `-o FILE` or stdout, with a trailing newline.
//! Structured diagnostics are rendered at the CLI boundary.

use clap::{Args, Parser, Subcommand};
use p4spectec::{
    lang::data::arena::Arena,
    metacross::{self, Emit, kast::K, render, sexp::Redex},
};
use std::{
    fmt, fs,
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

/// Writes the term and a newline to the output file or stdout.
fn emit(output: Option<&PathBuf>, term: impl fmt::Display) -> io::Result<()> {
    let text = format!("{term}\n");
    match output {
        Some(path) => fs::write(path, text),
        None => io::stdout().lock().write_all(text.as_bytes()),
    }
}

/// The `script` term of a specification, rendering elaboration warnings first.
fn script<E: Emit>(args: &SpecArgs) -> metacross::Result<E::Script> {
    let (result, warnings) = metacross::lower_script(std::slice::from_ref(&args.path));
    for warning in &warnings {
        render(warning);
    }
    E::script(&result?)
}

/// The `val` term of a P4 program.
fn program<E: Emit>(args: &P4Args) -> metacross::Result<E::Program> {
    let mut arena = Arena::new();
    let value = metacross::parse_p4(&mut arena, &args.includes, &args.program)?;
    E::program(&arena, value)
}

fn run(cli: Cli) -> metacross::Result<()> {
    Ok(match &cli.command {
        Command::Kast(args) => emit(args.output.as_ref(), script::<K>(args)?),
        Command::KastP4(args) => emit(args.output.as_ref(), program::<K>(args)?),
        Command::Sexp(args) => emit(args.output.as_ref(), script::<Redex>(args)?),
        Command::SexpP4(args) => emit(args.output.as_ref(), program::<Redex>(args)?),
    }?)
}

fn main() -> ExitCode {
    match run(Cli::parse()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            render(&error.into_report());
            ExitCode::FAILURE
        }
    }
}
