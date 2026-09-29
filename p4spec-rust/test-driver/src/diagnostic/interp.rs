//! Pinned interpreter backtracking failures
//!
//! The same source is lowered through each public pipeline and run on nat 1.
//! Setup errors, fatal failures, and unexpected success cannot become snapshots.

use super::failure;
use crate::Result;
use p4spec_rust::{
    diagnostic::Report,
    interp::shared::backtrack::Failure,
    lang::{common::source::Span, data::value::make},
    runner::{self, BuiltinInterface, Config, Interpreter, NullExtern, Runner},
};

// = Execution

/// Evaluates R and preserves its exhausted mismatch for final rendering.
fn reject<Interp>(
    name: &str,
    mut runner: Runner<Interp, BuiltinInterface, NullExtern>,
) -> Result<Vec<Report>>
where
    Interp: Interpreter<BuiltinInterface, NullExtern, Error = Failure>,
{
    // Match the reference harness's entry relation and nat input
    let value = make::nat(runner.arena_mut(), 1.into(), Span::default())
        .map_err(|error| failure(name, error))?;
    // Accept only recoverable exhaustion at the actual interpreter boundary
    match runner.context().call_rel("R", &[value]) {
        Err(failure @ Failure::Mismatch(_)) => Ok(vec![*failure.into_report()]),
        Err(Failure::Fatal(report)) => {
            Err(failure(name, format!("unexpected fatal failure: {report}")))
        }
        Ok(_) => Err(failure(name, "relation unexpectedly matched")),
    }
}

// = Cases

/// Runs a pinned local backtracking case through its selected interpreter.
pub fn run(name: &str) -> Result<Vec<Report>> {
    let path = "interp/backtrack.watsup";
    let config = Config::new(true, false, false);
    // Earlier-stage or loading failures remain setup failures
    match name {
        "interp-al-backtrack" => {
            let spec_al = p4spec_rust::algo([path]).map_err(|error| failure(name, error))?;
            let runner = runner::build_al(spec_al, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, runner)
        }
        "interp-sl-backtrack" => {
            let spec_sl =
                p4spec_rust::structure([path], true).map_err(|error| failure(name, error))?;
            let runner = runner::build_sl(spec_sl, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, runner)
        }
        "interp-pl-backtrack" => {
            let spec_pl = p4spec_rust::prosify([path]).map_err(|error| failure(name, error))?;
            let runner = runner::build_pl(spec_pl, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, runner)
        }
        _ => Err(failure(name, "unknown interpreter diagnostic case")),
    }
}
