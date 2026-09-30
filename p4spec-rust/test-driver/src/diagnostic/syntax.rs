//! Parser failures through production program composition
//!
//! CLI and corpus execution share `Runner::parse_and_eval_program`.
//! Each real interpreter first reaches the probe on valid P4 input;
//! rejected P4 then retains its full report and never reaches that probe.

use std::{cell::Cell, rc::Rc};

use p4spec_rust::{
    diagnostic::{Label, Report, ReportKind},
    interface::p4::{error::P4Error, parse},
    interp::shared::backtrack::Failure,
    lang::{common::source::Span, data::value::Value, il::ast::Typ},
    runner::{
        self, BuiltinInterface, Config, Extern, ExternError, Interface, Interpreter, ProgramError,
        Runner, RunnerContext,
    },
};

use super::{failure, host::reports_equal};
use crate::Result;

/// Counts actual evaluation through an extern in the entry relation.
struct Probe(Rc<Cell<usize>>);

impl Extern for Probe {
    fn eval_rel<Interp, Iface>(
        &self,
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _values: &[Value],
    ) -> std::result::Result<(Vec<Value>, bool), Interp::Error>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        self.0.set(self.0.get() + 1);
        Err(ExternError::Message("evaluation probe".to_owned()).into())
    }

    fn eval_func<Interp, Iface>(
        &self,
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> std::result::Result<(Value, bool), Interp::Error>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        self.0.set(self.0.get() + 1);
        Err(ExternError::Message("evaluation probe".to_owned()).into())
    }

    fn clear(&mut self) {}
}

/// Adds the pinned related item, detail, and trace to a real syntax rejection.
fn syntax_report(error: P4Error) -> P4Error {
    let P4Error::Syntax(mut report) = error else {
        return error;
    };
    if let ReportKind::Cause(diagnostic) = &mut report.kind {
        diagnostic.notes.push("Syntax detail.".to_owned());
        diagnostic
            .labels
            .push(Label::secondary(&Span::default(), "syntax related item"));
    }
    report
        .children
        .push(Report::frame(Span::default(), "syntax trace", Vec::new()));
    P4Error::Syntax(report)
}

/// Compares complete payloads and proves that rejected input bypasses execution.
fn reject<Interp>(
    name: &str,
    mut runner: Runner<Interp, BuiltinInterface, Probe>,
    calls: &Cell<usize>,
) -> Result<Vec<Report>>
where
    Interp: Interpreter<BuiltinInterface, Probe, Error = Failure>,
{
    // Valid input must actually reach the instrumented execution boundary
    let result =
        runner.parse_and_eval_program("R", |arena| parse::parse_string(arena, "input.p4", ""));
    if !matches!(result, Err(ProgramError::Runtime(_))) || calls.get() != 1 {
        return Err(failure(name, "valid input did not reach the evaluation probe"));
    }
    runner.reset();
    calls.set(0);

    // Parse independently to retain an expected payload outside the composition
    let error_expect = parse::parse_string(runner.arena_mut(), "input.p4", "header {")
        .err()
        .ok_or_else(|| failure(name, "invalid P4 unexpectedly parsed"))?;
    let error_expect = syntax_report(error_expect);
    if !matches!(&error_expect, P4Error::Syntax(_)) {
        return Err(failure(name, "fixture failed outside syntax admission"));
    }
    runner.reset();

    // The production parser adapter preserves enriched reports as well as plain ones
    let result = runner.parse_and_eval_program("R", |arena| {
        parse::parse_string(arena, "input.p4", "header {").map_err(syntax_report)
    });
    let Err(ProgramError::Parse(P4Error::Syntax(report))) = result else {
        return Err(failure(name, "syntax rejection lost its parse identity"));
    };
    if calls.get() != 0 {
        return Err(failure(name, "parse rejection evaluated the program"));
    }
    if !reports_equal(&report, error_expect.report()) {
        return Err(failure(name, "syntax diagnostic fields changed at program composition"));
    }
    Ok(Vec::new())
}

/// Exercises the same parse-to-execution operation used by product callers.
pub fn run(name: &str) -> Result<Vec<Report>> {
    let calls = Rc::new(Cell::new(0));
    let config = Config::new(true, false, false);
    let path = "interp/syntax-transport.watsup";
    match name {
        "interp-al-syntax-diagnostic" => {
            let spec = p4spec_rust::algo([path]).map_err(|error| failure(name, error))?;
            let runner = runner::build_al(spec, config, Probe(Rc::clone(&calls)))
                .map_err(|error| failure(name, error))?;
            reject(name, runner, &calls)
        }
        "interp-sl-syntax-diagnostic" => {
            let spec =
                p4spec_rust::structure([path], true).map_err(|error| failure(name, error))?;
            let runner = runner::build_sl(spec, config, Probe(Rc::clone(&calls)))
                .map_err(|error| failure(name, error))?;
            reject(name, runner, &calls)
        }
        "interp-pl-syntax-diagnostic" => {
            let spec = p4spec_rust::prosify([path]).map_err(|error| failure(name, error))?;
            let runner = runner::build_pl(spec, config, Probe(Rc::clone(&calls)))
                .map_err(|error| failure(name, error))?;
            reject(name, runner, &calls)
        }
        _ => Err(failure(name, "unknown syntax transport case")),
    }
}
