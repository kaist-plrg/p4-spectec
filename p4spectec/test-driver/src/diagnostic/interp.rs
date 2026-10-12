//! Pinned interpreter failures
//!
//! Each source runs through AL, SL, and PL with nat 1 as the input to R.
//! Cases check the runtime failure kind and code before rendering a snapshot.
//! Setup failures and unexpected success fail the test.

use std::path::Path;

use p4spectec::lang::{common::source::Span, data::value::flat as value};

use p4spectec::diagnostic::{Report, ReportKind};

use p4spectec::runner::{
    self, BuiltinInterface, Config, Interpreter, InterpreterError, NullExtern, Runner,
};

use crate::Result;

use super::{Case, failure};

/// Loads and compares this module's diagnostic fixtures.
pub fn run(path: &Path, path_cli: Option<&Path>) -> Result<()> {
    let groups = super::load(path, false)?;
    super::run_registered(
        "interp",
        &groups,
        path_cli,
        Some(run_case),
        p4spectec::diagnostic::RenderConfig {
            frame_style: Some(p4spectec::diagnostic::DisplayStyle::Short),
            ..Default::default()
        },
    )
}

// = Expectations

/// Distinguishes fatal failures from recoverable mismatches.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum FailureKind {
    Fatal,
    Mismatch,
}

/// Collects runtime codes beneath invocation and expression frames.
fn codes(report: &Report) -> Vec<&str> {
    let mut codes_found = Vec::new();
    let mut pending = vec![report];
    while let Some(report) = pending.pop() {
        // Inspect causes without depending on rendered text
        if let ReportKind::Cause(diagnostic) = &report.kind
            && let Some(code) = diagnostic.code.as_deref()
        {
            codes_found.push(code);
        }
        // Include causes nested under operation frames
        pending.extend(&report.children);
    }
    codes_found
}

// = Execution

/// Checks that R fails with the expected kind and code.
pub(super) fn reject<Interp>(
    name: &str,
    mut runner: Runner<Interp, BuiltinInterface, NullExtern>,
    kind_expect: FailureKind,
    code: &str,
) -> Result<Vec<Report>>
where
    Interp: Interpreter<BuiltinInterface, NullExtern>,
{
    // Keep the entry relation and input common to all cases
    let value = value::make::nat(runner.arena_mut(), 1.into(), Span::default())
        .map_err(|error| failure(name, error))?;
    let error = runner
        .context()
        .call_rel("R", &[value])
        .err()
        .ok_or_else(|| failure(name, "relation unexpectedly matched"))?;

    // Check recovery behavior before converting the failure to a report
    let kind = match &error {
        InterpreterError::Fatal(_) => FailureKind::Fatal,
        InterpreterError::Mismatch(_) => FailureKind::Mismatch,
    };
    if kind != kind_expect {
        return Err(failure(name, format!("expected {kind_expect:?}, got {kind:?}")));
    }

    // Reject unrelated runtime errors even when updating snapshots
    let report = error.into_report();
    let codes_actual = codes(&report);
    if !codes_actual.contains(&code) {
        return Err(failure(
            name,
            format!("missing expected diagnostic {code}, got {codes_actual:?}"),
        ));
    }
    Ok(vec![*report])
}

// = Cases

/// Runs a local negative case through its selected interpreter.
fn run_case(case: &Case) -> Result<Vec<Report>> {
    // P4 syntax fixtures use the production source parser
    if case.input.extension().is_some_and(|ext| ext == "p4") {
        return super::syntax::run(case);
    }
    let name = case.name.as_str();
    // Interpreter language and acceptance guards come from the registration
    let [stage, arg_kind, code, arg_det] = case.args.as_slice() else {
        return Err(failure(name, "interpreter requires stage, kind, code, and det arguments"));
    };
    let kind = match arg_kind.as_str() {
        "fatal" => FailureKind::Fatal,
        "mismatch" => FailureKind::Mismatch,
        _ => return Err(failure(name, "invalid interpreter failure kind")),
    };
    let det = arg_det.parse().map_err(|error| failure(name, error))?;
    let config = Config::new(true, det, false);
    let path = case.path_input();

    // Keep parse, elaboration, lowering, and loading failures out of snapshots
    match stage.as_str() {
        "al" => {
            let spec_al = p4spectec::algo(&[path]).map_err(|error| failure(name, error))?;
            let runner = runner::build_al(spec_al, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, runner, kind, code)
        }
        "sl" => {
            let spec_sl =
                p4spectec::structure(&[path], true).map_err(|error| failure(name, error))?;
            let runner = runner::build_sl(spec_sl, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, runner, kind, code)
        }
        "pl" => {
            let spec_pl = p4spectec::prosify(&[path]).map_err(|error| failure(name, error))?;
            let runner = runner::build_pl(spec_pl, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, runner, kind, code)
        }
        _ => Err(failure(name, "unknown interpreter stage")),
    }
}
