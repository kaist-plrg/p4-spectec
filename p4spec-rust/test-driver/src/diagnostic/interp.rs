//! Pinned interpreter failures
//!
//! Each source runs through AL, SL, and PL with nat 1 as the input to R.
//! Cases check the runtime failure kind and code before rendering a snapshot.
//! Setup failures and unexpected success fail the test.

use super::failure;
use crate::Result;
use p4spec_rust::{
    diagnostic::{Report, ReportKind},
    interp::shared::backtrack::Failure,
    lang::{common::source::Span, data::value::make},
    runner::{self, BuiltinInterface, Config, Interpreter, NullExtern, Runner},
};

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
    Interp: Interpreter<BuiltinInterface, NullExtern, Error = Failure>,
{
    // Keep the entry relation and input common to all cases
    let value = make::nat(runner.arena_mut(), 1.into(), Span::default())
        .map_err(|error| failure(name, error))?;
    let error = runner
        .context()
        .call_rel("R", &[value])
        .err()
        .ok_or_else(|| failure(name, "relation unexpectedly matched"))?;

    // Check recovery behavior before converting the failure to a report
    let kind = match &error {
        Failure::Fatal(_) => FailureKind::Fatal,
        Failure::Mismatch(_) => FailureKind::Mismatch,
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
pub fn run(name: &str) -> Result<Vec<Report>> {
    // Split the stage from the shared source case
    let (case, stage) = name
        .rsplit_once("-interp-")
        .ok_or_else(|| failure(name, "invalid interpreter diagnostic case"))?;
    let (kind, code) = match case {
        "backtrack" | "deepest-failure" | "later-tie" => {
            (FailureKind::Mismatch, "runtime/condition-unmet")
        }
        "index-out-of-bounds" | "nested-call" => {
            // AL inserts a bounds condition before evaluating an index
            (FailureKind::Mismatch, "runtime/condition-unmet")
        }
        "slice-out-of-bounds" => (FailureKind::Fatal, "runtime/slice-out-of-bounds"),
        "numeric-invalid" => (FailureKind::Fatal, "runtime/numeric-invalid"),
        "builtin-failed" => (FailureKind::Mismatch, "runtime/builtin-failed"),
        "hold-failed" | "hold-iter-failed" => {
            (FailureKind::Mismatch, "runtime/hold-condition-unmet")
        }
        "not-hold-failed" => (FailureKind::Mismatch, "runtime/not-hold-condition-unmet"),
        "extern-failed" | "fatal-skips-otherwise" | "builtin-fallback" | "hold-fatal" => {
            (FailureKind::Fatal, "runtime/extern-failed")
        }
        "relation-nondeterministic" if stage == "al" => {
            (FailureKind::Fatal, "runtime/relation-nondeterministic")
        }
        "function-nondeterministic" if stage == "al" => {
            (FailureKind::Fatal, "runtime/function-nondeterministic")
        }
        "relation-nondeterministic" | "function-nondeterministic" => {
            (FailureKind::Fatal, "runtime/instruction-nondeterministic")
        }
        _ => return Err(failure(name, "unknown interpreter diagnostic case")),
    };

    // Enable only the checks needed by each case
    let det = matches!(case, "relation-nondeterministic" | "function-nondeterministic");
    let config = Config::new(true, det, false);
    let path = format!("interp/{case}.watsup");

    // Keep parse, elaboration, lowering, and loading failures out of snapshots
    match stage {
        "al" => {
            let spec_al = p4spec_rust::algo([path]).map_err(|error| failure(name, error))?;
            let runner = runner::build_al(spec_al, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, runner, kind, code)
        }
        "sl" => {
            let spec_sl =
                p4spec_rust::structure([path], true).map_err(|error| failure(name, error))?;
            let runner = runner::build_sl(spec_sl, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, runner, kind, code)
        }
        "pl" => {
            let spec_pl = p4spec_rust::prosify([path]).map_err(|error| failure(name, error))?;
            let runner = runner::build_pl(spec_pl, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, runner, kind, code)
        }
        _ => Err(failure(name, "unknown interpreter stage")),
    }
}
