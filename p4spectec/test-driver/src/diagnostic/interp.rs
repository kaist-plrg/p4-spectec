//! Pinned interpreter failures
//!
//! Each source runs through AL, SL, and PL with nat 1 as the input to R.
//! Cases check the runtime failure kind and code before rendering a snapshot.
//! Setup failures and unexpected success fail the test.

use std::path::Path;

use p4spectec::lang::{
    common::source::Span,
    data::{arena::Arena, value::flat as value},
};

use p4spectec::diagnostic::Report;

use p4spectec::interface::p4::P4Interface;

use p4spectec::runner::{self, Config, InterpreterError, NullExtern, Runner};

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

// = Execution

/// Checks that R fails with the expected kind and code.
pub(super) fn reject(
    name: &str,
    arena: &mut Arena,
    mut runner: Runner<P4Interface, NullExtern>,
    kind_expect: FailureKind,
    code: &str,
) -> Result<Vec<Report>> {
    // Keep the entry relation and input common to all cases
    let value =
        value::make::nat(arena, 1.into(), Span::default()).map_err(|error| failure(name, error))?;
    let error = runner
        .eval_rel(arena, "R", &[value])
        .result
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
    let codes_actual = super::codes(&report);
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
    let paths = std::slice::from_ref(&case.input);

    let mut arena = Arena::new();

    // Keep parse, elaboration, lowering, and loading failures out of snapshots
    match stage.as_str() {
        "al" => {
            let spec_al = p4spectec::algo(paths).map_err(|error| failure(name, error))?;
            let runner = runner::build_al(&mut arena, spec_al, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, &mut arena, runner, kind, code)
        }
        "sl" => {
            let spec_sl =
                p4spectec::structure(paths, true).map_err(|error| failure(name, error))?;
            let runner = runner::build_sl(&mut arena, spec_sl, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, &mut arena, runner, kind, code)
        }
        "pl" => {
            let spec_pl = p4spectec::prosify(paths).map_err(|error| failure(name, error))?;
            let runner = runner::build_pl(&mut arena, spec_pl, config, NullExtern)
                .map_err(|error| failure(name, error))?;
            reject(name, &mut arena, runner, kind, code)
        }
        _ => Err(failure(name, "unknown interpreter stage")),
    }
}
