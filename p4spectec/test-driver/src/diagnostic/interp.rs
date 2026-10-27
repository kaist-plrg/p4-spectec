//! Pinned interpreter failures
//!
//! Each source runs through AL, SL, and PL with nat 1 as the input to R.
//! Cases check the runtime failure kind and code before rendering a snapshot.
//! Setup failures and unexpected success fail the test.

use p4spectec::lang::{
    common::source::Span,
    data::{arena::Arena, value::flat as value},
};

use p4spectec::diagnostic::Report;

use p4spectec::interface::p4::P4Interface;

use p4spectec::runner::{Config, InterpreterError, NullExtern, Runner};

use crate::{
    Result,
    util::{self},
};

use serde::Deserialize;

use super::{Case, failure};

// = Expectations

/// Distinguishes fatal failures from recoverable mismatches.
#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum FailureKind {
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
pub(super) fn run_case(case: &Case) -> Result<Vec<Report>> {
    // P4 syntax fixtures use the production source parser
    if case.input.extension().is_some_and(|ext| ext == "p4") {
        return super::syntax::run(case);
    }
    let name = case.name.as_str();
    // Interpreter language and acceptance guards come from the registration
    let (Some(language), Some(kind), Some(code)) = (case.language, case.kind, &case.code) else {
        return Err(failure(name, "interpreter requires languages, kind, and code"));
    };
    let config = Config::new(true, case.det, false);
    let paths = std::slice::from_ref(&case.input);

    // Keep parse, elaboration, lowering, and loading failures out of snapshots
    let mut arena = Arena::new();
    let spec = language.spec(paths).map_err(|error| failure(name, error))?;
    let runner =
        util::build(&mut arena, spec, config, NullExtern).map_err(|error| failure(name, error))?;
    reject(name, &mut arena, runner, kind, code)
}
