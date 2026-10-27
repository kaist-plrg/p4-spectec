//! Pinned interpreter failures
//!
//! Each source runs in its registered language,
//! applying the registered relation to the registered natural-number arguments.
//! Cases check the runtime failure kind and code before rendering a snapshot.
//! Setup failures and unexpected success fail the test.

use p4spectec::lang::{
    common::source::Span,
    data::{arena::Arena, value::flat as value},
};

use p4spectec::diagnostic::Report;

use p4spectec::interface::p4::P4Interface;

use p4spectec::runner::{Config, InterpreterError, NullExtern, Runner};

use crate::harness::{Result, interpreter};

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

/// Checks that the relation fails on the arguments with the expected kind and code.
fn reject(
    name: &str,
    arena: &mut Arena,
    mut runner: Runner<P4Interface, NullExtern>,
    relation: &str,
    nats: &[u64],
    kind_expect: FailureKind,
    code: &str,
) -> Result<Vec<Report>> {
    let mut values = Vec::new();
    for &nat in nats {
        let value = value::make::nat(arena, nat.into(), Span::default())
            .map_err(|error| failure(name, error))?;
        values.push(value);
    }
    let error = runner
        .eval_rel(arena, relation, &values)
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
    let (Some(language), Some(relation), Some(kind), Some(code)) =
        (case.language, &case.relation, case.kind, &case.code)
    else {
        return Err(failure(name, "interpreter requires languages, relation, kind, and code"));
    };
    let config = Config::new(true, case.det, false);
    let paths = std::slice::from_ref(&case.input);

    // Keep parse, elaboration, lowering, and loading failures out of snapshots
    let mut arena = Arena::new();
    let spec = language.spec(paths).map_err(|error| failure(name, error))?;
    let runner = interpreter::build(&mut arena, spec, config, NullExtern)
        .map_err(|error| failure(name, error))?;
    reject(name, &mut arena, runner, relation, &case.nats, kind, code)
}
