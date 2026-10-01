//! Acceptance checks for the diagnostic snapshot harness
//!
//! The driver source is included directly so these tests exercise `reject`.
//! Its binary owns the error type; the shim preserves its formatted message.
//! Fixtures stay with the diagnostic snapshots in the test-driver directory.

#[allow(dead_code)]
#[path = "../test-driver/src/diagnostic/interp.rs"]
mod interp;

use std::path::Path;

use interp::{FailureKind, reject};
use p4spec_rust::{
    diagnostic::Report,
    runner::{self, Config, NullExtern},
};

type Result<T> = std::result::Result<T, String>;

/// Formats harness failures like the driver's diagnostic module.
fn failure(name: &str, message: impl std::fmt::Display) -> String {
    format!("{name}: {message}")
}

/// Runs an AL fixture through the driver's rejection checks.
fn check(case: &str, kind: FailureKind, code: &str) -> Result<Vec<Report>> {
    // Resolve the driver's fixtures from the main crate
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("test-driver/expected/diagnostic/interp")
        .join(case)
        .with_extension("watsup");
    // Keep the same configuration used by the harness acceptance tests
    let spec_al = p4spec_rust::algo(&[path]).unwrap();
    let runner = runner::build_al(spec_al, Config::new(true, false, false), NullExtern).unwrap();
    reject(case, runner, kind, code)
}

#[test]
fn fatal_execution_errors_can_be_snapshotted() {
    let reports =
        check("extern-failed", FailureKind::Fatal, "runtime/extern-unconfigured").unwrap();
    assert_eq!(reports.len(), 1);
}

#[test]
fn mismatches_can_be_snapshotted() {
    let reports = check("backtrack", FailureKind::Mismatch, "runtime/condition-unmet").unwrap();
    assert_eq!(reports.len(), 1);
}

#[test]
fn fatal_execution_errors_cannot_pass_as_mismatches() {
    let error =
        check("extern-failed", FailureKind::Mismatch, "runtime/extern-unconfigured").unwrap_err();
    assert!(error.contains("expected Mismatch, got Fatal"), "{error}");
}

#[test]
fn mismatches_cannot_pass_as_fatal_execution_errors() {
    let error = check("backtrack", FailureKind::Fatal, "runtime/condition-unmet").unwrap_err();
    assert!(error.contains("expected Fatal, got Mismatch"), "{error}");
}

#[test]
fn unrelated_errors_cannot_be_snapshotted() {
    let error =
        check("backtrack", FailureKind::Mismatch, "runtime/extern-unconfigured").unwrap_err();
    assert!(error.contains("missing expected diagnostic runtime/extern-unconfigured"), "{error}");
}

#[test]
fn successful_execution_cannot_be_snapshotted() {
    let error =
        check("relation-nondeterministic", FailureKind::Fatal, "runtime/relation-nondeterministic")
            .unwrap_err();
    assert!(error.contains("relation unexpectedly matched"), "{error}");
}
