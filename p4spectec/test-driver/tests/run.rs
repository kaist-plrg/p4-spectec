//! P4 execution corpora under every interpreter language
//!
//! Positive programs must pass `Program_inst`;
//! negative programs must fail `Program_ok`.
//! Programs listed under the test's exclusion directories are skipped.
//! Each test runs its corpus in one `p4spectec-test-worker`,
//! which gives each program the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set.

use p4spectec_test::harness::corpus::{Corpus, Outcome};

const SAMPLES: &str = "p4c/testdata/p4_16_samples";
const ERRORS: &str = "p4c/testdata/p4_16_errors";
const REGRESSION_POS: &str = "testdata/regression/pos";
const REGRESSION_NEG: &str = "testdata/regression/neg";

const STATIC: &str = "excludes/static";

/// Runs the programs under `dir` through `relation` and expects `outcome` of each,
/// skipping those listed under `dirs_exclude`.
///
/// Panics with the error text, so that its lines print unescaped.
fn run(language: &str, relation: &str, dir: &str, dirs_exclude: &[&str], outcome: Outcome) {
    let result = Corpus::files(dir, ".p4")
        .and_then(|corpus| corpus.excludes(dirs_exclude))
        .and_then(|corpus| {
            corpus.run(env!("CARGO_BIN_EXE_p4spectec-test-worker"), &["run", language, relation])
        })
        .and_then(|report| report.expect_all(outcome));
    if let Err(error) = result {
        panic!("{error}");
    }
}

#[test]
fn pos_al() {
    run("al", "Program_inst", SAMPLES, &[STATIC], Outcome::Pass)
}

#[test]
fn pos_sl() {
    run("sl", "Program_inst", SAMPLES, &[STATIC], Outcome::Pass)
}

#[test]
fn pos_pl() {
    run("pl", "Program_inst", SAMPLES, &[STATIC], Outcome::Pass)
}

#[test]
fn neg_al() {
    run("al", "Program_ok", ERRORS, &[STATIC], Outcome::Fail)
}

#[test]
fn neg_sl() {
    run("sl", "Program_ok", ERRORS, &[STATIC], Outcome::Fail)
}

#[test]
fn neg_pl() {
    run("pl", "Program_ok", ERRORS, &[STATIC], Outcome::Fail)
}

#[test]
fn regression_pos_al() {
    run("al", "Program_inst", REGRESSION_POS, &[STATIC], Outcome::Pass)
}

#[test]
fn regression_pos_sl() {
    run("sl", "Program_inst", REGRESSION_POS, &[STATIC], Outcome::Pass)
}

#[test]
fn regression_pos_pl() {
    run("pl", "Program_inst", REGRESSION_POS, &[STATIC], Outcome::Pass)
}

#[test]
fn regression_neg_al() {
    run("al", "Program_ok", REGRESSION_NEG, &[STATIC], Outcome::Fail)
}

#[test]
fn regression_neg_sl() {
    run("sl", "Program_ok", REGRESSION_NEG, &[STATIC], Outcome::Fail)
}

#[test]
fn regression_neg_pl() {
    run("pl", "Program_ok", REGRESSION_NEG, &[STATIC], Outcome::Fail)
}
