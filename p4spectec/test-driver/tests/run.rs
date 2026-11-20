//! P4 execution corpora under every interpreter language
//!
//! Positive programs run through `Program_inst`, negative ones through `Program_ok`.
//! Programs listed under the test's exclusion directories are skipped.
//! Each test runs its corpus in one `p4spectec-test-worker`,
//! which gives each program the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set,
//! and compares the outcomes with `expected/run/<suite>.expected`,
//! which the languages share.

use std::path::Path;

use p4spectec_test::harness::corpus::Corpus;

const SAMPLES: &str = "p4c/testdata/p4_16_samples";
const ERRORS: &str = "p4c/testdata/p4_16_errors";
const REGRESSION_POS: &str = "testdata/regression/pos";
const REGRESSION_NEG: &str = "testdata/regression/neg";

const HEADERS: &str = "excludes/headers";
const STATIC: &str = "excludes/static";

/// Worker executable, which Cargo builds before the tests.
const WORKER: &str = env!("CARGO_BIN_EXE_p4spectec-test-worker");

/// Worker options: specification, P4 includes, and the runner options enabled.
const OPTIONS: [&str; 7] =
    ["--spec", "spec", "--include", "p4c/p4include", "--cache", "--det", "--tco"];

/// Runs the programs under `dir` through `relation`, skipping those listed under `dirs_exclude`,
/// and compares the outcomes with the snapshot of `suite`.
///
/// Panics with the error text, so that its lines print unescaped.
fn run(suite: &str, language: &str, relation: &str, dir: &str, dirs_exclude: &[&str]) {
    let result = Corpus::files(dir, ".p4")
        .and_then(|corpus| corpus.excludes(dirs_exclude))
        .and_then(|corpus| {
            let args = [&OPTIONS[..], &["run", language, relation]].concat();
            corpus.run(WORKER, &args)
        });
    match result {
        Ok(report) => report.check(
            &Path::new(env!("CARGO_MANIFEST_DIR")).join(format!("expected/run/{suite}.expected")),
        ),
        Err(error) => panic!("{error}"),
    }
}

#[test]
fn pos_al() {
    run("pos", "al", "Program_inst", SAMPLES, &[HEADERS, STATIC])
}

#[test]
fn pos_sl() {
    run("pos", "sl", "Program_inst", SAMPLES, &[HEADERS, STATIC])
}

#[test]
fn pos_pl() {
    run("pos", "pl", "Program_inst", SAMPLES, &[HEADERS, STATIC])
}

#[test]
fn neg_al() {
    run("neg", "al", "Program_ok", ERRORS, &[STATIC])
}

#[test]
fn neg_sl() {
    run("neg", "sl", "Program_ok", ERRORS, &[STATIC])
}

#[test]
fn neg_pl() {
    run("neg", "pl", "Program_ok", ERRORS, &[STATIC])
}

#[test]
fn regression_pos_al() {
    run("regression-pos", "al", "Program_inst", REGRESSION_POS, &[STATIC])
}

#[test]
fn regression_pos_sl() {
    run("regression-pos", "sl", "Program_inst", REGRESSION_POS, &[STATIC])
}

#[test]
fn regression_pos_pl() {
    run("regression-pos", "pl", "Program_inst", REGRESSION_POS, &[STATIC])
}

#[test]
fn regression_neg_al() {
    run("regression-neg", "al", "Program_ok", REGRESSION_NEG, &[STATIC])
}

#[test]
fn regression_neg_sl() {
    run("regression-neg", "sl", "Program_ok", REGRESSION_NEG, &[STATIC])
}

#[test]
fn regression_neg_pl() {
    run("regression-neg", "pl", "Program_ok", REGRESSION_NEG, &[STATIC])
}
