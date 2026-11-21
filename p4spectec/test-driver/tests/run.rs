//! P4 execution corpora under every interpreter language
//!
//! Positive programs run through `Program_inst`, negative ones through `Program_ok`.
//! Programs listed under the test's exclusion directories are skipped.
//! Each test runs its corpus in one `p4spectec-test-worker`,
//! which gives each program the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set,
//! and checks that every program not excluded reaches the suite's outcome.

use p4spectec_test::harness::{
    corpus::{Corpus, Outcome},
    interpreter::Language,
};

const HEADERS: &str = "excludes/headers";
const STATIC: &str = "excludes/static";

/// Worker executable, which Cargo builds before the tests.
const WORKER: &str = env!("CARGO_BIN_EXE_p4spectec-test-worker");

/// Worker options: specification, P4 includes, and the runner options enabled.
const OPTIONS: [&str; 7] =
    ["--spec", "spec", "--include", "p4c/p4include", "--cache", "--det", "--tco"];

/// One execution corpus.
struct Suite {
    /// Names the run log `run-<name>-<language>.log`.
    name: &'static str,
    /// Names the relation each program runs through.
    relation: &'static str,
    /// Names the outcome every program must reach.
    outcome: Outcome,
    /// Holds the programs, relative to the repository root.
    dir: &'static str,
    /// Holds the `.exclude` files of the programs to skip.
    dirs_exclude: &'static [&'static str],
}

/// Runs the programs of `suite` under `language`
/// and checks that each reaches the outcome of `suite`.
///
/// Panics with the error text, so that its lines print unescaped.
fn run(suite: &Suite, language: Language) {
    let result = Corpus::files(suite.dir, ".p4")
        .and_then(|corpus| corpus.excludes(suite.dirs_exclude))
        .and_then(|corpus| {
            let args = [&OPTIONS[..], &["run", language.name(), suite.relation]].concat();
            corpus.run(WORKER, &args, &format!("run-{}-{}", suite.name, language.name()))
        });
    match result {
        Ok(report) => report.check(suite.outcome),
        Err(error) => panic!("{error}"),
    }
}

const POS: Suite = Suite {
    name: "pos",
    relation: "Program_inst",
    outcome: Outcome::Pass,
    dir: "p4c/testdata/p4_16_samples",
    dirs_exclude: &[HEADERS, STATIC],
};

#[test]
fn pos_al() {
    run(&POS, Language::Al);
}

#[test]
fn pos_sl() {
    run(&POS, Language::Sl);
}

#[test]
fn pos_pl() {
    run(&POS, Language::Pl);
}

const NEG: Suite = Suite {
    name: "neg",
    relation: "Program_ok",
    outcome: Outcome::Fail,
    dir: "p4c/testdata/p4_16_errors",
    dirs_exclude: &[STATIC],
};

#[test]
fn neg_al() {
    run(&NEG, Language::Al);
}

#[test]
fn neg_sl() {
    run(&NEG, Language::Sl);
}

#[test]
fn neg_pl() {
    run(&NEG, Language::Pl);
}

const REGRESSION_POS: Suite = Suite {
    name: "regression-pos",
    relation: "Program_inst",
    outcome: Outcome::Pass,
    dir: "testdata/regression/pos",
    dirs_exclude: &[STATIC],
};

#[test]
fn regression_pos_al() {
    run(&REGRESSION_POS, Language::Al);
}

#[test]
fn regression_pos_sl() {
    run(&REGRESSION_POS, Language::Sl);
}

#[test]
fn regression_pos_pl() {
    run(&REGRESSION_POS, Language::Pl);
}

const REGRESSION_NEG: Suite = Suite {
    name: "regression-neg",
    relation: "Program_ok",
    outcome: Outcome::Fail,
    dir: "testdata/regression/neg",
    dirs_exclude: &[STATIC],
};

#[test]
fn regression_neg_al() {
    run(&REGRESSION_NEG, Language::Al);
}

#[test]
fn regression_neg_sl() {
    run(&REGRESSION_NEG, Language::Sl);
}

#[test]
fn regression_neg_pl() {
    run(&REGRESSION_NEG, Language::Pl);
}
