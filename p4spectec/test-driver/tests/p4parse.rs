//! P4 parse/unparse/parse roundtrips
//!
//! A program passes if it parses, prints, and parses again to the same program.
//! Programs listed under a test's exclusion directories are skipped;
//! `excludes/p4parse` names those the parser rejects or does not reproduce,
//! and `excludes/p4parse-reject` the error programs it must reject,
//! which `neg_reject` checks.
//! Each test runs its corpus in one `p4spectec-test-worker`,
//! which gives each program the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set.

use p4spectec_test::harness::{
    Result,
    corpus::{Corpus, Outcome},
};

/// Worker options: specification and P4 includes.
const OPTIONS: [&str; 4] = ["--spec", "spec", "--include", "p4c/p4include"];

/// Roundtrips every program of `corpus`, each of which must reach `outcome`.
///
/// Panics with the error text, so that its lines print unescaped.
fn p4parse(corpus: Result<Corpus>, outcome: Outcome) {
    let result = corpus
        .and_then(|corpus| {
            let args = [&OPTIONS[..], &["p4parse"]].concat();
            corpus.run(env!("CARGO_BIN_EXE_p4spectec-test-worker"), &args)
        })
        .and_then(|report| report.expect_all(outcome));
    if let Err(error) = result {
        panic!("{error}");
    }
}

#[test]
fn pos() {
    let corpus = Corpus::files("p4c/testdata/p4_16_samples", ".p4")
        .and_then(|corpus| corpus.excludes(&["excludes/headers", "excludes/p4parse"]));
    p4parse(corpus, Outcome::Pass);
}

#[test]
fn custom() {
    let corpus = Corpus::files("testdata/custom", ".p4")
        .and_then(|corpus| corpus.excludes(&["excludes/p4parse"]));
    p4parse(corpus, Outcome::Pass);
}

#[test]
fn neg() {
    let corpus = Corpus::files("p4c/testdata/p4_16_errors", ".p4")
        .and_then(|corpus| corpus.excludes(&["excludes/p4parse", "excludes/p4parse-reject"]));
    p4parse(corpus, Outcome::Pass);
}

#[test]
fn neg_reject() {
    p4parse(Corpus::listed("excludes/p4parse-reject"), Outcome::ParseFail);
}
