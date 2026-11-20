//! P4 parse/unparse/parse roundtrips
//!
//! Every program must parse, print, and parse again to the same program.
//! Programs listed under the test's exclusion directories are skipped;
//! `excludes/p4parse` names those the parser rejects or does not reproduce.
//! Each test runs its corpus in one `p4spectec-test-worker`,
//! which gives each program the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set.

use p4spectec_test::harness::corpus::{Corpus, Outcome};

/// Worker options: specification and P4 includes.
const OPTIONS: [&str; 4] = ["--spec", "spec", "--include", "p4c/p4include"];

/// Roundtrips the programs under `dir`, skipping those listed under `dirs_exclude`.
///
/// Panics with the error text, so that its lines print unescaped.
fn p4parse(dir: &str, dirs_exclude: &[&str]) {
    let result = Corpus::files(dir, ".p4")
        .and_then(|corpus| corpus.excludes(dirs_exclude))
        .and_then(|corpus| {
            let args = [&OPTIONS[..], &["p4parse"]].concat();
            corpus.run(env!("CARGO_BIN_EXE_p4spectec-test-worker"), &args)
        })
        .and_then(|report| report.expect_all(Outcome::Pass));
    if let Err(error) = result {
        panic!("{error}");
    }
}

#[test]
fn pos() {
    p4parse("p4c/testdata/p4_16_samples", &["excludes/headers", "excludes/p4parse"]);
}

#[test]
fn custom() {
    p4parse("testdata/custom", &["excludes/p4parse"]);
}

#[test]
fn neg() {
    p4parse("p4c/testdata/p4_16_errors", &["excludes/p4parse"]);
}
