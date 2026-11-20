//! P4 parse/unparse/parse roundtrips
//!
//! A program passes if it parses, prints, and parses again to the same program.
//! Programs listed under the test's exclusion directories are skipped.
//! Each test runs its corpus in one `p4spectec-test-worker`,
//! which gives each program the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set,
//! and compares the outcomes with `expected/p4parse/<suite>.expected`.

use std::path::Path;

use p4spectec_test::harness::corpus::Corpus;

/// Worker options: specification and P4 includes.
const OPTIONS: [&str; 4] = ["--spec", "spec", "--include", "p4c/p4include"];

/// Roundtrips the programs under `dir`, skipping those listed under `dirs_exclude`,
/// and compares the outcomes with the snapshot of `suite`.
///
/// Panics with the error text, so that its lines print unescaped.
fn p4parse(suite: &str, dir: &str, dirs_exclude: &[&str]) {
    let result = Corpus::files(dir, ".p4")
        .and_then(|corpus| corpus.excludes(dirs_exclude))
        .and_then(|corpus| {
            let args = [&OPTIONS[..], &["p4parse"]].concat();
            corpus.run(env!("CARGO_BIN_EXE_p4spectec-test-worker"), &args)
        });
    match result {
        Ok(report) => report.check(
            &Path::new(env!("CARGO_MANIFEST_DIR"))
                .join(format!("expected/p4parse/{suite}.expected")),
        ),
        Err(error) => panic!("{error}"),
    }
}

#[test]
fn pos() {
    p4parse("pos", "p4c/testdata/p4_16_samples", &["excludes/headers"]);
}

#[test]
fn custom() {
    p4parse("custom", "testdata/custom", &[]);
}

#[test]
fn neg() {
    p4parse("neg", "p4c/testdata/p4_16_errors", &[]);
}
