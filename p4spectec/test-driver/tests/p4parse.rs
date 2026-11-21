//! P4 parse/unparse/parse roundtrips
//!
//! A program passes if it parses, prints, and parses again to the same program.
//! Programs listed under the test's exclusion directories are skipped.
//! Each test runs its corpus in one `p4spectec-test-worker`,
//! which gives each program the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set,
//! and checks that every program not excluded passes.

use p4spectec_test::harness::corpus::{Corpus, Outcome};

/// Holds the `.exclude` files of the programs whose roundtrip fails.
const P4PARSE: &str = "excludes/p4parse";

/// Worker executable, which Cargo builds before the tests.
const WORKER: &str = env!("CARGO_BIN_EXE_p4spectec-test-worker");

/// Worker options: specification and P4 includes.
const OPTIONS: [&str; 4] = ["--spec", "spec", "--include", "p4c/p4include"];

/// One roundtrip corpus.
struct Suite {
    /// Names the run log `p4parse-<name>.log`.
    name: &'static str,
    /// Holds the programs, relative to the repository root.
    dir: &'static str,
    /// Holds the `.exclude` files of the programs to skip.
    dirs_exclude: &'static [&'static str],
}

/// Roundtrips the programs of `suite`
/// and checks that each passes.
///
/// Panics with the error text, so that its lines print unescaped.
fn p4parse(suite: &Suite) {
    let result = Corpus::files(suite.dir, ".p4")
        .and_then(|corpus| corpus.excludes(suite.dirs_exclude))
        .and_then(|corpus| {
            let args = [&OPTIONS[..], &["p4parse"]].concat();
            corpus.run(WORKER, &args, &format!("p4parse-{}", suite.name))
        });
    match result {
        Ok(report) => report.check(Outcome::Pass),
        Err(error) => panic!("{error}"),
    }
}

const POS: Suite = Suite {
    name: "pos",
    dir: "p4c/testdata/p4_16_samples",
    dirs_exclude: &["excludes/headers", P4PARSE],
};

#[test]
fn pos() {
    p4parse(&POS);
}

const CUSTOM: Suite = Suite { name: "custom", dir: "testdata/custom", dirs_exclude: &[] };

#[test]
fn custom() {
    p4parse(&CUSTOM);
}

const NEG: Suite =
    Suite { name: "neg", dir: "p4c/testdata/p4_16_errors", dirs_exclude: &[P4PARSE] };

#[test]
fn neg() {
    p4parse(&NEG);
}
