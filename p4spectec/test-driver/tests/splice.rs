//! Splicing errors and warnings of AsciiDoc skeletons
//!
//! Each fixture under `tests/p4spectec/splice/neg` is a skeleton
//! that must fail to splice against `tests/p4spectec/splice/minimal.watsup`;
//! each under `tests/p4spectec/splice/pos` must splice.
//! The warnings and any error are compared with the snapshot `<stem>.snap` beside the skeleton.

use std::path::PathBuf;

use p4spectec::{diagnostic::RenderConfig, frontend::parse::parse_files, specdoc::splicer};

use p4spectec_test::harness::{
    Outcome,
    fixture::{check, fixtures, must_pass},
};

// = Suites

/// One directory of skeletons under `tests/p4spectec/`.
struct Fixtures {
    /// Holds the skeletons, relative to `tests/p4spectec/`.
    dir: &'static str,
    /// Names the specification the skeletons splice against, relative to `tests/p4spectec/`.
    spec: &'static str,
    /// Names whether splicing must pass or fail.
    outcome: Outcome,
}

impl Fixtures {
    /// Splices each skeleton, requires the outcome of the fixtures,
    /// and compares the warnings and any error with the skeleton's snapshot.
    fn check(&self) {
        for path in fixtures(self.dir, &[".adoc"]) {
            let paths_spec = [PathBuf::from(self.spec)];
            let spec_el = must_pass(&path, "parsing", parse_files(&paths_spec));
            let spec_pl = must_pass(&path, "PL conversion", p4spectec::prosify(&paths_spec));
            let text = std::fs::read_to_string(&path).unwrap();
            let text_path = path.to_string_lossy();
            let (result, mut reports) =
                splicer::splice_strings_with_warnings(&spec_el, &spec_pl, &[(&text_path, &text)]);
            match (self.outcome, result) {
                (Outcome::Pass, Ok(_)) => {}
                (Outcome::Fail, Err(report)) => reports.push(*report),
                (Outcome::Fail, Ok(_)) => {
                    panic!("{}: splicing unexpectedly passed", path.display())
                }
                (Outcome::Pass, Err(report)) => {
                    panic!("{}: splicing failed: {report}", path.display())
                }
            }
            check(&path.with_extension("snap"), &reports, &RenderConfig::default());
        }
    }
}

// = Tests

const NEG: Fixtures =
    Fixtures { dir: "splice/neg", spec: "splice/minimal.watsup", outcome: Outcome::Fail };

#[test]
fn neg() {
    NEG.check();
}

const POS: Fixtures =
    Fixtures { dir: "splice/pos", spec: "splice/minimal.watsup", outcome: Outcome::Pass };

#[test]
fn pos() {
    POS.check();
}
