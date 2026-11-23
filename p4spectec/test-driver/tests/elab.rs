//! Elaboration errors and warnings of specification sources
//!
//! Each fixture under `tests/p4spectec/elab/neg` must parse and fail to elaborate;
//! its warnings and error are compared with the snapshot `<stem>.snap` beside it.
//! Each fixture under `tests/p4spectec/elab/pos` must elaborate;
//! its warnings, if any, are compared with its snapshot.

use p4spectec::{diagnostic::RenderConfig, frontend::parse::parse_files, pass};

use p4spectec_test::harness::{
    Outcome,
    fixture::{check, fixtures, must_pass},
};

// = Suites

/// One directory under `tests/p4spectec/`.
struct Fixtures {
    /// Holds the fixtures, relative to `tests/p4spectec/`.
    dir: &'static str,
    /// Names whether elaboration must pass or fail.
    outcome: Outcome,
}

impl Fixtures {
    /// Elaborates each fixture, requires the outcome of the fixtures,
    /// and compares the warnings and any error with the fixture's snapshot.
    fn check(&self) {
        for path in fixtures(self.dir, &[".watsup"]) {
            let spec_el = must_pass(&path, "parsing", parse_files([&path]));
            let (result, mut reports) = pass::elaborate::convert_with_warnings(spec_el);
            match (self.outcome, result) {
                (Outcome::Pass, Ok(_)) => {}
                // Keep the warnings before the error
                (Outcome::Fail, Err(report)) => reports.push(*report),
                (Outcome::Fail, Ok(_)) => {
                    panic!("{}: elaboration unexpectedly passed", path.display())
                }
                (Outcome::Pass, Err(report)) => {
                    panic!("{}: elaboration failed: {report}", path.display())
                }
            }
            check(&path.with_extension("snap"), &reports, &RenderConfig::default());
        }
    }
}

// = Tests

const NEG: Fixtures = Fixtures { dir: "elab/neg", outcome: Outcome::Fail };

#[test]
fn neg() {
    NEG.check();
}

const POS: Fixtures = Fixtures { dir: "elab/pos", outcome: Outcome::Pass };

#[test]
fn pos() {
    POS.check();
}
