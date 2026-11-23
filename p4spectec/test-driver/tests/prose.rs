//! PL conversion errors of specification sources
//!
//! Each fixture under `tests/p4spectec/prose/neg` must structure and fail to convert to PL;
//! its error is compared with the snapshot `<stem>.snap` beside it.

use p4spectec::{diagnostic::RenderConfig, pass};

use p4spectec_test::harness::fixture::{check, fixtures, must_pass};

// = Suites

/// One directory under `tests/p4spectec/`.
struct Fixtures {
    /// Holds the fixtures, relative to `tests/p4spectec/`.
    dir: &'static str,
}

impl Fixtures {
    /// Converts each fixture to PL, which must fail, and compares the error with its snapshot.
    fn check(&self) {
        for path in fixtures(self.dir, &[".watsup"]) {
            let spec_sl = must_pass(
                &path,
                "structuring",
                p4spectec::structure(std::slice::from_ref(&path), false),
            );
            match pass::prosify::convert(spec_sl) {
                Ok(_) => panic!("{}: PL conversion unexpectedly passed", path.display()),
                Err(report) => {
                    check(&path.with_extension("snap"), &[*report], &RenderConfig::default())
                }
            }
        }
    }
}

// = Tests

const NEG: Fixtures = Fixtures { dir: "prose/neg" };

#[test]
fn neg() {
    NEG.check();
}
