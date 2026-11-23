//! AL conversion errors of specification sources
//!
//! Each fixture under `tests/p4spectec/algo/neg` must elaborate and fail to convert to AL;
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
    /// Converts each fixture to AL, which must fail, and compares the error with its snapshot.
    fn check(&self) {
        for path in fixtures(self.dir, &[".watsup"]) {
            let spec_il =
                must_pass(&path, "elaboration", p4spectec::elab(std::slice::from_ref(&path)));
            match pass::algo::convert(spec_il) {
                Ok(_) => panic!("{}: AL conversion unexpectedly passed", path.display()),
                Err(report) => {
                    check(&path.with_extension("snap"), &[*report], &RenderConfig::default())
                }
            }
        }
    }
}

// = Tests

const NEG: Fixtures = Fixtures { dir: "algo/neg" };

#[test]
fn neg() {
    NEG.check();
}
