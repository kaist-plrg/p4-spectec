//! Parse errors of specification sources
//!
//! Each fixture under `tests/p4spectec/parse/neg` is a `.watsup` source, or a `.mixop` notation type,
//! that must fail to parse;
//! its error is compared with the snapshot `<stem>.snap` beside it.

use std::path::Path;

use p4spectec::{
    diagnostic::{RenderConfig, Report},
    frontend::parse::{parse_files, parse_typ},
};

use p4spectec_test::harness::fixture::{check, fixtures};

// = Helpers

/// Parses the fixture at `path`, a notation type if it is a `.mixop` file.
fn parse(path: &Path) -> Result<(), Box<Report>> {
    // A .mixop file holds a notation type for its own parser entry point
    if path.extension().is_some_and(|ext| ext == "mixop") {
        let text = std::fs::read_to_string(path).unwrap();
        parse_typ(&text).map(drop)
    } else {
        parse_files([path]).map(drop)
    }
}

// = Suites

/// One directory under `tests/p4spectec/`.
struct Fixtures {
    /// Holds the fixtures, relative to `tests/p4spectec/`.
    dir: &'static str,
}

impl Fixtures {
    /// Parses each fixture, which must fail, and compares the error with its snapshot.
    fn check(&self) {
        for path in fixtures(self.dir, &[".watsup", ".mixop"]) {
            match parse(&path) {
                Ok(()) => panic!("{}: parsing unexpectedly passed", path.display()),
                Err(report) => {
                    check(&path.with_extension("snap"), &[*report], &RenderConfig::default())
                }
            }
        }
    }
}

// = Tests

const NEG: Fixtures = Fixtures { dir: "parse/neg" };

#[test]
fn neg() {
    NEG.check();
}
