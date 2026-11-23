//! P4 parse/unparse/parse roundtrips
//!
//! A program passes if it parses, prints, and parses again to the same program;
//! a failure names the step that failed in its details.
//! Programs listed under the test's exclusion directories are skipped.
//! Each corpus test prepares the unparser once, runs its corpus from the repository root,
//! and checks that every program not excluded passes.
//! Each fixture test instead parses each program under `tests/p4spectec/p4parse`, which must fail,
//! and compares the error with the snapshot `<stem>.snap`.

use std::path::{Path, PathBuf};

use p4spectec::{
    interface::p4::{
        error::{P4Error, P4ErrorKind},
        parse::{parse_file, parse_string},
        unparse::P4Unparser,
    },
    lang::{
        common::source::Span,
        data::{arena::Arena, typ},
    },
};
use p4spectec_test::harness::{
    self, Error, Outcome, Result,
    corpus::{Corpus, Report},
    fixture::{check, fixtures, frames_short},
};

/// Holds the `.exclude` files of the programs whose roundtrip fails.
const P4PARSE: &str = "excludes/p4parse";

// = Helpers

/// Parses, prints, and reparses the program at `path`.
fn roundtrip(
    unparser: &P4Unparser,
    paths_include: &[PathBuf],
    path: &Path,
) -> Result<(Outcome, String)> {
    let mut arena = Arena::new();
    let program = match parse_file(&mut arena, paths_include, path) {
        Ok(program) => program,
        Err(error @ P4Error { kind: P4ErrorKind::Syntax(_), .. }) => {
            return Ok((Outcome::Fail, format!("parse: {error}")));
        }
        Err(error) => return Err(failure(path, error)),
    };
    // The parser's root nonterminal is the program's declared type
    let typ_program = typ::make::var(
        p4spectec::phrase!(node: "p4program".into(), span: Span::default()),
        Vec::new(),
    );
    let text = unparser
        .render(&arena, &typ_program, &program)
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    let program_roundtrip = match parse_string(&mut arena, path, &text) {
        Ok(program) => program,
        Err(error @ P4Error { kind: P4ErrorKind::Syntax(_), .. }) => {
            return Ok((Outcome::Fail, format!("reparse of the printed program: {error}")));
        }
        Err(error) => return Err(failure(path, error)),
    };
    // Canonical bodies ignore source spans and type notes, like IL value equality
    if arena.value.canon_id(program.node) == arena.value.canon_id(program_roundtrip.node) {
        Ok((Outcome::Pass, String::new()))
    } else {
        Ok((Outcome::Fail, "roundtrip: the printed program reparses to a different one".into()))
    }
}

/// Names the program in an error that is not a test outcome.
fn failure(path: &Path, error: P4Error) -> Error {
    Error::from(format!("{}: {}", path.display(), error.into_report()))
}

// = Suites

/// One roundtrip corpus.
struct Suite {
    /// Names the run log `p4parse-<name>.log`.
    name: &'static str,
    /// Holds the programs, relative to the repository root.
    dir: &'static str,
    /// Holds the `.exclude` files of the programs to skip.
    dirs_exclude: &'static [&'static str],
}

impl Suite {
    /// Roundtrips the programs of the suite
    /// and checks that each passes.
    ///
    /// Panics with the error text, so that its lines print unescaped.
    fn p4parse(&self) {
        match self.report() {
            Ok(report) => report.check(Outcome::Pass),
            Err(error) => panic!("{error}"),
        }
    }

    /// Roundtrips the programs of the suite from the repository root.
    fn report(&self) -> Result<Report> {
        std::env::set_current_dir(harness::root())?;
        let corpus = Corpus::files(self.dir, ".p4")?.excludes(self.dirs_exclude)?;
        let spec_al = p4spectec::algo(&[PathBuf::from("spec")])?;
        let unparser = P4Unparser::from_al_spec(&spec_al);
        let paths_include = [PathBuf::from("p4c/p4include")];
        corpus.run(&format!("p4parse-{}", self.name), |paths| {
            roundtrip(&unparser, &paths_include, &paths[0])
        })
    }
}

/// One directory of programs under `tests/p4spectec/`.
struct Fixtures {
    /// Holds the programs, relative to `tests/p4spectec/`.
    dir: &'static str,
}

impl Fixtures {
    /// Parses each program, which must fail with a syntax error,
    /// and compares the error with the program's snapshot.
    fn check(&self) {
        for path in fixtures(self.dir, &[".p4"]) {
            let text = std::fs::read_to_string(&path).unwrap();
            match parse_string(&mut Arena::new(), &path, &text) {
                Ok(_) => panic!("{}: P4 parsing unexpectedly passed", path.display()),
                Err(error) => {
                    assert!(
                        matches!(error.kind, P4ErrorKind::Syntax(_)),
                        "{}: failed outside syntax admission",
                        path.display()
                    );
                    check(&path.with_extension("snap"), &[*error.into_report()], &frames_short());
                }
            }
        }
    }
}

// = Tests

const P4C_POS: Suite = Suite {
    name: "p4c-pos",
    dir: "p4c/testdata/p4_16_samples",
    dirs_exclude: &["excludes/headers", P4PARSE],
};

#[test]
fn p4c_pos() {
    P4C_POS.p4parse();
}

const CUSTOM: Suite = Suite { name: "custom", dir: "testdata/custom", dirs_exclude: &[] };

#[test]
fn custom() {
    CUSTOM.p4parse();
}

const P4C_NEG: Suite =
    Suite { name: "p4c-neg", dir: "p4c/testdata/p4_16_errors", dirs_exclude: &[P4PARSE] };

#[test]
fn p4c_neg() {
    P4C_NEG.p4parse();
}

const FIXTURE_NEG: Fixtures = Fixtures { dir: "p4parse/neg" };

#[test]
fn fixture_neg() {
    FIXTURE_NEG.check();
}
