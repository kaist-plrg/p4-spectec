//! P4 execution corpora under every interpreter language
//!
//! Positive programs run through `Program_inst`, negative ones through `Program_ok`.
//! Programs listed under the test's exclusion directories are skipped.
//! Each corpus test prepares the specification once, runs its corpus from the repository root,
//! and checks that every program not excluded reaches the suite's outcome.
//! Each fixture test instead applies a relation of each specification under `tests/p4spectec/run`:
//! `R` to 1 must fail, with its error compared to the snapshot `<stem>.snap`
//! (or `<stem>.<language>.snap`), and `Main` must hold.

use std::path::{Path, PathBuf};

use p4spectec::{
    SpecLang,
    interface::{
        self,
        p4::{
            P4Interface,
            error::{P4Error, P4ErrorKind},
            parse::parse_string,
            preprocessor::preprocess,
        },
    },
    lang::{
        common::source::Span,
        data::{arena::Arena, value::flat as value},
    },
    runner::{Config, NullExtern, Runner},
};
use p4spectec_sim::Dummy;
use p4spectec_test::harness::{
    self, Error, Outcome, Result,
    corpus::{Corpus, Report},
    fixture::{check, fixtures, frames_short, must_pass},
};

const HEADERS: &str = "excludes/headers";
const STATIC: &str = "excludes/static";

/// Runner options of every corpus.
const CONFIG: Config = Config { cache: true, det: true, guard: false, tco: true };

// = Helpers

/// Evaluates the program at `path` through `relation`.
fn eval(
    arena: &mut Arena,
    runner: &mut Runner<P4Interface, Dummy>,
    paths_include: &[PathBuf],
    relation: &str,
    path: &Path,
) -> Result<Outcome> {
    let source = preprocess(paths_include, path).map_err(|error| failure(path, error))?;
    // Reset before parsing: no value may cross this program boundary
    runner.reset();
    arena.reset_values();
    match parse_string(arena, path, &source) {
        // Evaluate a parsed program
        Ok(program) => match runner.eval_program(arena, relation, program).result {
            Ok(_) => Ok(Outcome::Pass),
            Err(_) => Ok(Outcome::Fail),
        },
        // Reject a program with a syntax error
        Err(P4Error { kind: P4ErrorKind::Syntax(_), .. }) => Ok(Outcome::Fail),
        // Report any other parser failure as an execution error
        Err(error) => Err(failure(path, error)),
    }
}

/// Names the program in an error that is not a test outcome.
fn failure(path: &Path, error: P4Error) -> Error {
    Error::from(format!("{}: {}", path.display(), error.into_report()))
}

// = Suites

/// One execution corpus.
struct Suite {
    /// Names the run log `run-<name>-<language>.log`.
    name: &'static str,
    /// Names the relation each program runs through.
    relation: &'static str,
    /// Names the outcome every program must reach.
    outcome: Outcome,
    /// Holds the programs, relative to the repository root.
    dir: &'static str,
    /// Holds the `.exclude` files of the programs to skip.
    dirs_exclude: &'static [&'static str],
}

impl Suite {
    /// Runs the programs of the suite under `language`
    /// and checks that each reaches the outcome of the suite.
    ///
    /// Panics with the error text, so that its lines print unescaped.
    fn run(&self, language: SpecLang) {
        match self.report(language) {
            Ok(report) => report.check(self.outcome),
            Err(error) => panic!("{error}"),
        }
    }

    /// Runs the programs of the suite under `language` from the repository root.
    fn report(&self, language: SpecLang) -> Result<Report> {
        std::env::set_current_dir(harness::root())?;
        let corpus = Corpus::files(self.dir, ".p4")?.excludes(self.dirs_exclude)?;
        let mut arena = Arena::new();
        let spec = p4spectec::runner_spec_with_warnings(language, &[PathBuf::from("spec")]).0?;
        let interface = interface::p4(&spec);
        let mut runner = Runner::new(&mut arena, spec, CONFIG, interface, Dummy::default())?;
        let paths_include = [PathBuf::from("p4c/p4include")];
        corpus.run(
            &format!("run-{}-{}", self.name, format!("{language:?}").to_lowercase()),
            |paths| {
                let outcome =
                    eval(&mut arena, &mut runner, &paths_include, self.relation, &paths[0])?;
                Ok((outcome, String::new()))
            },
        )
    }
}

/// One directory of specifications under `tests/p4spectec/`, each applying one relation.
struct Fixtures {
    /// Holds the specifications, relative to `tests/p4spectec/`.
    dir: &'static str,
    /// Names the relation applied in each specification.
    relation: &'static str,
    /// Holds the natural numbers the relation is applied to.
    nats: &'static [u64],
    /// Names whether the application must pass or fail.
    outcome: Outcome,
}

impl Fixtures {
    /// Applies the relation of the fixtures in each specification under `language`,
    /// requires their outcome, and compares a failure's report with the fixture's snapshot.
    fn check(&self, language: SpecLang) {
        let name_language = format!("{language:?}").to_lowercase();
        for path in fixtures(self.dir, &[".watsup"]) {
            // Build a runner of the specification in the language
            let mut arena = Arena::new();
            let spec = must_pass(
                &path,
                "conversion",
                p4spectec::runner_spec_with_warnings(language, std::slice::from_ref(&path)).0,
            );
            let interface = interface::p4(&spec);
            let mut runner = must_pass(
                &path,
                "runner build",
                Runner::new(&mut arena, spec, CONFIG, interface, NullExtern),
            );
            // Apply the relation to the natural numbers
            let values: Vec<_> = self
                .nats
                .iter()
                .map(|&nat| value::make::nat(&mut arena, nat.into(), Span::default()).unwrap())
                .collect();
            let result = runner.eval_rel(&mut arena, self.relation, &values).result;
            match (self.outcome, result) {
                (Outcome::Pass, Ok(_)) => {}
                (Outcome::Fail, Err(error)) => {
                    // A language's own expectation overrides the shared one
                    let path_language = path.with_extension(format!("{name_language}.snap"));
                    let path_expected = if path_language.exists() {
                        path_language
                    } else {
                        path.with_extension("snap")
                    };
                    check(&path_expected, &[*error.into_report()], &frames_short());
                }
                (Outcome::Fail, Ok(_)) => panic!(
                    "{}: {} in {name_language} unexpectedly passed",
                    path.display(),
                    self.relation
                ),
                (Outcome::Pass, Err(error)) => panic!(
                    "{}: {} in {name_language} failed: {}",
                    path.display(),
                    self.relation,
                    error.into_report()
                ),
            }
        }
    }
}

// = Tests

const P4C_POS: Suite = Suite {
    name: "p4c-pos",
    relation: "Program_inst",
    outcome: Outcome::Pass,
    dir: "p4c/testdata/p4_16_samples",
    dirs_exclude: &[HEADERS, STATIC],
};

#[test]
fn p4c_pos_al() {
    P4C_POS.run(SpecLang::Al);
}

#[test]
fn p4c_pos_sl() {
    P4C_POS.run(SpecLang::Sl);
}

#[test]
fn p4c_pos_pl() {
    P4C_POS.run(SpecLang::Pl);
}

const P4C_NEG: Suite = Suite {
    name: "p4c-neg",
    relation: "Program_ok",
    outcome: Outcome::Fail,
    dir: "p4c/testdata/p4_16_errors",
    dirs_exclude: &[STATIC],
};

#[test]
fn p4c_neg_al() {
    P4C_NEG.run(SpecLang::Al);
}

#[test]
fn p4c_neg_sl() {
    P4C_NEG.run(SpecLang::Sl);
}

#[test]
fn p4c_neg_pl() {
    P4C_NEG.run(SpecLang::Pl);
}

const REGRESSION_POS: Suite = Suite {
    name: "regression-pos",
    relation: "Program_inst",
    outcome: Outcome::Pass,
    dir: "testdata/regression/pos",
    dirs_exclude: &[STATIC],
};

#[test]
fn regression_pos_al() {
    REGRESSION_POS.run(SpecLang::Al);
}

#[test]
fn regression_pos_sl() {
    REGRESSION_POS.run(SpecLang::Sl);
}

#[test]
fn regression_pos_pl() {
    REGRESSION_POS.run(SpecLang::Pl);
}

const REGRESSION_NEG: Suite = Suite {
    name: "regression-neg",
    relation: "Program_ok",
    outcome: Outcome::Fail,
    dir: "testdata/regression/neg",
    dirs_exclude: &[STATIC],
};

#[test]
fn regression_neg_al() {
    REGRESSION_NEG.run(SpecLang::Al);
}

#[test]
fn regression_neg_sl() {
    REGRESSION_NEG.run(SpecLang::Sl);
}

#[test]
fn regression_neg_pl() {
    REGRESSION_NEG.run(SpecLang::Pl);
}

const FIXTURE_NEG: Fixtures =
    Fixtures { dir: "run/neg", relation: "R", nats: &[1], outcome: Outcome::Fail };

#[test]
fn fixture_neg_al() {
    FIXTURE_NEG.check(SpecLang::Al);
}

#[test]
fn fixture_neg_sl() {
    FIXTURE_NEG.check(SpecLang::Sl);
}

#[test]
fn fixture_neg_pl() {
    FIXTURE_NEG.check(SpecLang::Pl);
}

const FIXTURE_POS: Fixtures =
    Fixtures { dir: "run/pos", relation: "Main", nats: &[], outcome: Outcome::Pass };

#[test]
fn fixture_pos_al() {
    FIXTURE_POS.check(SpecLang::Al);
}

#[test]
fn fixture_pos_sl() {
    FIXTURE_POS.check(SpecLang::Sl);
}

#[test]
fn fixture_pos_pl() {
    FIXTURE_POS.check(SpecLang::Pl);
}
