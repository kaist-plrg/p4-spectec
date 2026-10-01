//! Public specification preparation and program execution

use std::{fs, path::PathBuf};

use p4spec_rust::{
    RunError, SpecLang, lang::traits::print::Print, runner, runner_spec_with_warnings,
    specdoc_spec_with_warnings,
};

#[path = "support/directory.rs"]
mod directory;

fn fixture(path: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(path)
}

#[test]
fn runner_spec_selects_each_language_and_executes_programs() {
    let paths = [fixture("cli/run/types.watsup"), fixture("cli/run/relations.watsup")];
    for lang in [SpecLang::Al, SpecLang::Sl, SpecLang::Pl] {
        let (result, warnings) = runner_spec_with_warnings(lang, &paths);
        assert!(warnings.is_empty());
        let spec = result.unwrap();
        assert!(matches!(
            (lang, &spec),
            (SpecLang::Al, runner::Spec::Al(_))
                | (SpecLang::Sl, runner::Spec::Sl(_))
                | (SpecLang::Pl, runner::Spec::Pl(_))
        ));
        p4spec_rust::run(
            spec,
            runner::Config::new(true, false, false),
            "Pass",
            &[],
            &fixture("cli/run/empty.p4"),
        )
        .unwrap();
    }
}

#[test]
fn runner_spec_retains_warnings_on_conversion_failure() {
    let directory = directory::Directory::new("runner-spec-warning");
    let path = directory.0.join("warning.watsup");
    fs::write(&path, "dec $missing : nat\n").unwrap();
    let paths = [path, fixture("algorithmic/impure_else_premises.watsup")];
    for lang in [SpecLang::Al, SpecLang::Sl, SpecLang::Pl] {
        let (result, warnings) = runner_spec_with_warnings(lang, &paths);
        assert!(result.is_err());
        assert_eq!(warnings.len(), 1);
        assert!(
            warnings[0]
                .to_string()
                .contains("elab/function-clause-missing")
        );
    }
    let (result, warnings) = specdoc_spec_with_warnings(&paths);
    assert!(result.is_err());
    assert_eq!(warnings.len(), 1);
    assert!(
        warnings[0]
            .to_string()
            .contains("elab/function-clause-missing")
    );
}

#[test]
fn specdoc_spec_retains_source_and_prepares_prose() {
    let directory = directory::Directory::new("specdoc-spec");
    let path = directory.0.join("spec.watsup");
    fs::write(&path, "var x : nat\n").unwrap();
    let (result, warnings) = specdoc_spec_with_warnings(&[path]);
    assert!(warnings.is_empty());
    let (spec_el, spec_pl) = result.unwrap();
    assert_eq!(Print::to_string(&spec_el), "var x : nat\n");
    assert_eq!(Print::to_string(&spec_pl), "var x : nat");
}

#[test]
fn run_distinguishes_parsing_and_evaluation_failures() {
    let paths = [fixture("cli/run/types.watsup"), fixture("cli/run/relations.watsup")];
    for lang in [SpecLang::Al, SpecLang::Sl, SpecLang::Pl] {
        for (relation, path_p4, parsing) in
            [("Pass", "cli/run/invalid.p4", true), ("Reject", "cli/run/empty.p4", false)]
        {
            let spec = runner_spec_with_warnings(lang, &paths).0.unwrap();
            let error = p4spec_rust::run(
                spec,
                runner::Config::new(true, false, false),
                relation,
                &[],
                &fixture(path_p4),
            )
            .unwrap_err();
            if parsing {
                assert!(matches!(error, RunError::Parse(_)));
            } else {
                assert!(matches!(error, RunError::Eval(_)));
            }
        }
    }
}
