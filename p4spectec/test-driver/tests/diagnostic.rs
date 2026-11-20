//! Diagnostics of each pipeline stage, one test per stage
//!
//! A test runs every fixture of its directories under `testdata/spec`:
//! the stages before its own must pass, and its own must fail
//! (or, for a fixture under `pos/`, pass).
//! The reports are rendered and compared with `<stem>.expect` beside the fixture;
//! `UPDATE_EXPECT=1` rewrites the expectation instead.
//! Every test runs from `testdata/spec`, so that reports show stable relative paths.

use std::{
    fmt::Display,
    path::{Path, PathBuf},
};

use expect_test::expect_file;

use p4spectec::{
    diagnostic::{DisplayStyle, RenderConfig, Renderer, Report},
    frontend::parse::{parse_files, parse_typ},
    interface::p4::{error::P4ErrorKind, parse::parse_string},
    lang::{
        common::source::Span,
        data::{arena::Arena, value::flat as value},
    },
    pass,
    runner::{self, Config, NullExtern, RunError, Spec},
    specdoc::{adoc, splicer},
};

use p4spectec_sim::Dummy;

use p4spectec_test::harness::{
    corpus,
    interpreter::{self, Language},
};

/// Interpreter languages every execution fixture runs in.
const LANGUAGES: [Language; 3] = [Language::Al, Language::Sl, Language::Pl];

/// Stems of the `neg/run` fixtures that run with determinism checks.
const STEMS_DET: [&str; 3] =
    ["relation-nondeterministic", "function-nondeterministic", "list-overlap-det"];

// = Helpers

/// Enters `testdata/spec` and lists the files of `dir` ending in one of `suffixes`.
///
/// Paths are relative to `testdata/spec`.
/// A file named by an `.args` file of `dir` is an input of that CLI case
/// (see `cli/tests/diagnostic.rs`), not a fixture of its own, and is left out.
fn fixtures(dir: &str, suffixes: &[&str]) -> Vec<PathBuf> {
    let root = corpus::root().join("testdata/spec");
    std::env::set_current_dir(&root).expect("enter testdata/spec");
    let list = |suffix| corpus::collect(Path::new(dir), suffix).expect("fixture directory");
    let paths_cli: Vec<PathBuf> = list(".args")
        .into_iter()
        .flat_map(|path| {
            let text = std::fs::read_to_string(path).expect("argument file");
            text.lines().map(PathBuf::from).collect::<Vec<_>>()
        })
        .collect();
    let mut paths: Vec<PathBuf> = suffixes
        .iter()
        .flat_map(|suffix| list(suffix))
        .filter(|path| !paths_cli.contains(path))
        .collect();
    assert!(!paths.is_empty(), "no fixtures in {dir}");
    paths.sort();
    paths
}

/// Returns the result of a stage that must pass for `path`.
fn must_pass<T>(path: &Path, stage: &str, result: Result<T, impl Display>) -> T {
    result.unwrap_or_else(|error| panic!("{}: {stage} failed: {error}", path.display()))
}

/// Returns the error of a stage that must fail for `path`.
fn must_fail<T, E>(path: &Path, stage: &str, result: Result<T, E>) -> E {
    match result {
        Ok(_) => panic!("{}: {stage} unexpectedly passed", path.display()),
        Err(error) => error,
    }
}

/// Renders `reports` under `config` and compares the text with `path_expected`.
fn check(path_expected: &Path, reports: &[Report], config: &RenderConfig) {
    let mut text = String::new();
    for report in reports {
        let rendered = Renderer::new(config.clone())
            .render_to_string(report)
            .expect("render report");
        text.push_str(&rendered);
    }
    let path_expected = std::env::current_dir().unwrap().join(path_expected);
    expect_file![path_expected].assert_eq(&text);
}

/// Returns the expectation beside `path`.
fn expected(path: &Path) -> PathBuf {
    path.with_extension("expect")
}

/// Renders runtime frames in the short style.
fn frames_short() -> RenderConfig {
    RenderConfig { frame_style: Some(DisplayStyle::Short), ..Default::default() }
}

/// Builds a runner of the specification at `path` in `language`.
fn runner(
    arena: &mut Arena,
    path: &Path,
    language: Language,
    det: bool,
) -> runner::Runner<p4spectec::interface::p4::P4Interface, NullExtern> {
    let spec = must_pass(path, "conversion", language.spec(&[path.to_path_buf()]));
    let config = Config::new(true, det, false, true);
    must_pass(path, "runner build", interpreter::build(arena, spec, config, NullExtern))
}

// = Specification stages

#[test]
fn parse() {
    for path in fixtures("neg/parse", &[".watsup", ".mixop"]) {
        // A .mixop file holds a notation type for its own parser entry point
        let report = if path.extension().is_some_and(|ext| ext == "mixop") {
            let text = std::fs::read_to_string(&path).unwrap();
            must_fail(&path, "parsing", parse_typ(&text))
        } else {
            must_fail(&path, "parsing", parse_files([&path]))
        };
        check(&expected(&path), &[*report], &RenderConfig::default());
    }
}

#[test]
fn elab() {
    for path in fixtures("neg/elab", &[".watsup"])
        .into_iter()
        .chain(fixtures("pos/elab", &[".watsup"]))
    {
        let spec_el = must_pass(&path, "parsing", parse_files([&path]));
        // Keep the warnings before the result, then the error if any
        let (result, mut reports) = pass::elaborate::convert_with_warnings(spec_el);
        if path.starts_with("pos") {
            must_pass(&path, "elaboration", result);
        } else {
            reports.push(*must_fail(&path, "elaboration", result));
        }
        check(&expected(&path), &reports, &RenderConfig::default());
    }
}

#[test]
fn algo() {
    for path in fixtures("neg/algo", &[".watsup"]) {
        let spec_il = must_pass(&path, "elaboration", p4spectec::elab(std::slice::from_ref(&path)));
        let report = must_fail(&path, "algorithmic conversion", pass::algo::convert(spec_il));
        check(&expected(&path), &[*report], &RenderConfig::default());
    }
}

#[test]
fn structure() {
    for path in fixtures("neg/structure", &[".watsup"]) {
        // Both rule-group modes fail with the same report
        for without_rule_groups in [false, true] {
            let spec_al = must_pass(
                &path,
                "algorithmic conversion",
                p4spectec::algo(std::slice::from_ref(&path)),
            );
            let result = pass::structure::convert(spec_al, without_rule_groups);
            let report = must_fail(&path, "structuring", result);
            check(&expected(&path), &[*report], &RenderConfig::default());
        }
    }
}

#[test]
fn prose() {
    for path in fixtures("neg/prose", &[".watsup"]) {
        let spec_sl = must_pass(
            &path,
            "structuring",
            p4spectec::structure(std::slice::from_ref(&path), false),
        );
        let report = must_fail(&path, "prose conversion", pass::prosify::convert(spec_sl));
        check(&expected(&path), &[*report], &RenderConfig::default());
    }
}

#[test]
fn specdoc() {
    for path in fixtures("neg/specdoc", &[".watsup"]) {
        // Conversion must pass without warnings before rendering warns
        let (result, warnings) = p4spectec::prosify_with_warnings(std::slice::from_ref(&path));
        let spec_pl = must_pass(&path, "prose conversion", result);
        assert!(warnings.is_empty(), "{}: conversion warned", path.display());
        let mut warnings = Vec::new();
        adoc::pl::render_spec(&mut warnings, &spec_pl);
        assert!(!warnings.is_empty(), "{}: AsciiDoc rendered without warnings", path.display());
        check(&expected(&path), &warnings, &RenderConfig::default());
    }
}

#[test]
fn splice() {
    // Splice each skeleton against the one specification of its directory
    let paths_spec = [PathBuf::from("cli/splice/minimal.watsup")];
    for path in fixtures("cli/splice", &[".adoc"]) {
        let spec_el = must_pass(&path, "parsing", parse_files(&paths_spec));
        let spec_pl = must_pass(&path, "prose conversion", p4spectec::prosify(&paths_spec));
        let text = std::fs::read_to_string(&path).unwrap();
        let text_path = path.to_string_lossy();
        let result = splicer::splice_strings(&spec_el, &spec_pl, &[(&text_path, &text)]);
        let report = must_fail(&path, "splicing", result);
        check(&expected(&path), &[*report], &RenderConfig::default());
    }
}

// = Execution

#[test]
fn p4_syntax() {
    for path in fixtures("p4", &[".p4"]) {
        let text = std::fs::read_to_string(&path).unwrap();
        let error = must_fail(&path, "P4 parsing", parse_string(&mut Arena::new(), &path, &text));
        assert!(
            matches!(error.kind, P4ErrorKind::Syntax(_)),
            "{}: failed outside syntax admission",
            path.display()
        );
        check(&expected(&path), &[*error.into_report()], &frames_short());
    }
}

#[test]
fn run() {
    for path in fixtures("neg/run", &[".watsup"]) {
        let stem = path.file_stem().unwrap().to_string_lossy();
        let det = STEMS_DET.contains(&&*stem);
        for language in LANGUAGES {
            // Apply `R` to the natural 1
            let mut arena = Arena::new();
            let mut runner = runner(&mut arena, &path, language, det);
            let nat = value::make::nat(&mut arena, 1u64.into(), Span::default()).unwrap();
            let result = runner.eval_rel(&mut arena, "R", &[nat]).result;
            let error = must_fail(&path, "relation R", result);
            // A language's own expectation overrides the shared one
            let path_language = path.with_extension(format!("{}.expect", language.name()));
            let path_expected =
                if path_language.exists() { path_language } else { expected(&path) };
            check(&path_expected, &[*error.into_report()], &frames_short());
        }
    }
}

#[test]
fn run_main() {
    // Each source's `Main`, whose notation is `OK`, must hold in every language
    for path in fixtures("pos/run", &[".watsup"]) {
        for language in LANGUAGES {
            let mut arena = Arena::new();
            let mut runner = runner(&mut arena, &path, language, true);
            let result = runner.eval_rel(&mut arena, "Main", &[]).result;
            must_pass(&path, &format!("Main in {}", language.name()), result.map_err(|_| "error"));
        }
    }
}

#[test]
fn sim() {
    let config = Config::new(true, false, false, true);
    // Admission must reject an architecture the simulator lacks
    for path in fixtures("cli/sim", &[".watsup"]) {
        let spec_al = must_pass(
            &path,
            "algorithmic conversion",
            p4spectec::algo(std::slice::from_ref(&path)),
        );
        let result = p4spectec_sim::build(Spec::Al(spec_al), "unsupported", config);
        let report = must_fail(&path, "simulator build", result);
        check(&expected(&path), &[*report], &frames_short());
    }
    // P4 programs run under the full specification and fail a static assertion
    let paths_spec = [PathBuf::from("../../spec")];
    let paths_include = [PathBuf::from("../../p4c/p4include")];
    for path in fixtures("cli/sim", &[".p4"]) {
        let spec_al = must_pass(&path, "algorithmic conversion", p4spectec::algo(&paths_spec));
        let result = runner::run(
            Spec::Al(spec_al),
            config,
            Dummy::default(),
            "Program_ok",
            &paths_include,
            &path,
        );
        let report = match must_fail(&path, "simulation", result) {
            RunError::Eval(error) => error.into_report(),
            error => panic!("{}: setup failed before simulation: {error}", path.display()),
        };
        check(&expected(&path), &[*report], &frames_short());
    }
}
