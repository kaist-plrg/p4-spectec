use crate::{
    Error, Result,
    corpus::{self, Outcome, Results},
};
use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};
use p4spec_rust::{
    interface::p4::{error::P4Error, parse::parse_string, preprocessor::preprocess},
    runner::{self, BuiltinInterface, Config, Interpreter, ProgramError, Runner},
    sim_plugin::dummy::Dummy,
};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::mpsc,
    thread,
    time::Instant,
};

struct RunSuite {
    paths: Vec<PathBuf>,
    id_relation: &'static str,
    use_excludes: bool,
    results: Results,
}

fn collect_suite(
    path_dir: &str,
    id_relation: &'static str,
    name_expected: &str,
    use_excludes: bool,
) -> Result<RunSuite> {
    let path_expected = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("expected/run")
        .join(name_expected);
    Ok(RunSuite {
        paths: corpus::collect(Path::new(path_dir), ".p4")?,
        id_relation,
        use_excludes,
        results: Results::new(expect_file![path_expected]),
    })
}

pub fn run() -> Result<()> {
    let suites = [
        collect_suite("p4c/testdata/p4_16_samples", "Program_inst", "pos-al.expected", true),
        collect_suite("p4c/testdata/p4_16_errors", "Program_ok", "neg-al.expected", true),
    ]
    .into_iter()
    .collect::<Result<Vec<_>>>()?;
    run_with("AL cache=on det=false", suites, || {
        let spec_al =
            p4spec_rust::algo(["spec"]).map_err(|error| Error::Invalid(error.to_string()))?;
        runner::build_al(spec_al, Config::new(true, false, false), Dummy)
            .map_err(|error| Error::Invalid(error.to_string()))
    })
}

/// Runs the SL execution suites against source-derived expected results
pub fn run_sl(det: bool) -> Result<()> {
    // The OCaml SL outcomes are byte-identical to these AL expectation files
    let suites = [
        collect_suite("p4c/testdata/p4_16_samples", "Program_inst", "pos-al.expected", true),
        collect_suite("p4c/testdata/p4_16_errors", "Program_ok", "neg-al.expected", true),
    ]
    .into_iter()
    .collect::<Result<Vec<_>>>()?;
    run_with(&format!("SL cache=on det={det}"), suites, || {
        let spec_sl = p4spec_rust::structure(["spec"], true)
            .map_err(|error| Error::Invalid(error.to_string()))?;
        runner::build_sl(spec_sl, Config::new(true, det, false), Dummy)
            .map_err(|error| Error::Invalid(error.to_string()))
    })
}

/// Runs the PL execution suites against source-derived expected results.
pub fn run_pl(det: bool) -> Result<()> {
    let suites = [
        collect_suite("p4c/testdata/p4_16_samples", "Program_inst", "pos-al.expected", true),
        collect_suite("p4c/testdata/p4_16_errors", "Program_ok", "neg-al.expected", true),
    ]
    .into_iter()
    .collect::<Result<Vec<_>>>()?;
    run_with(&format!("PL cache=on det={det}"), suites, || {
        let spec_pl =
            p4spec_rust::prosify(["spec"]).map_err(|error| Error::Invalid(error.to_string()))?;
        runner::build_pl(spec_pl, Config::new(true, det, false), Dummy)
            .map_err(|error| Error::Invalid(error.to_string()))
    })
}

fn run_with<Interp, Build>(
    text_mode: &str,
    mut suites: Vec<RunSuite>,
    build_runner: Build,
) -> Result<()>
where
    Interp: Interpreter<BuiltinInterface, Dummy>,
    Build: FnOnce() -> Result<Runner<Interp, BuiltinInterface, Dummy>>,
{
    let start = Instant::now();
    let excludes = corpus::collect_excludes(Path::new("excludes/static"))?;
    let collected: usize = suites.iter().map(|suite| suite.paths.len()).sum();
    let excluded = suites
        .iter()
        .filter(|suite| suite.use_excludes)
        .flat_map(|suite| &suite.paths)
        .filter(|path| path.to_str().is_some_and(|path| excludes.contains(path)))
        .count();
    eprintln!(
        "{text_mode}: collected={collected} excluded={excluded} to execute={}; preparing specification",
        collected - excluded
    );
    let mut runner = build_runner()?;
    let includes = vec![PathBuf::from("p4c/p4include")];
    fs::read_dir(&includes[0])?;
    // Preprocess upcoming files while the runner parses and evaluates in order
    let paths_preprocess: Vec<_> = suites
        .iter()
        .flat_map(|suite| {
            suite.paths.iter().filter(|path| {
                !suite.use_excludes || !path.to_str().is_some_and(|path| excludes.contains(path))
            })
        })
        .cloned()
        .collect();
    let progress = ProgressBar::new(collected as u64).with_style(
        ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")
            .map_err(|error| Error::Invalid(error.to_string()))?,
    );
    let mut executed = 0;
    let mut passed = 0;
    thread::scope(|scope| -> Result<()> {
        let (sender, receiver) = mpsc::sync_channel(2);
        scope.spawn(move || {
            for path in paths_preprocess {
                let source = preprocess(&includes, &path).map_err(|error| error.to_string());
                if sender.send(source).is_err() {
                    break;
                }
            }
        });
        for suite in &mut suites {
            for path in &suite.paths {
                progress.set_message(path.display().to_string());
                let excluded =
                    suite.use_excludes && path.to_str().is_some_and(|path| excludes.contains(path));
                let outcome = if excluded {
                    Outcome::Exclude
                } else {
                    // Reset before parsing: no value may cross this program boundary
                    runner.reset();
                    fs::File::open(path)?;
                    let source = receiver
                        .recv()
                        .map_err(|error| {
                            Error::Invalid(format!("preprocessor worker stopped: {error}"))
                        })?
                        .map_err(|error| {
                            Error::Invalid(format!(
                                "{}: test execution error: {error}",
                                path.display()
                            ))
                        })?;
                    let result = runner.parse_and_eval_program(suite.id_relation, |arena| {
                        parse_string(arena, path, &source)
                    });
                    let outcome = match result {
                        Ok(_) => Outcome::Pass,
                        Err(ProgramError::Parse(P4Error::Syntax(_)))
                        | Err(ProgramError::Runtime(_)) => Outcome::Fail,
                        Err(ProgramError::Parse(error)) => {
                            return Err(Error::Invalid(format!(
                                "{}: test execution error: {error}",
                                path.display()
                            )));
                        }
                    };
                    executed += 1;
                    if outcome == Outcome::Pass {
                        passed += 1;
                    }
                    outcome
                };
                suite.results.record(path, outcome)?;
                progress.inc(1);
            }
        }
        Ok(())
    })?;
    progress.finish_with_message("complete");
    eprintln!(
        "{text_mode}: collected={collected} excluded={excluded} executed={executed} pass={passed} fail={} elapsed={:.3}s",
        executed - passed,
        start.elapsed().as_secs_f64()
    );
    for suite in suites {
        suite.results.check();
    }
    eprintln!("{text_mode}: all {collected} file results matched expected");
    Ok(())
}
