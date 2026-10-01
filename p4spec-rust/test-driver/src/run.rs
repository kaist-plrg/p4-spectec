use std::{collections::BTreeSet, fs, path::PathBuf, sync::mpsc, thread, time::Instant};

use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};

use p4spec_rust::runner::{self, BuiltinInterface, Config, Interpreter, Runner};

use p4spec_rust::interface::p4::{error::P4Error, parse::parse_string, preprocessor::preprocess};

use p4spec_rust::sim_plugin::dummy::Dummy;

use crate::{
    Error, Result,
    corpus::{self, Outcome, Results},
    suite::{self, Config as SuiteConfig, Language, RunSuite},
};

struct CollectedSuite {
    paths: Vec<PathBuf>,
    id_relation: String,
    results: Results,
}

/// Runs registered execution suites with the selected interpreter.
pub fn run(
    config: &SuiteConfig,
    registrations: &[RunSuite],
    language: Language,
    det: bool,
) -> Result<()> {
    // Collect registered corpus roots in suite order
    let suites = registrations
        .iter()
        .filter(|suite| suite.languages.contains(&language))
        .map(|suite| {
            let path_expected = suite::expected_path(&suite.expected);
            Ok(CollectedSuite {
                paths: corpus::collect(&suite.root, ".p4")?,
                id_relation: suite.relation.clone(),
                results: Results::new(expect_file![path_expected]),
            })
        })
        .collect::<Result<Vec<_>>>()?;
    if suites.is_empty() {
        return Err(Error::Invalid(format!("no {} execution suites registered", language.name())));
    }
    match language {
        Language::Al => run_with(config, &format!("AL cache=on det={det}"), suites, || {
            let spec_al = p4spec_rust::algo(&config.spec)
                .map_err(|error| Error::Invalid(error.to_string()))?;
            runner::build_al(spec_al, Config::new(true, det, false), Dummy)
                .map_err(|error| Error::Invalid(error.to_string()))
        }),
        Language::Sl => run_with(config, &format!("SL cache=on det={det}"), suites, || {
            let spec_sl = p4spec_rust::structure(&config.spec, true)
                .map_err(|error| Error::Invalid(error.to_string()))?;
            runner::build_sl(spec_sl, Config::new(true, det, false), Dummy)
                .map_err(|error| Error::Invalid(error.to_string()))
        }),
        Language::Pl => run_with(config, &format!("PL cache=on det={det}"), suites, || {
            let spec_pl = p4spec_rust::prosify(&config.spec)
                .map_err(|error| Error::Invalid(error.to_string()))?;
            runner::build_pl(spec_pl, Config::new(true, det, false), Dummy)
                .map_err(|error| Error::Invalid(error.to_string()))
        }),
    }
}

fn run_with<Interp, Build>(
    config: &SuiteConfig,
    text_mode: &str,
    mut suites: Vec<CollectedSuite>,
    build_runner: Build,
) -> Result<()>
where
    Interp: Interpreter<BuiltinInterface, Dummy>,
    Build: FnOnce() -> Result<Runner<Interp, BuiltinInterface, Dummy>>,
{
    let start = Instant::now();
    let mut excludes = BTreeSet::new();
    for path in &config.excludes_static {
        excludes.extend(corpus::collect_excludes(path)?);
    }
    let collected: usize = suites.iter().map(|suite| suite.paths.len()).sum();
    let excluded = suites
        .iter()
        .flat_map(|suite| &suite.paths)
        .filter(|path| path.to_str().is_some_and(|path| excludes.contains(path)))
        .count();
    eprintln!(
        "{text_mode}: collected={collected} excluded={excluded} to execute={}; preparing specification",
        collected - excluded
    );
    let mut runner = build_runner()?;
    let includes = &config.includes;
    for path in includes {
        fs::read_dir(path)?;
    }
    // Preprocess upcoming files while the runner parses and evaluates in order
    let paths_preprocess: Vec<_> = suites
        .iter()
        .flat_map(|suite| {
            suite
                .paths
                .iter()
                .filter(|path| !path.to_str().is_some_and(|path| excludes.contains(path)))
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
                let source =
                    preprocess(includes, &path).map_err(|error| error.into_report().to_string());
                if sender.send(source).is_err() {
                    break;
                }
            }
        });
        for suite in &mut suites {
            for path in &suite.paths {
                progress.set_message(path.display().to_string());
                let excluded = path.to_str().is_some_and(|path| excludes.contains(path));
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
                    let outcome = match parse_string(runner.arena_mut(), path, &source) {
                        Ok(program) => match runner.eval_program(&suite.id_relation, program) {
                            Ok(_) => Outcome::Pass,
                            Err(_) => Outcome::Fail,
                        },
                        Err(P4Error {
                            kind: p4spec_rust::interface::p4::error::P4ErrorKind::Syntax(_),
                            ..
                        }) => Outcome::Fail,
                        Err(error) => {
                            return Err(Error::Invalid(format!(
                                "{}: test execution error: {}",
                                path.display(),
                                error.into_report()
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
