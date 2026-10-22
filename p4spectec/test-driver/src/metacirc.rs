//! P4 execution corpus through a prepared AL metacircular tower
//!
//! This driver owns corpus traversal, preprocessing, execution, and comparison.
//! The tower retains its prepared runners while each program's state is reset.

use std::{collections::BTreeSet, fs, path::PathBuf, sync::mpsc, thread, time::Instant};

use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};

use p4spectec::{
    interface::p4::{error::P4Error, parse::parse_string, preprocessor::preprocess},
    lang::data::arena::Arena,
    metacirc::{Tower, TowerConfig},
    runner::Config,
};

use crate::{
    Error, ExecutionOptions, Result,
    corpus::{self, Outcome, Results},
    suite::{self, CorpusConfig, Language},
};

struct CollectedSuite {
    paths: Vec<PathBuf>,
    id_relation: String,
    results: Results,
}

/// Runs the registered P4 corpus through one prepared AL/P4 tower.
pub fn run(
    config: &CorpusConfig,
    config_tower: &TowerConfig,
    options: ExecutionOptions,
) -> Result<()> {
    let ExecutionOptions { cache_on, no_det } = options;
    let det = !no_det;
    let language = Language::Sl;
    let text_cache = if cache_on { "on" } else { "off" };
    let text_mode = format!("metacirc SL cache={text_cache} det={det}");
    let start = Instant::now();
    // Collect registered corpus roots in suite order
    let mut suites = config
        .suites
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

    let mut excludes = BTreeSet::new();
    for path in &config.excludes {
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
    let mut arena = Arena::new();
    let mut tower = Tower::build(&mut arena, config_tower, Config::new(cache_on, det, false))
        .map_err(|error| Error::Invalid(error.to_string()))?;
    let includes = &config.includes;
    for path in includes {
        fs::read_dir(path)?;
    }
    // Preprocess upcoming files while the tower parses and evaluates in order
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
    let guard_stdout = crate::output::SilenceStdout::new()?;
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
                    tower.reset();
                    arena.reset_values();
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
                    let outcome = match parse_string(&mut arena, path, &source) {
                        Ok(program) => {
                            // Patch failures abort; evaluation rejection is an outcome
                            let value = tower
                                .patch(&mut arena, &suite.id_relation, program)
                                .map_err(|error| {
                                    Error::Invalid(format!(
                                        "{}: test execution error: {error}",
                                        path.display()
                                    ))
                                })?;
                            let accepted = tower
                                .eval_program(&mut arena, &suite.id_relation, value)
                                .result
                                .is_ok();
                            if accepted { Outcome::Pass } else { Outcome::Fail }
                        }
                        Err(P4Error {
                            kind: p4spectec::interface::p4::error::P4ErrorKind::Syntax(_),
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
    guard_stdout.finish()?;
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
