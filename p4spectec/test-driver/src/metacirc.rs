//! P4 programs and AL scripts through AL metacircular towers
//!
//! This driver owns corpus traversal, preprocessing, execution, and comparison.
//! `run` dispatches each suite by its tower's target language;
//! every suite enters its target through the tower's target relation.
//! `run_programs` prepares each P4 tower once and resets its state per program;
//! `run_scripts` prepares a fresh tower per AL script,
//! because an AL target supplies the builtin definitions of its own level.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
    sync::mpsc,
    thread,
    time::Instant,
};

use expect_test::expect_file;
use indicatif::{ProgressBar, ProgressStyle};

use p4spectec::{
    interface::p4::{error::P4Error, parse::parse_string, preprocessor::preprocess},
    lang::data::arena::Arena,
    metacirc::{
        self, Tower, TowerConfig,
        config::{Lang, ProgramConfig},
    },
    runner::Config,
};

use crate::{
    Error, ExecutionOptions, Result,
    corpus::{self, Outcome, Results},
    suite::{self, MetacircConfig, MetacircSuite},
};

struct CollectedSuite<'a> {
    paths: Vec<PathBuf>,
    path_tower: &'a Path,
    results: Results,
}

/// Runs the registered suites, AL scripts first, then P4 programs.
pub fn run(config: &MetacircConfig, options: ExecutionOptions) -> Result<()> {
    let mut suites_p4 = Vec::new();
    let mut suites_al = Vec::new();
    for suite in &config.suites {
        let config_tower = TowerConfig::from_file(&suite.tower)
            .map_err(|error| Error::Invalid(format!("{}: {error}", suite.name)))?;
        match config_tower.target_lang() {
            Lang::P4 => suites_p4.push(suite),
            Lang::Al => suites_al.push(suite),
        }
    }
    if !suites_al.is_empty() {
        run_scripts(&suites_al, options)?;
    }
    if !suites_p4.is_empty() {
        run_programs(config, &suites_p4, options)?;
    }
    Ok(())
}

/// Runs P4 suites, preparing each distinct tower once.
fn run_programs(
    config: &MetacircConfig,
    suites: &[&MetacircSuite],
    options: ExecutionOptions,
) -> Result<()> {
    let ExecutionOptions { cache_on, no_det } = options;
    let det = !no_det;
    let text_cache = if cache_on { "on" } else { "off" };
    let text_mode = format!("metacirc P4 cache={text_cache} det={det}");
    let start = Instant::now();
    // Collect registered corpus roots in suite order
    let mut suites = suites
        .iter()
        .map(|suite| {
            let path_expected = suite::expected_path(&suite.expected);
            Ok(CollectedSuite {
                paths: corpus::collect(&suite.root, ".p4")?,
                path_tower: &suite.tower,
                results: Results::new(expect_file![path_expected]),
            })
        })
        .collect::<Result<Vec<_>>>()?;

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
    // Suites sharing a tower file share its prepared runners
    let mut towers = BTreeMap::new();
    for suite in &suites {
        if towers.contains_key(suite.path_tower) {
            continue;
        }
        let config_tower = TowerConfig::from_file(suite.path_tower)
            .map_err(|error| Error::Invalid(error.to_string()))?;
        let options_tower = Config::new(cache_on, det, false);
        let tower = Tower::build(&mut arena, &config_tower, &ProgramConfig::P4, options_tower)
            .map_err(|error| Error::Invalid(error.to_string()))?;
        towers.insert(suite.path_tower, tower);
    }
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
            let tower = towers
                .get_mut(suite.path_tower)
                .expect("tower prepared per suite");
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
                            let value = tower.patch(&mut arena, program).map_err(|error| {
                                Error::Invalid(format!(
                                    "{}: test execution error: {error}",
                                    path.display()
                                ))
                            })?;
                            let accepted = tower.eval_program(&mut arena, value).result.is_ok();
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

/// Runs AL suites through their towers and compares pass or fail.
fn run_scripts(suites: &[&MetacircSuite], options: ExecutionOptions) -> Result<()> {
    let ExecutionOptions { cache_on, no_det } = options;
    let det = !no_det;
    let text_cache = if cache_on { "on" } else { "off" };
    let text_mode = format!("metacirc AL cache={text_cache} det={det}");
    // Specification debug premises print to stdout
    let guard_stdout = crate::output::SilenceStdout::new()?;
    let mut results_by_suite = Vec::with_capacity(suites.len());
    for suite in suites {
        let path_expected = suite::expected_path(&suite.expected);
        let mut results = Results::new(expect_file![path_expected]);
        let config_tower = TowerConfig::from_file(&suite.tower)
            .map_err(|error| Error::Invalid(format!("{}: {error}", suite.name)))?;
        for path in corpus::collect(&suite.root, ".watsup")? {
            let paths_program = std::slice::from_ref(&path);
            // Invalid scripts are test errors, not rejected executions
            p4spectec::algo(paths_program)
                .map_err(|error| Error::Invalid(format!("{}: {error}", path.display())))?;
            let accepted =
                metacirc::run(&config_tower, &[], paths_program, Config::new(cache_on, det, false))
                    .is_ok();
            results.record(&path, if accepted { Outcome::Pass } else { Outcome::Fail })?;
        }
        results_by_suite.push(results);
    }
    guard_stdout.finish()?;
    for results in results_by_suite {
        results.check();
    }
    eprintln!("{text_mode}: all script results matched expected");
    Ok(())
}
