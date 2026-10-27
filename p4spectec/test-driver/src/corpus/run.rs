use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
    sync::mpsc,
    thread,
    time::Instant,
};

use p4spectec::lang::data::arena::Arena;

use p4spectec::interface::p4::{error::P4Error, parse::parse_string, preprocessor::preprocess};

use p4spectec::sim_plugin::dummy::Dummy;

use serde::Deserialize;

use crate::{
    Error, Result,
    corpus::{self, Outcome, Results, log::TestLog},
    util::{self, Configuration},
};

/// Supplies the specification, includes, exclusions, configurations, and suites.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub spec: Vec<PathBuf>,
    pub includes: Vec<PathBuf>,
    pub excludes: Vec<PathBuf>,
    pub configs: Vec<Configuration>,
    pub suites: Vec<RunSuite>,
}

/// Registers a P4 execution corpus and its entry relation,
/// expected in `expected/run/<name>.expected`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RunSuite {
    pub name: String,
    pub root: PathBuf,
    pub relation: String,
}

struct CollectedSuite {
    name: String,
    root: PathBuf,
    paths: Vec<PathBuf>,
    id_relation: String,
    results: Results,
}

/// Runs registered execution suites under the named configuration.
pub fn run(name_config: &str, path_log: Option<&Path>) -> Result<()> {
    let registration: Registration = util::load("run")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let text_mode = format!("run {}", config.describe());
    // Collect registered corpus roots in suite order
    let mut suites = registration
        .suites
        .iter()
        .map(|suite| {
            Ok(CollectedSuite {
                name: suite.name.clone(),
                root: suite.root.clone(),
                paths: corpus::collect(&suite.root, ".p4")?,
                id_relation: suite.relation.clone(),
                results: Results::new(&util::expected_path("run", &suite.name)),
            })
        })
        .collect::<Result<Vec<_>>>()?;
    let len = suites.iter().map(|suite| suite.paths.len()).sum();
    let mut log = TestLog::create(path_log, &format!("run-{}", config.name), len)?;
    let start = Instant::now();
    let mut excludes = BTreeSet::new();
    for path in &registration.excludes {
        excludes.extend(corpus::collect_excludes(path)?);
    }
    let collected: usize = suites.iter().map(|suite| suite.paths.len()).sum();
    let excluded = suites
        .iter()
        .flat_map(|suite| &suite.paths)
        .filter(|path| path.to_str().is_some_and(|path| excludes.contains(path)))
        .count();
    let text_collect = format!(
        "{text_mode}: collected={collected} excluded={excluded} to execute={}; preparing specification",
        collected - excluded
    );
    eprintln!("{text_collect}");
    log.note(&text_collect)?;
    let mut arena = Arena::new();
    let spec = config.language.spec(&registration.spec)?;
    let mut runner = util::build(&mut arena, spec, config.runner(), Dummy)?;
    log.note(&format!("specification prepared in {:.1}s", start.elapsed().as_secs_f64()))?;
    let includes = &registration.includes;
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
    let progress = util::progress_bar(collected)?;
    let mut executed = 0;
    let mut passed = 0;
    let guard_stdout = super::output::SilenceStdout::new()?;
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
            log.suite(&suite.name, &suite.root)?;
            for path in &suite.paths {
                progress.set_message(path.display().to_string());
                let excluded = path.to_str().is_some_and(|path| excludes.contains(path));
                let outcome = if excluded {
                    log.skip(path, Outcome::Exclude.name())?;
                    Outcome::Exclude
                } else {
                    log.begin(path)?;
                    // Reset before parsing: no value may cross this program boundary
                    runner.reset();
                    arena.reset_values();
                    let source = receiver
                        .recv()
                        .map_err(|error| {
                            Error::from(format!("preprocessor worker stopped: {error}"))
                        })?
                        .map_err(|error| {
                            Error::from(format!(
                                "{}: test execution error: {error}",
                                path.display()
                            ))
                        })?;
                    let outcome = match parse_string(&mut arena, path, &source) {
                        Ok(program) => {
                            match runner
                                .eval_program(&mut arena, &suite.id_relation, program)
                                .result
                            {
                                Ok(_) => Outcome::Pass,
                                Err(_) => Outcome::Fail,
                            }
                        }
                        Err(P4Error {
                            kind: p4spectec::interface::p4::error::P4ErrorKind::Syntax(_),
                            ..
                        }) => Outcome::Fail,
                        Err(error) => {
                            return Err(Error::from(format!(
                                "{}: test execution error: {}",
                                path.display(),
                                error.into_report()
                            )));
                        }
                    };
                    log.end(outcome.name())?;
                    executed += 1;
                    if outcome == Outcome::Pass {
                        passed += 1;
                    }
                    outcome
                };
                suite.results.record(&[path], outcome.name(), "")?;
                progress.inc(1);
            }
        }
        Ok(())
    })?;
    guard_stdout.finish()?;
    progress.finish_with_message("complete");
    let text_summary = format!(
        "{text_mode}: collected={collected} excluded={excluded} executed={executed} pass={passed} fail={} elapsed={:.3}s",
        executed - passed,
        start.elapsed().as_secs_f64()
    );
    eprintln!("{text_summary}");
    log.finish(&text_summary)?;
    for suite in suites {
        suite.results.check();
    }
    eprintln!("{text_mode}: all {collected} file results matched expected");
    Ok(())
}
