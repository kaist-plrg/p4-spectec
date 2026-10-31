use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
    sync::mpsc,
    thread,
    time::{Duration, Instant},
};

use p4spectec::lang::data::arena::Arena;

use p4spectec::interface::p4::{
    P4Interface, error::P4Error, parse::parse_string, preprocessor::preprocess,
};

use p4spectec::runner::Runner;

use p4spectec::sim_plugin::dummy::Dummy;

use serde::Deserialize;

use crate::harness::{
    self, Error, Result,
    corpus::{self, Outcome, Results},
    interpreter::{self, Configuration},
    log::TestLog,
    worker::{self, Channel, Reply, Worker},
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

/// Registers a P4 execution corpus, its entry relation, and its expected results.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RunSuite {
    pub name: String,
    pub root: PathBuf,
    pub relation: String,
    pub expected: PathBuf,
}

struct CollectedSuite {
    name: String,
    root: PathBuf,
    paths: Vec<PathBuf>,
    id_relation: String,
    results: Results,
}

/// Runs registered execution suites under the named configuration.
///
/// With a `timeout`, a worker runs each program,
/// and a program still running after the timeout is recorded as a timeout.
pub fn run(name_config: &str, timeout: Option<Duration>, path_log: Option<&Path>) -> Result<()> {
    let registration: Registration = harness::load("run")?;
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
                results: Results::new(&std::path::absolute(&suite.expected)?),
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
    // A worker prepares its own specification; otherwise prepare it here
    let mut worker = timeout.map(|timeout| {
        Worker::new(vec!["worker".into(), "run".into(), config.name.clone()], timeout)
    });
    let mut local = None;
    if worker.is_none() {
        local = Some(prepare(&registration, config)?);
        log.note(&format!("specification prepared in {:.1}s", start.elapsed().as_secs_f64()))?;
    }
    let includes = &registration.includes;
    // Preprocess upcoming files while the runner parses and evaluates in order
    let paths_preprocess: Vec<_> = suites
        .iter()
        .filter(|_| local.is_some())
        .flat_map(|suite| {
            suite
                .paths
                .iter()
                .filter(|path| !path.to_str().is_some_and(|path| excludes.contains(path)))
        })
        .cloned()
        .collect();
    let progress = corpus::progress_bar(collected)?;
    let mut executed = 0;
    let mut passed = 0;
    let mut timed_out = 0;
    let guard_stdout = harness::output::SilenceStdout::new()?;
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
                    let outcome = match (&mut worker, &mut local) {
                        (Some(worker), _) => {
                            let request =
                                format!("{}\t{}", suite.id_relation, worker::path_text(path)?);
                            match worker.request(&request)? {
                                Reply::Done { status, .. } => Outcome::from_name(&status)?,
                                Reply::Timeout => Outcome::Timeout,
                            }
                        }
                        (None, Some((arena, runner))) => {
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
                            execute(arena, runner, &suite.id_relation, path, &source)?
                        }
                        (None, None) => unreachable!("specification prepared without a worker"),
                    };
                    log.end(outcome.name())?;
                    executed += 1;
                    match outcome {
                        Outcome::Pass => passed += 1,
                        Outcome::Timeout => timed_out += 1,
                        _ => {}
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
        "{text_mode}: collected={collected} excluded={excluded} executed={executed} pass={passed} fail={} timeout={timed_out} elapsed={:.3}s",
        executed - passed - timed_out,
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

/// Builds the runner of the configuration's specification.
fn prepare(
    registration: &Registration,
    config: &Configuration,
) -> Result<(Arena, Runner<P4Interface, Dummy>)> {
    let mut arena = Arena::new();
    let spec = config.language.spec(&registration.spec)?;
    let runner = interpreter::build(&mut arena, spec, config.runner(), Dummy)?;
    Ok((arena, runner))
}

/// Evaluates one preprocessed program through `relation`.
fn execute(
    arena: &mut Arena,
    runner: &mut Runner<P4Interface, Dummy>,
    relation: &str,
    path: &Path,
    source: &str,
) -> Result<Outcome> {
    // Reset before parsing: no value may cross this program boundary
    runner.reset();
    arena.reset_values();
    match parse_string(arena, path, source) {
        Ok(program) => match runner.eval_program(arena, relation, program).result {
            Ok(_) => Ok(Outcome::Pass),
            Err(_) => Ok(Outcome::Fail),
        },
        Err(P4Error { kind: p4spectec::interface::p4::error::P4ErrorKind::Syntax(_), .. }) => {
            Ok(Outcome::Fail)
        }
        Err(error) => Err(Error::from(format!(
            "{}: test execution error: {}",
            path.display(),
            error.into_report()
        ))),
    }
}

/// Serves `worker run <config>`: each request `<relation>\t<path>` runs one program.
pub fn serve(name_config: &str) -> Result<()> {
    let channel = Channel::open()?;
    let registration: Registration = harness::load("run")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let (mut arena, mut runner) = prepare(&registration, config)?;
    channel.serve(|request| {
        let (relation, path) = request
            .split_once('\t')
            .ok_or_else(|| Error::from(format!("invalid worker request: {request}")))?;
        let path = Path::new(path);
        let source = preprocess(&registration.includes, path).map_err(|error| {
            Error::from(format!(
                "{}: test execution error: {}",
                path.display(),
                error.into_report()
            ))
        })?;
        let outcome = execute(&mut arena, &mut runner, relation, path, &source)?;
        Ok((outcome.name().to_owned(), String::new()))
    })
}
