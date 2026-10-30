//! P4 programs and spec-language scripts through metacircular towers,
//! registered in `suites/metacirc/run.toml`
//!
//! This driver owns corpus traversal, preprocessing, execution, and comparison.
//! `run` takes the suites with a tower for the configuration's base
//! and dispatches each by the language of its tower's top level;
//! every suite enters its top level through that level's relation.
//! `run_programs` prepares each P4 tower once and resets its state per program;
//! `run_scripts` prepares a fresh tower per script,
//! because each script supplies the builtin definitions for the top level.
//! Both skip the sources listed by the configuration's exclusions.
//! With a timeout, a worker per tower runs each source instead,
//! and a source still running after the timeout is recorded as a timeout.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
    sync::mpsc,
    thread,
    time::{Duration, Instant},
};

use clap::ValueEnum;
use serde::Deserialize;

use p4spectec::{
    interface::p4::{error::P4Error, parse::parse_string, preprocessor::preprocess},
    lang::data::arena::Arena,
    metacirc::{self, Tower, TowerConfig, config::Lang},
    sim_plugin::dummy::Dummy,
};

use super::config::{Base, ByBase, Configuration};
use crate::{
    harness::{
        self, Error, Result,
        corpus::{self, Outcome, Results},
        log::TestLog,
        worker::{self, Channel, Reply, Worker},
    },
    suite::diagnostic,
};

/// Supplies execution includes, exclusions per base, configurations, and suites.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub includes: Vec<PathBuf>,
    pub excludes: ByBase<Vec<PathBuf>>,
    pub configs: Vec<Configuration>,
    pub suites: Vec<RunSuite>,
}

/// Registers a corpus run through one tower per base.
///
/// The tower's top level decides the entry relation
/// and whether inputs are P4 programs or AL or IL scripts.
/// P4 suites compare against an `expected` file of per-program results;
/// script suites give every script under `root` one `outcome`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RunSuite {
    pub name: String,
    pub towers: ByBase<PathBuf>,
    pub root: PathBuf,
    pub expected: Option<PathBuf>,
    pub outcome: Option<diagnostic::Outcome>,
}

/// Selects suites by the input language of their tower's top level.
#[derive(Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum Input {
    /// AL scripts
    Al,
    /// P4 programs
    P4,
}

/// A registered suite with the tower of the configuration's base.
struct SelectedSuite<'a> {
    suite: &'a RunSuite,
    path_tower: &'a Path,
}

struct CollectedSuite<'a> {
    name: &'a str,
    root: &'a Path,
    paths: Vec<PathBuf>,
    path_tower: &'a Path,
    results: Results,
}

/// Runs the suites under the named configuration, AL scripts first, then P4 programs.
///
/// `name_suite` restricts the run to the registered suite of that name,
/// and `input` to the suites whose tower takes that input.
/// With a `timeout`, workers run the sources.
/// `path_log` names the directory of the timing logs,
/// one for the scripts and one for the programs.
pub fn run(
    name_config: &str,
    name_suite: Option<&str>,
    input: Option<Input>,
    timeout: Option<Duration>,
    path_log: Option<&Path>,
) -> Result<()> {
    let registration: Registration = harness::load("metacirc/run")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    if let Some(name) = name_suite
        && !registration.suites.iter().any(|suite| suite.name == name)
    {
        return Err(Error::from(format!("unknown metacirc suite {name}")));
    }
    let mut suites_p4 = Vec::new();
    let mut suites_al = Vec::new();
    for suite in &registration.suites {
        // Skip suites of other names and suites without a tower for the base
        if name_suite.is_some_and(|name| suite.name != name) {
            continue;
        }
        let Some(path_tower) = suite.towers.get(config.base) else {
            continue;
        };
        // The tower's top level decides the input
        let config_tower = TowerConfig::from_file(path_tower)
            .map_err(|error| Error::from(format!("{}: {error}", suite.name)))?;
        let (input_suite, suites_input) = match config_tower.top_lang() {
            Lang::P4 => (Input::P4, &mut suites_p4),
            Lang::Al | Lang::Il => (Input::Al, &mut suites_al),
        };
        if input.is_none_or(|input| input == input_suite) {
            suites_input.push(SelectedSuite { suite, path_tower });
        }
    }
    if suites_p4.is_empty() && suites_al.is_empty() {
        return Err(Error::from(format!("no metacirc suites selected under {name_config}")));
    }
    let excludes = collect_excludes(&registration, config.base)?;
    if !suites_al.is_empty() {
        run_scripts(config, &excludes, &suites_al, timeout, path_log)?;
    }
    if !suites_p4.is_empty() {
        run_programs(&registration, config, &excludes, &suites_p4, timeout, path_log)?;
    }
    Ok(())
}

/// Runs P4 suites, preparing each distinct tower once.
fn run_programs(
    registration: &Registration,
    config: &Configuration,
    excludes: &BTreeSet<String>,
    suites: &[SelectedSuite<'_>],
    timeout: Option<Duration>,
    path_log: Option<&Path>,
) -> Result<()> {
    let text_mode = format!("metacirc P4 {}", config.describe());
    let start = Instant::now();
    // Collect registered corpus roots in suite order
    let mut suites = suites
        .iter()
        .map(|SelectedSuite { suite, path_tower }| {
            let path_expected = suite.expected.as_deref().ok_or_else(|| {
                Error::from(format!("{}: a P4 suite needs an expected file", suite.name))
            })?;
            Ok(CollectedSuite {
                name: &suite.name,
                root: &suite.root,
                paths: corpus::collect(&suite.root, ".p4")?,
                path_tower,
                results: Results::new(&std::path::absolute(path_expected)?),
            })
        })
        .collect::<Result<Vec<_>>>()?;

    let collected: usize = suites.iter().map(|suite| suite.paths.len()).sum();
    let excluded = suites
        .iter()
        .flat_map(|suite| &suite.paths)
        .filter(|path| path.to_str().is_some_and(|path| excludes.contains(path)))
        .count();
    let mut log = TestLog::create(path_log, &format!("metacirc-p4-{}", config.name), collected)?;
    let text_collect = format!(
        "{text_mode}: collected={collected} excluded={excluded} to execute={}; preparing specification",
        collected - excluded
    );
    eprintln!("{text_collect}");
    log.note(&text_collect)?;
    let mut arena = Arena::new();
    // Suites sharing a tower file share its prepared runners or its worker
    let mut towers = BTreeMap::new();
    let mut workers = BTreeMap::new();
    for suite in &suites {
        if let Some(timeout) = timeout {
            let args = worker_args("program", config, suite.path_tower)?;
            workers
                .entry(suite.path_tower)
                .or_insert_with(|| Worker::new(args, timeout));
            continue;
        }
        if towers.contains_key(suite.path_tower) {
            continue;
        }
        let config_tower = TowerConfig::from_file(suite.path_tower)?;
        let tower = Tower::build(&mut arena, &config_tower, config.runner(), Dummy)?;
        towers.insert(suite.path_tower, tower);
    }
    if timeout.is_none() {
        log.note(&format!("towers prepared in {:.1}s", start.elapsed().as_secs_f64()))?;
    }
    let includes = &registration.includes;
    for path in includes {
        fs::read_dir(path)?;
    }
    // Preprocess upcoming files while the tower parses and evaluates in order
    let paths_preprocess: Vec<_> = suites
        .iter()
        .filter(|_| timeout.is_none())
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
            let mut tower = towers.get_mut(suite.path_tower);
            let mut worker = workers.get_mut(suite.path_tower);
            log.suite(suite.name, suite.root)?;
            for path in &suite.paths {
                progress.set_message(path.display().to_string());
                let excluded = path.to_str().is_some_and(|path| excludes.contains(path));
                let outcome = if excluded {
                    log.skip(path, Outcome::Exclude.name())?;
                    Outcome::Exclude
                } else {
                    log.begin(path)?;
                    let outcome = match (&mut worker, &mut tower) {
                        (Some(worker), _) => match worker.request(worker::path_text(path)?)? {
                            Reply::Done { status, .. } => Outcome::from_name(&status)?,
                            Reply::Timeout => Outcome::Timeout,
                        },
                        (None, Some(tower)) => {
                            fs::File::open(path)?;
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
                            execute_program(&mut arena, tower, path, &source)?
                        }
                        (None, None) => unreachable!("tower prepared per suite"),
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

/// Evaluates one preprocessed program through the tower.
fn execute_program(
    arena: &mut Arena,
    tower: &mut Tower,
    path: &Path,
    source: &str,
) -> Result<Outcome> {
    // Reset before parsing: no value may cross this program boundary
    tower.reset();
    arena.reset_values();
    match parse_string(arena, path, source) {
        Ok(program) => {
            // Encoding failures abort; evaluation rejection is an outcome
            let values = tower.encode_input(arena, program).map_err(|error| {
                Error::from(format!("{}: test execution error: {error}", path.display()))
            })?;
            let accepted = tower.eval_input(arena, &values).is_ok();
            Ok(if accepted { Outcome::Pass } else { Outcome::Fail })
        }
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

/// Serves `worker program <config> <tower>`: each request names one P4 program.
pub fn serve_programs(name_config: &str, path_tower: &Path) -> Result<()> {
    let channel = Channel::open()?;
    let registration: Registration = harness::load("metacirc/run")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let mut arena = Arena::new();
    let config_tower = TowerConfig::from_file(path_tower)?;
    let mut tower = Tower::build(&mut arena, &config_tower, config.runner(), Dummy)?;
    channel.serve(|request| {
        let path = Path::new(request);
        let source = preprocess(&registration.includes, path).map_err(|error| {
            Error::from(format!(
                "{}: test execution error: {}",
                path.display(),
                error.into_report()
            ))
        })?;
        let outcome = execute_program(&mut arena, &mut tower, path, &source)?;
        Ok((outcome.name().to_owned(), String::new()))
    })
}

/// Returns the worker command `worker <kind> <config> <tower>`.
fn worker_args(kind: &str, config: &Configuration, path_tower: &Path) -> Result<Vec<String>> {
    Ok(vec![
        "worker".to_owned(),
        kind.to_owned(),
        config.name.clone(),
        worker::path_text(path_tower)?.to_owned(),
    ])
}

/// Gathers the excluded corpus paths of `base`.
fn collect_excludes(registration: &Registration, base: Base) -> Result<BTreeSet<String>> {
    let mut excludes = BTreeSet::new();
    for path in registration.excludes.get(base).into_iter().flatten() {
        excludes.extend(corpus::collect_excludes(path)?);
    }
    Ok(excludes)
}

/// Runs script suites through their towers and checks each script's outcome.
fn run_scripts(
    config: &Configuration,
    excludes: &BTreeSet<String>,
    suites: &[SelectedSuite<'_>],
    timeout: Option<Duration>,
    path_log: Option<&Path>,
) -> Result<()> {
    let text_mode = format!("metacirc script {}", config.describe());
    let start = Instant::now();
    // Collect every suite's scripts first so the log knows the total
    let mut suites = suites
        .iter()
        .map(|SelectedSuite { suite, path_tower }| {
            let outcome_expect = suite.outcome.ok_or_else(|| {
                Error::from(format!("{}: a script suite needs an outcome", suite.name))
            })?;
            let config_tower = TowerConfig::from_file(path_tower)
                .map_err(|error| Error::from(format!("{}: {error}", suite.name)))?;
            let paths = corpus::collect(&suite.root, ".watsup")?;
            let worker = match timeout {
                Some(timeout) => {
                    Some(Worker::new(worker_args("script", config, path_tower)?, timeout))
                }
                None => None,
            };
            Ok((*suite, config_tower, outcome_expect, paths, worker))
        })
        .collect::<Result<Vec<_>>>()?;
    let collected = suites.iter().map(|(_, _, _, paths, _)| paths.len()).sum();
    let mut log =
        TestLog::create(path_log, &format!("metacirc-script-{}", config.name), collected)?;
    // Specification debug premises print to stdout
    let guard_stdout = harness::output::SilenceStdout::new()?;
    let mut executed = 0;
    let mut excluded = 0;
    let mut mismatches = Vec::new();
    for (suite, config_tower, outcome_expect, paths, worker) in &mut suites {
        log.suite(&suite.name, &suite.root)?;
        for path in paths {
            if path.to_str().is_some_and(|path| excludes.contains(path)) {
                log.skip(path, "exclude")?;
                excluded += 1;
                continue;
            }
            log.begin(path)?;
            executed += 1;
            let outcome = match worker {
                Some(worker) => match worker.request(worker::path_text(path)?)? {
                    Reply::Done { status, .. } => script_outcome_from_name(&status)?,
                    Reply::Timeout => {
                        // A timed-out script matches neither outcome
                        log.end(&format!("timeout, expected {outcome_expect:?}"))?;
                        mismatches.push(format!(
                            "{} ({}): expected {outcome_expect:?}, timed out",
                            path.display(),
                            suite.name
                        ));
                        continue;
                    }
                },
                None => execute_script(config_tower, config, path)?,
            };
            if outcome == *outcome_expect {
                log.end(&format!("{outcome:?}"))?;
            } else {
                log.end(&format!("{outcome:?}, expected {outcome_expect:?}"))?;
                mismatches.push(format!(
                    "{} ({}): expected {outcome_expect:?}, got {outcome:?}",
                    path.display(),
                    suite.name
                ));
            }
        }
    }
    guard_stdout.finish()?;
    let text_summary = format!(
        "{text_mode}: collected={collected} excluded={excluded} executed={executed} mismatched={} elapsed={:.3}s",
        mismatches.len(),
        start.elapsed().as_secs_f64()
    );
    eprintln!("{text_summary}");
    log.finish(&text_summary)?;
    // Report every mismatched script before failing
    if !mismatches.is_empty() {
        return Err(Error::from(format!(
            "{text_mode}: outcomes differ from their suite:\n{}",
            mismatches.join("\n")
        )));
    }
    eprintln!("{text_mode}: all {executed} script outcomes matched their suite");
    Ok(())
}

/// Runs one script through a fresh tower.
fn execute_script(
    config_tower: &TowerConfig,
    config: &Configuration,
    path: &PathBuf,
) -> Result<diagnostic::Outcome> {
    let paths_program = std::slice::from_ref(path);
    // Invalid scripts are test errors, not rejected executions;
    // an IL top level converts the script itself, so it need only elaborate
    let valid = match config_tower.top_lang() {
        Lang::Il => p4spectec::elab(paths_program).map(|_| ()),
        _ => p4spectec::algo(paths_program).map(|_| ()),
    };
    valid.map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    let accepted = metacirc::run(config_tower, &[], paths_program, config.runner()).is_ok();
    Ok(if accepted { diagnostic::Outcome::Success } else { diagnostic::Outcome::Failure })
}

/// Serves `worker script <config> <tower>`: each request names one script.
pub fn serve_scripts(name_config: &str, path_tower: &Path) -> Result<()> {
    let channel = Channel::open()?;
    let registration: Registration = harness::load("metacirc/run")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let config_tower = TowerConfig::from_file(path_tower)?;
    channel.serve(|request| {
        let outcome = execute_script(&config_tower, config, &PathBuf::from(request))?;
        Ok((script_outcome_name(outcome).to_owned(), String::new()))
    })
}

/// Spells a script outcome in a worker answer.
fn script_outcome_name(outcome: diagnostic::Outcome) -> &'static str {
    match outcome {
        diagnostic::Outcome::Success => "success",
        diagnostic::Outcome::Failure => "failure",
    }
}

/// Reads a script outcome from a worker answer.
fn script_outcome_from_name(name: &str) -> Result<diagnostic::Outcome> {
    match name {
        "success" => Ok(diagnostic::Outcome::Success),
        "failure" => Ok(diagnostic::Outcome::Failure),
        _ => Err(Error::from(format!("unknown script outcome {name}"))),
    }
}
