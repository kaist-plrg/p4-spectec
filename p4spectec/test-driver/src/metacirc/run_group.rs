//! P4 corpus groups through a metacircular tower, registered in `suites/metacirc/run-group.toml`
//!
//! `run` runs one registered suite with one group's exclusions
//! through the suite's P4 tower for the configuration's base,
//! and compares every program's outcome with the group's expected file for that suite.
//! `split` writes the group folders that those exclusions name,
//! cutting the programs not excluded by `split.excludes` in order into groups of given sizes,
//! and checks that the registered groups run each of those programs exactly once.
//! Collection and execution are copies of `run`'s P4 programs.
//! With a timeout, a worker runs each program instead.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
    sync::mpsc,
    thread,
    time::{Duration, Instant},
};

use serde::Deserialize;

use p4spectec::{
    interface::p4::{
        error::{P4Error, P4ErrorKind},
        parse::parse_string,
        preprocessor::preprocess,
    },
    lang::data::arena::Arena,
    metacirc::{self, Tower, TowerConfig, config::Lang},
    sim_plugin::dummy::Dummy,
};

use super::config::{ByBase, Configuration};
use crate::harness::{
    self, Error, Result,
    corpus::{self, Outcome, Results},
    log::TestLog,
    worker::{self, Channel, Reply, Worker},
};

/// Supplies the includes, group split, configurations, suites, and groups.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub includes: Vec<PathBuf>,
    pub split: Split,
    pub configs: Vec<Configuration>,
    pub suites: Vec<GroupSuite>,
    pub groups: Vec<Group>,
}

/// Names the exclusions of programs left out of every group and the folder of each group.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Split {
    pub excludes: Vec<PathBuf>,
    pub groups: Vec<PathBuf>,
}

/// Registers the .p4 files under `root` and one P4 tower per base.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GroupSuite {
    pub name: String,
    pub towers: ByBase<PathBuf>,
    pub root: PathBuf,
}

/// Registers the exclusions of one group and its expected file per suite name.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Group {
    pub name: String,
    pub excludes: Vec<PathBuf>,
    pub expected: BTreeMap<String, PathBuf>,
}

/// Starts each written group file.
const HEADER_GROUP: &str = "# written by metacirc-test run-group-split\n";

/// Runs the named suite with the named group's exclusions under the named configuration.
///
/// A program still running `timeout` after it starts is recorded as a timeout,
/// and `path_log` names the directory of the timing log.
pub fn run(
    name_config: &str,
    name_suite: &str,
    name_group: &str,
    timeout: Option<Duration>,
    path_log: Option<&Path>,
) -> Result<()> {
    let registration: Registration = harness::load("metacirc/run-group")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let suite = registration
        .suites
        .iter()
        .find(|suite| suite.name == name_suite)
        .ok_or_else(|| Error::from(format!("unknown run group suite {name_suite}")))?;
    let group = registration
        .groups
        .iter()
        .find(|group| group.name == name_group)
        .ok_or_else(|| Error::from(format!("unknown run group {name_group}")))?;
    let path_expected = group.expected.get(&suite.name).ok_or_else(|| {
        Error::from(format!("group {} has no expected file for {}", group.name, suite.name))
    })?;
    if config.skip_pending("run-group") {
        return Ok(());
    }
    let path_tower = suite.towers.get(config.base).ok_or_else(|| {
        Error::from(format!("{}: no {} tower registered", suite.name, config.base.name()))
    })?;
    let config_tower = TowerConfig::from_file(path_tower)
        .map_err(|error| Error::from(format!("{}: {error}", suite.name)))?;
    if config_tower.top_lang() != Lang::P4 {
        return Err(Error::from(format!("{}: the tower does not take P4", suite.name)));
    }
    let text_mode =
        format!("metacirc run-group {} {} group {}", config.describe(), suite.name, group.name);
    let name_log = format!("metacirc-run-group-{}-{}-group{}", config.name, suite.name, group.name);
    let start = Instant::now();
    let excludes = collect_excludes(&group.excludes)?;
    let programs = collect_programs(suite, &excludes)?;
    let collected = programs.len();
    let excluded = programs.iter().filter(|program| program.excluded).count();
    let mut log = TestLog::create(path_log, &name_log, collected)?;
    let text_collect =
        format!("{text_mode}: collected={collected} excluded={excluded}; preparing specification");
    eprintln!("{text_collect}");
    log.note(&text_collect)?;
    // A worker builds its own tower; otherwise build it here
    let mut arena = Arena::new();
    let mut worker = None;
    let mut tower = None;
    if let Some(timeout) = timeout {
        let args = vec![
            "worker".to_owned(),
            "run-group".to_owned(),
            config.name.clone(),
            worker::text_path(path_tower)?.to_owned(),
        ];
        worker = Some(Worker::new(args, timeout));
    } else {
        let start_prepare = Instant::now();
        tower = Some(Tower::build(&mut arena, &config_tower, config.runner(), Dummy)?);
        log.note(&format!("tower prepared in {:.1}s", start_prepare.elapsed().as_secs_f64()))?;
    }
    let includes = &registration.includes;
    for path in includes {
        fs::read_dir(path)?;
    }
    // Preprocess upcoming files while the tower parses and evaluates in order
    let paths_preprocess: Vec<_> = programs
        .iter()
        .filter(|program| timeout.is_none() && !program.excluded)
        .map(|program| program.path.clone())
        .collect();
    let progress = corpus::progress_bar(collected)?;
    let mut results = Results::new(&std::path::absolute(path_expected)?);
    let mut passed = 0;
    let mut timed_out = 0;
    let guard_stdout = harness::output::SilenceStdout::new()?;
    log.suite(&suite.name, &suite.root)?;
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
        for program in &programs {
            let path = program.path.as_path();
            progress.set_message(path.display().to_string());
            // Record excluded programs without running them
            if program.excluded {
                results.record(&[path], Outcome::Exclude)?;
                log.skip(path, Outcome::Exclude.name())?;
                progress.inc(1);
                continue;
            }
            log.begin(path)?;
            let outcome = match (&mut worker, &mut tower) {
                (Some(worker), _) => match worker.request(worker::text_path(path)?)? {
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
                (None, None) => unreachable!("tower built without a worker"),
            };
            match outcome {
                Outcome::Pass => passed += 1,
                Outcome::Timeout => timed_out += 1,
                _ => {}
            }
            results.record(&[path], outcome)?;
            log.end(outcome.name())?;
            progress.inc(1);
        }
        Ok(())
    })?;
    guard_stdout.finish()?;
    progress.finish_with_message("complete");
    let matched = results.check()?;
    let executed = collected - excluded;
    let text_summary = format!(
        "{text_mode}: collected={collected} excluded={excluded} executed={executed} pass={passed} fail={} timeout={timed_out} elapsed={:.3}s{}",
        executed - passed - timed_out,
        start.elapsed().as_secs_f64(),
        if matched { "; all expected records matched" } else { "" }
    );
    eprintln!("{text_summary}");
    log.finish(&text_summary)?;
    if !matched {
        return Err(Error::from(format!("{text_mode}: records differ from expected")));
    }
    Ok(())
}

/// Writes the suite's group file `<suite>.exclude` into every folder of `split.groups`.
///
/// The suite's programs not excluded by `split.excludes`, in collection order,
/// fill the groups in turn, the group of each folder taking as many as its entry of `sizes`.
/// The split takes exactly one registered suite and a size per folder
/// summing to the number of its programs.
/// Then checks the registered groups with `check_groups`.
pub fn split(sizes: &[usize]) -> Result<()> {
    let registration: Registration = harness::load("metacirc/run-group")?;
    let split = &registration.split;
    if split.groups.is_empty() {
        return Err(Error::from("no run group folders registered"));
    }
    if sizes.len() != split.groups.len() {
        return Err(Error::from(format!(
            "{} group sizes given for {} group folders",
            sizes.len(),
            split.groups.len()
        )));
    }
    let [suite] = registration.suites.as_slice() else {
        return Err(Error::from(format!(
            "the split takes one registered suite, not {}",
            registration.suites.len()
        )));
    };
    let excludes = collect_excludes(&split.excludes)?;
    let paths = collect_programs(suite, &excludes)?
        .into_iter()
        .filter(|program| !program.excluded)
        .map(|program| Ok(worker::text_path(&program.path)?.to_owned()))
        .collect::<Result<Vec<_>>>()?;
    let total: usize = sizes.iter().sum();
    if total != paths.len() {
        return Err(Error::from(format!(
            "group sizes sum to {total}, but {} has {} programs to split",
            suite.name,
            paths.len()
        )));
    }
    let mut rest = paths.as_slice();
    for (dir, &size) in split.groups.iter().zip(sizes) {
        let (group, tail) = rest.split_at(size);
        rest = tail;
        fs::create_dir_all(dir)?;
        let path = dir.join(format!("{}.exclude", suite.name));
        let text = group
            .iter()
            .fold(HEADER_GROUP.to_owned(), |text, path_p4| text + path_p4 + "\n");
        fs::write(&path, text)?;
        eprintln!("{}: {} tests", path.display(), group.len());
    }
    check_groups(&registration)
}

/// Checks that the registered groups together run every program
/// not excluded by `split.excludes`, each in exactly one group, and no other program.
fn check_groups(registration: &Registration) -> Result<()> {
    let excludes_split = collect_excludes(&registration.split.excludes)?;
    let excludes_groups = registration
        .groups
        .iter()
        .map(|group| Ok((group.name.as_str(), collect_excludes(&group.excludes)?)))
        .collect::<Result<Vec<_>>>()?;
    let mut problems = Vec::new();
    for suite in &registration.suites {
        let programs = collect_programs(suite, &BTreeSet::new())?;
        for program in &programs {
            let names = excludes_groups
                .iter()
                .filter(|(_, excludes)| !excludes_path(excludes, &program.path))
                .map(|(name, _)| *name)
                .collect::<Vec<_>>();
            let kept = !excludes_path(&excludes_split, &program.path);
            let problem = match (kept, names.as_slice()) {
                (true, [_]) | (false, []) => continue,
                (true, []) => "run by no group".to_owned(),
                (true, _) => format!("run by groups {}", names.join(", ")),
                (false, _) => {
                    format!("excluded by the split but run by groups {}", names.join(", "))
                }
            };
            problems.push(format!("{}: {}: {problem}", suite.name, program.path.display()));
        }
    }
    if problems.is_empty() {
        eprintln!("groups cover every program once over {} suites", registration.suites.len());
        return Ok(());
    }
    Err(Error::from(format!(
        "{} programs not run by exactly one group:\n{}",
        problems.len(),
        problems.join("\n")
    )))
}

/// Tells whether the exclusions list a path.
fn excludes_path(excludes: &BTreeSet<String>, path: &Path) -> bool {
    path.to_str().is_some_and(|path| excludes.contains(path))
}

/// Collects the entries of the .exclude files under each directory.
fn collect_excludes(dirs: &[PathBuf]) -> Result<BTreeSet<String>> {
    let mut excludes = BTreeSet::new();
    for dir in dirs {
        excludes.extend(
            corpus::collect_excludes(dir)
                .map_err(|error| Error::from(format!("{}: {error}", dir.display())))?,
        );
    }
    Ok(excludes)
}

struct Program {
    path: PathBuf,
    excluded: bool,
}

/// Collects the suite's .p4 files under its root.
fn collect_programs(suite: &GroupSuite, excludes: &BTreeSet<String>) -> Result<Vec<Program>> {
    Ok(corpus::collect(&suite.root, ".p4")?
        .into_iter()
        .map(|path| {
            let excluded = excludes_path(excludes, &path);
            Program { path, excluded }
        })
        .collect())
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
            let values = tower
                .encode_input(arena, &metacirc::Input::P4(program))
                .map_err(|error| {
                    Error::from(format!("{}: test execution error: {error}", path.display()))
                })?;
            let accepted = tower.eval_input(arena, &values).is_ok();
            Ok(if accepted { Outcome::Pass } else { Outcome::Fail })
        }
        Err(P4Error { kind: P4ErrorKind::Syntax(_), .. }) => Ok(Outcome::Fail),
        Err(error) => Err(Error::from(format!(
            "{}: test execution error: {}",
            path.display(),
            error.into_report()
        ))),
    }
}

/// Serves `worker run-group <config> <tower>`: each request names one P4 program.
pub fn serve(name_config: &str, path_tower: &Path) -> Result<()> {
    let channel = Channel::open()?;
    let registration: Registration = harness::load("metacirc/run-group")?;
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
