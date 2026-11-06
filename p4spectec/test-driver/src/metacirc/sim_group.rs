//! Simulation groups through a metacircular tower, registered in `suites/metacirc/sim-group.toml`
//!
//! `run` runs one registered suite with one group's exclusions
//! on a simulator built from the configuration's P4 tower,
//! and compares every pair's outcome with the group's expected file for that suite.
//! `split` writes the group folders that those exclusions name,
//! dealing each suite's pairs not excluded by `split.excludes` over the groups in turn.
//! Collection and execution are copies of the main driver's `sim`,
//! whose worker side `sim::serve_simulator` answers the requests.
//! With a timeout, a worker runs each test instead.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use p4spectec::{
    lang::data::encoding::Encoding,
    metacirc::{self, TowerConfig},
    sim_plugin::Simulator,
};

use serde::Deserialize;

use super::config::{ByBase, Configuration};
use crate::{
    harness::{
        self, Error, Result,
        corpus::{self, Outcome, Results},
        log::TestLog,
        worker::{self, Channel, Reply, Worker},
    },
    suite::sim,
};

/// Supplies the includes, P4 towers, group split, configurations, suites, and groups.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub includes: Vec<PathBuf>,
    pub towers: ByBase<PathBuf>,
    pub split: Split,
    pub configs: Vec<Configuration>,
    pub suites: Vec<GroupSuite>,
    pub groups: Vec<Group>,
}

/// Names the exclusions of pairs left out of every group and the folder of each group.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Split {
    pub excludes: Vec<PathBuf>,
    pub groups: Vec<PathBuf>,
}

/// Registers paired P4/STF corpus discovery and optional patches.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GroupSuite {
    pub name: String,
    pub arch: String,
    pub p4: PathBuf,
    pub stf: PathBuf,
    pub patches: Option<PathBuf>,
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
const HEADER_GROUP: &str = "# written by metacirc-test sim-group-split\n";

/// Runs the named suite with the named group's exclusions under the named configuration.
///
/// A test still running `timeout` after it starts is recorded as a timeout,
/// and `path_log` names the directory of the timing log.
pub fn run(
    name_config: &str,
    name_suite: &str,
    name_group: &str,
    timeout: Option<Duration>,
    path_log: Option<&Path>,
) -> Result<()> {
    let registration: Registration = harness::load("metacirc/sim-group")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let suite = registration
        .suites
        .iter()
        .find(|suite| suite.name == name_suite)
        .ok_or_else(|| Error::from(format!("unknown simulation group suite {name_suite}")))?;
    let group = registration
        .groups
        .iter()
        .find(|group| group.name == name_group)
        .ok_or_else(|| Error::from(format!("unknown simulation group {name_group}")))?;
    let path_expected = group.expected.get(&suite.name).ok_or_else(|| {
        Error::from(format!("group {} has no expected file for {}", group.name, suite.name))
    })?;
    if config.skip_pending("sim-group") {
        return Ok(());
    }
    let path_tower = registration.towers.get(config.base).ok_or_else(|| {
        Error::from(format!("no {} simulation tower registered", config.base.name()))
    })?;
    let config_tower = TowerConfig::from_file(path_tower)?;
    let text_mode =
        format!("metacirc sim-group {} {} group {}", config.describe(), suite.name, group.name);
    let name_log = format!("metacirc-sim-group-{}-{}-group{}", config.name, suite.name, group.name);
    let start = Instant::now();
    let excludes = collect_excludes(&group.excludes)?;
    let pairs = collect_pairs(suite, &excludes)?;
    let collected = pairs.len();
    let excluded = pairs.iter().filter(|pair| pair.excluded).count();
    let patched = pairs.iter().filter(|pair| pair.patched).count();
    let mut log = TestLog::create(path_log, &name_log, collected)?;
    let text_collect =
        format!("{text_mode}: collected={collected} excluded={excluded}; preparing specification");
    eprintln!("{text_collect}");
    log.note(&text_collect)?;
    // A worker builds its own simulator; otherwise build it here
    let mut worker = None;
    let mut simulator = None;
    if let Some(timeout) = timeout {
        let args = ["worker", "sim-group", &config.name, &suite.arch]
            .map(str::to_owned)
            .to_vec();
        worker = Some(Worker::new(args, timeout));
    } else {
        let start_prepare = Instant::now();
        simulator = Some(metacirc::build_simulator(
            &config_tower,
            &suite.arch,
            config.runner(),
            Encoding::default(),
        )?);
        log.note(&format!(
            "{} specification prepared in {:.1}s",
            suite.arch,
            start_prepare.elapsed().as_secs_f64()
        ))?;
    }
    let includes = &registration.includes;
    let progress = corpus::progress_bar(collected)?;
    let mut results = Results::new(&std::path::absolute(path_expected)?);
    let mut passed = 0;
    let mut timed_out = 0;
    let guard_stdout = harness::output::SilenceStdout::new()?;
    log.suite(&suite.name, &suite.stf)?;
    for pair in &pairs {
        progress.set_message(pair.path_stf.display().to_string());
        let paths = [pair.path_p4.as_path(), pair.path_stf.as_path()];
        // Record excluded pairs without running them
        if pair.excluded {
            results.record(&paths, Outcome::Exclude)?;
            log.skip(&pair.path_stf, Outcome::Exclude.name())?;
            progress.inc(1);
            continue;
        }
        log.begin(&pair.path_stf)?;
        let reply = match (&mut worker, &mut simulator) {
            (Some(worker), _) => {
                let request = format!(
                    "{}\t{}",
                    worker::path_text(&pair.path_p4)?,
                    worker::path_text(&pair.path_stf)?
                );
                worker.request(&request)
            }
            (None, Some(simulator)) => {
                let (status, details) = answer(simulator, includes, &pair.path_p4, &pair.path_stf);
                Ok(Reply::Done { status, details })
            }
            (None, None) => unreachable!("simulator built without a worker"),
        }
        .map_err(|error| {
            Error::from(format!(
                "{}:{}:{}: {error}",
                suite.name,
                pair.path_p4.display(),
                pair.path_stf.display()
            ))
        })?;
        let outcome = match reply {
            Reply::Done { status, details } => {
                let outcome = Outcome::from_name(&status)?;
                // A failing test reports its simulation error as its details
                if outcome == Outcome::Fail {
                    progress.suspend(|| {
                        eprintln!(
                            "{}:{}:{}: {details}",
                            suite.name,
                            pair.path_p4.display(),
                            pair.path_stf.display()
                        )
                    });
                }
                outcome
            }
            Reply::Timeout => Outcome::Timeout,
        };
        match outcome {
            Outcome::Pass => passed += 1,
            Outcome::Timeout => timed_out += 1,
            _ => {}
        }
        results.record(&paths, outcome)?;
        log.end(outcome.name())?;
        progress.inc(1);
    }
    guard_stdout.finish()?;
    progress.finish_with_message("complete");
    let matched = results.check()?;
    let executed = collected - excluded;
    let text_summary = format!(
        "{text_mode}: collected={collected} excluded={excluded} executed={executed} patched={patched} pass={passed} fail={} timeout={timed_out} elapsed={:.3}s{}",
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

/// Writes each suite's group file `<suite>.exclude` into every folder of `split.groups`.
///
/// Each suite's pairs not excluded by `split.excludes`, numbered per suite in pair order,
/// go to the groups in turn, so a suite's groups differ in size by at most one.
pub fn split() -> Result<()> {
    let registration: Registration = harness::load("metacirc/sim-group")?;
    let split = &registration.split;
    if split.groups.is_empty() {
        return Err(Error::from("no simulation group folders registered"));
    }
    let excludes = collect_excludes(&split.excludes)?;
    let mut paths_stf = BTreeSet::new();
    for suite in &registration.suites {
        let mut groups = vec![Vec::new(); split.groups.len()];
        let pairs = collect_pairs(suite, &excludes)?;
        for (index, pair) in pairs.iter().filter(|pair| !pair.excluded).enumerate() {
            // A group file names a pair by its STF, which must name only that pair
            let path_stf = worker::path_text(&pair.path_stf)?.to_owned();
            if !paths_stf.insert(path_stf.clone()) {
                return Err(Error::from(format!("{path_stf} is the STF of two pairs")));
            }
            groups[index % split.groups.len()].push(path_stf);
        }
        for (dir, group) in split.groups.iter().zip(&groups) {
            fs::create_dir_all(dir)?;
            let path = dir.join(format!("{}.exclude", suite.name));
            let text = group
                .iter()
                .fold(HEADER_GROUP.to_owned(), |text, path_stf| text + path_stf + "\n");
            fs::write(&path, text)?;
            eprintln!("{}: {} tests", path.display(), group.len());
        }
    }
    Ok(())
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

struct Input {
    dir: PathBuf,
    path: PathBuf,
    patched: bool,
}

fn collect(dir: &Path, suffix: &str) -> Result<Vec<Input>> {
    corpus::collect(dir, suffix)?
        .into_iter()
        .map(|path| {
            let path = path.strip_prefix(dir)?.to_owned();
            Ok(Input { dir: dir.to_owned(), path, patched: false })
        })
        .collect()
}

fn patch(inputs: &mut [Input], patches: &[Input]) {
    for input in inputs {
        if let Some(patch) = patches
            .iter()
            .find(|patch| patch.path.file_stem() == input.path.file_stem())
        {
            input.dir.clone_from(&patch.dir);
            input.path.clone_from(&patch.path);
            input.patched = true;
        }
    }
}

struct Pair {
    path_p4: PathBuf,
    path_stf: PathBuf,
    patched: bool,
    excluded: bool,
}

/// Pairs the suite's P4 files of its architecture with their STF tests.
fn collect_pairs(suite: &GroupSuite, excludes: &BTreeSet<String>) -> Result<Vec<Pair>> {
    let mut inputs_p4 = collect(&suite.p4, ".p4")?;
    let include = match suite.arch.as_str() {
        "v1model" => "v1model.p4",
        "ebpf" => "ebpf_model.p4",
        "psa" => "bmv2/psa.p4",
        _ => {
            return Err(Error::from(format!("unknown architecture {}", suite.arch)));
        }
    };
    let mut inputs_arch = Vec::new();
    for input in inputs_p4.drain(..) {
        let text = fs::read_to_string(input.dir.join(&input.path))?;
        if text.contains(&format!("#include <{include}>"))
            || text.contains(&format!("#include \"{include}\""))
        {
            inputs_arch.push(input);
        }
    }
    let mut inputs_stf = collect(&suite.stf, ".stf")?;
    if let Some(dir) = &suite.patches {
        patch(&mut inputs_arch, &collect(dir, ".p4")?);
        patch(&mut inputs_stf, &collect(dir, ".stf")?);
    }
    let mut pairs = Vec::new();
    for input_p4 in inputs_arch {
        for input_stf in &inputs_stf {
            let stem_p4 = input_p4.path.file_stem();
            let dir_stf = input_stf.path.parent().unwrap_or(Path::new(""));
            if stem_p4 == Some(dir_stf.as_os_str())
                || (input_p4.path.parent() == input_stf.path.parent()
                    && stem_p4 == input_stf.path.file_stem())
            {
                let path_p4 = input_p4.dir.join(&input_p4.path);
                let path_stf = input_stf.dir.join(&input_stf.path);
                // Exclude the pair when either file is listed
                let excluded = [&path_p4, &path_stf]
                    .iter()
                    .any(|path| excludes.contains(&*path.to_string_lossy()));
                let patched = input_p4.patched || input_stf.patched;
                pairs.push(Pair { path_p4, path_stf, patched, excluded });
            }
        }
    }
    Ok(pairs)
}

/// Runs one STF test, answering pass, or fail with its simulation error.
fn answer(
    simulator: &mut Simulator,
    includes: &[PathBuf],
    path_p4: &Path,
    path_stf: &Path,
) -> (String, String) {
    match simulator.run_stf_test(includes, path_p4, path_stf, |_| {}) {
        Ok(()) => (Outcome::Pass.name().to_owned(), String::new()),
        Err(error) => (Outcome::Fail.name().to_owned(), error.to_string()),
    }
}

/// Serves `worker sim-group <config> <arch>` on the configuration's tower.
pub fn serve(name_config: &str, arch: &str) -> Result<()> {
    let channel = Channel::open()?;
    let registration: Registration = harness::load("metacirc/sim-group")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let path_tower = registration.towers.get(config.base).ok_or_else(|| {
        Error::from(format!("no {} simulation tower registered", config.base.name()))
    })?;
    let config_tower = TowerConfig::from_file(path_tower)?;
    let simulator =
        metacirc::build_simulator(&config_tower, arch, config.runner(), Encoding::default())?;
    sim::serve_simulator(channel, &registration.includes, simulator)
}
