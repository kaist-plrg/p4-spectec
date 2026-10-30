use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use p4spectec::lang::data::encoding::Encoding;

use p4spectec::sim_plugin::{self, SimError, Simulator};

use serde::Deserialize;

use crate::harness::{
    self, Error, Result,
    corpus::{self, Results},
    interpreter::Configuration,
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
    pub suites: Vec<SimSuite>,
}

/// Registers paired P4/STF corpus discovery, optional patches, and expected results.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SimSuite {
    pub name: String,
    pub arch: String,
    pub p4: PathBuf,
    pub stf: PathBuf,
    pub patches: Option<PathBuf>,
    pub expected: PathBuf,
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

impl SimSuite {
    fn collect(&self, excludes: &BTreeSet<String>) -> Result<Vec<Pair>> {
        let mut inputs_p4 = collect(&self.p4, ".p4")?;
        let include = match self.arch.as_str() {
            "v1model" => "v1model.p4",
            "ebpf" => "ebpf_model.p4",
            "psa" => "bmv2/psa.p4",
            _ => {
                return Err(Error::from(format!("unknown architecture {}", self.arch)));
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
        let mut inputs_stf = collect(&self.stf, ".stf")?;
        if let Some(dir) = &self.patches {
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
}

/// Runs registered simulation suites under the named configuration.
///
/// With a `timeout`, a worker runs each test,
/// and a test still running after the timeout is recorded as a timeout.
pub fn run(name_config: &str, timeout: Option<Duration>, path_log: Option<&Path>) -> Result<()> {
    let registration: Registration = harness::load("sim")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let suites = registration.suites.iter().collect::<Vec<_>>();
    let text_mode = format!("sim {}", config.describe());
    let name_log = format!("sim-{}", config.name);
    let timeout =
        timeout.map(|timeout| (timeout, vec!["worker".into(), "sim".into(), config.name.clone()]));
    run_suites(&registration, &suites, &text_mode, &name_log, timeout, path_log, |arch| {
        build(&registration, config, arch)
    })
}

/// Builds the native simulator of an architecture.
fn build(registration: &Registration, config: &Configuration, arch: &str) -> Result<Simulator> {
    let spec = config.language.spec(&registration.spec)?;
    Ok(sim_plugin::build(spec, arch, config.runner(), Encoding::default())?)
}

/// Runs the given suites of `registration` on one simulator per architecture.
///
/// `build_simulator` builds the simulator of an architecture,
/// and every suite compares its outcomes with its own expected file.
/// With `timeout`, a timeout and the worker command without the architecture,
/// a worker of that command and the architecture runs each test instead,
/// and a test still running after the timeout is recorded as a timeout.
pub fn run_suites<BuildSimulator>(
    registration: &Registration,
    suites: &[&SimSuite],
    text_mode: &str,
    name_log: &str,
    timeout: Option<(Duration, Vec<String>)>,
    path_log: Option<&Path>,
    build_simulator: BuildSimulator,
) -> Result<()>
where
    BuildSimulator: Fn(&str) -> Result<Simulator>,
{
    let start = Instant::now();
    let mut excludes = BTreeSet::new();
    for path in &registration.excludes {
        excludes.extend(corpus::collect_excludes(path)?);
    }
    let suites = suites
        .iter()
        .map(|suite| Ok((*suite, suite.collect(&excludes)?)))
        .collect::<Result<Vec<_>>>()?;
    let pairs = suites.iter().flat_map(|(_, pairs)| pairs);
    let collected = pairs.clone().count();
    let excluded = pairs.clone().filter(|pair| pair.excluded).count();
    let patched = pairs.filter(|pair| pair.patched).count();
    let mut log = TestLog::create(path_log, name_log, collected)?;
    let text_collect =
        format!("{text_mode}: collected={collected} excluded={excluded}; preparing specification");
    eprintln!("{text_collect}");
    log.note(&text_collect)?;
    let includes = &registration.includes;
    let progress = corpus::progress_bar(collected)?;
    let mut matched = 0;
    let mut timed_out = 0;
    // Build one simulator per registered architecture in first-suite order
    let mut archs = Vec::new();
    for (suite, _) in &suites {
        if !archs.contains(&suite.arch.as_str()) {
            archs.push(suite.arch.as_str());
        }
    }
    for arch in archs {
        let suites_arch: Vec<_> = suites
            .iter()
            .filter(|(suite, _)| suite.arch == arch)
            .collect();
        let pairs_arch = suites_arch.iter().flat_map(|(_, pairs)| pairs);
        let collected_arch = pairs_arch.clone().count();
        let excluded_arch = pairs_arch.clone().filter(|pair| pair.excluded).count();
        let patched_arch = pairs_arch.filter(|pair| pair.patched).count();
        // A worker builds its own simulator; otherwise build it here
        let mut worker = None;
        let mut simulator = None;
        if let Some((timeout, args)) = &timeout {
            let args = args.iter().cloned().chain([arch.to_owned()]).collect();
            worker = Some(Worker::new(args, *timeout));
        } else {
            let start_arch = Instant::now();
            simulator = Some(build_simulator(arch)?);
            log.note(&format!(
                "{arch} specification prepared in {:.1}s",
                start_arch.elapsed().as_secs_f64()
            ))?;
        }
        for (suite, pairs) in suites_arch {
            let mut results = Results::new(&std::path::absolute(&suite.expected)?);
            let guard_stdout = harness::output::SilenceStdout::new()?;
            log.suite(&suite.name, &suite.stf)?;
            for pair in pairs {
                progress.set_message(pair.path_stf.display().to_string());
                let paths = [pair.path_p4.as_path(), pair.path_stf.as_path()];
                // Record excluded pairs without running them
                if pair.excluded {
                    results.record(&paths, "exclude", "")?;
                    log.skip(&pair.path_stf, "exclude")?;
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
                        execute(simulator, includes, &pair.path_p4, &pair.path_stf)
                            .map(|details| Reply::Done { status: "pass".to_owned(), details })
                            .map_err(|error| Error::from(error.to_string()))
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
                let (status, details) = match reply {
                    Reply::Done { status, details } => {
                        matched += details.lines().count();
                        (status, details)
                    }
                    Reply::Timeout => {
                        timed_out += 1;
                        ("timeout".to_owned(), String::new())
                    }
                };
                results.record(&paths, &status, &details)?;
                log.end(&status)?;
                progress.inc(1);
            }
            guard_stdout.finish()?;
            results.check();
        }
        eprintln!(
            "{text_mode} {arch}: collected={collected_arch} excluded={excluded_arch} executed={} patched={patched_arch}",
            collected_arch - excluded_arch
        );
    }
    progress.finish_with_message("complete");
    let text_summary = format!(
        "{text_mode}: collected={collected} excluded={excluded} executed={} patched={patched} matches={matched} timeout={timed_out} elapsed={:.3}s; all expected records matched",
        collected - excluded,
        start.elapsed().as_secs_f64()
    );
    eprintln!("{text_summary}");
    log.finish(&text_summary)?;
    Ok(())
}

/// Runs one STF test, returning a line per transmitted packet.
fn execute(
    simulator: &mut Simulator,
    includes: &[PathBuf],
    path_p4: &Path,
    path_stf: &Path,
) -> std::result::Result<String, SimError> {
    let mut text_txs = String::new();
    simulator.run_stf_test(includes, path_p4, path_stf, |tx| {
        text_txs.push_str(&format!("tx\t{}\t{}\n", tx.port, tx.packet));
    })?;
    Ok(text_txs)
}

/// Serves `worker sim <config> <arch>` on the native simulator of the architecture.
pub fn serve(name_config: &str, arch: &str) -> Result<()> {
    let channel = Channel::open()?;
    let registration: Registration = harness::load("sim")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let simulator = build(&registration, config, arch)?;
    serve_simulator(channel, &registration.includes, simulator)
}

/// Answers each request `<p4>\t<stf>` with the packets its test transmits.
pub fn serve_simulator(
    channel: Channel,
    includes: &[PathBuf],
    mut simulator: Simulator,
) -> Result<()> {
    channel.serve(|request| {
        let (path_p4, path_stf) = request
            .split_once('\t')
            .ok_or_else(|| Error::from(format!("invalid worker request: {request}")))?;
        let details = execute(&mut simulator, includes, Path::new(path_p4), Path::new(path_stf))
            .map_err(|error| Error::from(error.to_string()))?;
        Ok(("pass".to_owned(), details))
    })
}
