use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
    time::Instant,
};

use p4spectec::lang::data::encoding::Encoding;

use p4spectec::sim_plugin::{self};

use serde::Deserialize;

use crate::{
    Error, Result,
    corpus::{self, Results, log::TestLog},
    util::{self, Configuration},
};

/// Supplies simulation exclusions, configurations, and P4/STF registrations.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub excludes: Vec<PathBuf>,
    pub configs: Vec<Configuration>,
    pub suites: Vec<SimSuite>,
}

/// Registers paired P4/STF corpus discovery and optional patches,
/// expected in `expected/sim/<name>.expected`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SimSuite {
    pub name: String,
    pub arch: String,
    pub p4: PathBuf,
    pub stf: PathBuf,
    pub patches: Option<PathBuf>,
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
pub fn run(name_config: &str, path_log: Option<&Path>) -> Result<()> {
    let registration: Registration = util::load("sim")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let text_mode = format!("sim {}", config.describe());
    let start = Instant::now();
    let mut excludes = BTreeSet::new();
    for path in &registration.excludes {
        excludes.extend(corpus::collect_excludes(path)?);
    }
    let suites = registration
        .suites
        .iter()
        .map(|suite| Ok((suite, suite.collect(&excludes)?)))
        .collect::<Result<Vec<_>>>()?;
    let pairs = suites.iter().flat_map(|(_, pairs)| pairs);
    let collected = pairs.clone().count();
    let excluded = pairs.clone().filter(|pair| pair.excluded).count();
    let patched = pairs.filter(|pair| pair.patched).count();
    let mut log = TestLog::create(path_log, &format!("sim-{}", config.name), collected)?;
    let text_collect =
        format!("{text_mode}: collected={collected} excluded={excluded}; preparing specification");
    eprintln!("{text_collect}");
    log.note(&text_collect)?;
    let includes = util::includes();
    let progress = util::progress_bar(collected)?;
    let mut matched = 0;
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
        let start_arch = Instant::now();
        let spec = config.language.spec(&util::spec())?;
        let config_runner = config.runner();
        let mut simulator = sim_plugin::build(spec, arch, config_runner, Encoding::default())?;
        log.note(&format!(
            "{arch} specification prepared in {:.1}s",
            start_arch.elapsed().as_secs_f64()
        ))?;
        for (suite, pairs) in suites_arch {
            let mut results = Results::new(&util::expected_path("sim", &suite.name));
            let guard_stdout = super::output::SilenceStdout::new()?;
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
                let mut text_txs = String::new();
                simulator
                    .run_stf_test(&includes, &pair.path_p4, &pair.path_stf, |tx| {
                        matched += 1;
                        text_txs.push_str(&format!("tx\t{}\t{}\n", tx.port, tx.packet));
                    })
                    .map_err(|error| {
                        Error::from(format!(
                            "{}:{}:{}: {error}",
                            suite.name,
                            pair.path_p4.display(),
                            pair.path_stf.display()
                        ))
                    })?;
                results.record(&paths, "pass", &text_txs)?;
                log.end("pass")?;
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
        "{text_mode}: collected={collected} excluded={excluded} executed={} patched={patched} matches={matched} elapsed={:.3}s; all expected records matched",
        collected - excluded,
        start.elapsed().as_secs_f64()
    );
    eprintln!("{text_summary}");
    log.finish(&text_summary)?;
    Ok(())
}
