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

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
    sync::mpsc,
    thread,
    time::Instant,
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
    paths: Vec<PathBuf>,
    path_tower: &'a Path,
    results: Results,
}

/// Runs the suites under the named configuration, AL scripts first, then P4 programs.
///
/// `name_suite` restricts the run to the registered suite of that name,
/// and `input` to the suites whose tower takes that input.
pub fn run(name_config: &str, name_suite: Option<&str>, input: Option<Input>) -> Result<()> {
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
        run_scripts(config, &excludes, &suites_al)?;
    }
    if !suites_p4.is_empty() {
        run_programs(&registration, config, &excludes, &suites_p4)?;
    }
    Ok(())
}

/// Runs P4 suites, preparing each distinct tower once.
fn run_programs(
    registration: &Registration,
    config: &Configuration,
    excludes: &BTreeSet<String>,
    suites: &[SelectedSuite<'_>],
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
        let config_tower = TowerConfig::from_file(suite.path_tower)?;
        let tower = Tower::build(&mut arena, &config_tower, config.runner(), Dummy)?;
        towers.insert(suite.path_tower, tower);
    }
    let includes = &registration.includes;
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
    let progress = corpus::progress_bar(collected)?;
    let mut executed = 0;
    let mut passed = 0;
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
                            // Patch failures abort; evaluation rejection is an outcome
                            let value = tower.patch(&mut arena, program).map_err(|error| {
                                Error::from(format!(
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
                            return Err(Error::from(format!(
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
                suite.results.record(&[path], outcome.name(), "")?;
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
) -> Result<()> {
    let text_mode = format!("metacirc script {}", config.describe());
    // Specification debug premises print to stdout
    let guard_stdout = harness::output::SilenceStdout::new()?;
    let mut executed = 0;
    let mut excluded = 0;
    let mut mismatches = Vec::new();
    for SelectedSuite { suite, path_tower } in suites {
        let outcome_expect = suite.outcome.ok_or_else(|| {
            Error::from(format!("{}: a script suite needs an outcome", suite.name))
        })?;
        let config_tower = TowerConfig::from_file(path_tower)
            .map_err(|error| Error::from(format!("{}: {error}", suite.name)))?;
        for path in corpus::collect(&suite.root, ".watsup")? {
            if path.to_str().is_some_and(|path| excludes.contains(path)) {
                excluded += 1;
                continue;
            }
            let paths_program = std::slice::from_ref(&path);
            // Invalid scripts are test errors, not rejected executions;
            // an IL top level converts the script itself, so it need only elaborate
            let valid = match config_tower.top_lang() {
                Lang::Il => p4spectec::elab(paths_program).map(|_| ()),
                _ => p4spectec::algo(paths_program).map(|_| ()),
            };
            valid.map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
            let accepted =
                metacirc::run(&config_tower, &[], paths_program, config.runner()).is_ok();
            let outcome =
                if accepted { diagnostic::Outcome::Success } else { diagnostic::Outcome::Failure };
            if outcome != outcome_expect {
                mismatches.push(format!(
                    "{} ({}): expected {outcome_expect:?}, got {outcome:?}",
                    path.display(),
                    suite.name
                ));
            }
            executed += 1;
        }
    }
    guard_stdout.finish()?;
    // Report every mismatched script before failing
    if !mismatches.is_empty() {
        return Err(Error::from(format!(
            "{text_mode}: outcomes differ from their suite:\n{}",
            mismatches.join("\n")
        )));
    }
    eprintln!(
        "{text_mode}: all {executed} script outcomes matched their suite, excluded={excluded}"
    );
    Ok(())
}
