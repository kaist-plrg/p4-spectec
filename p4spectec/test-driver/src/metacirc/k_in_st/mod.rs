//! K definitions run through a K-in-ST tower and compared with `krun`,
//! registered in `suites/metacirc/k-in-st.toml`
//!
//! `suite` reads the registration into definitions and their programs; `build`
//! kompiles each definition, builds its K library, and makes each initial term
//! with krun, into `$CARGO_TARGET_DIR/k-in-st/`. `run` runs each program with
//! `metacirc::run` and compares with `<program>.out` (`compare`): the same first
//! line (steps, how the run ended) and configurations equal in normal form.
//! A run that ends elsewhere passes by search if its final configuration is one
//! of `<program>.search`, the finals K allows, as where rules of the same priority both apply,
//! and one that ends in krun's configuration after other steps passes without a search.
//! `KRUN_EXPECT=1` writes `<program>.out` from krun's own run, and `<program>.search`
//! where the run still differs (`krun`).
//! With a timeout, a worker runs spec-k on each program,
//! and a run still going after the timeout is recorded as a timeout.

mod build;
mod compare;
mod krun;
mod suite;

use std::{
    fs,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use p4spectec::{
    metacirc::{self, TowerConfig},
    runner,
};

pub use self::suite::Registration;
use self::{
    build::Case,
    compare::{compare, reached, same_final},
    suite::Program,
};
use super::config::Configuration;
use crate::harness::{
    self, Error, Result,
    worker::{self, Channel, Reply, Worker},
};

/// How a program's run compares with krun's.
enum Verdict {
    Pass(&'static str),
    Fail(String),
    Timeout,
}

/// Where spec-k runs: in this process, or in a worker.
enum Spec<'a> {
    InProcess(&'a TowerConfig, runner::Config),
    Worker(Worker),
}

impl Spec<'_> {
    /// Runs a definition on an initial term; none after the timeout.
    fn run(&mut self, paths: &[PathBuf; 2], depth: Option<u64>) -> Result<Option<String>> {
        match self {
            Self::InProcess(tower, options) => Ok(Some(run_spec(tower, *options, paths, depth))),
            Self::Worker(worker) => {
                let depth = depth.map_or_else(|| "-".to_owned(), |depth| depth.to_string());
                let request = format!(
                    "{}\t{}\t{depth}",
                    worker::text_path(&paths[0])?,
                    worker::text_path(&paths[1])?
                );
                Ok(match worker.request(&request)? {
                    Reply::Done { details, .. } => Some(details),
                    Reply::Timeout => None,
                })
            }
        }
    }
}

/// spec-k's run of a definition on an initial term, or how the tower failed.
fn run_spec(
    tower: &TowerConfig,
    options: runner::Config,
    paths: &[PathBuf; 2],
    depth: Option<u64>,
) -> String {
    match metacirc::run(tower, &[], paths, options, depth) {
        Ok(output) => output.unwrap_or_default(),
        Err(error) => format!("failed: {error}"),
    }
}

/// Serves `worker k-in-st <config> <tower>`: each request names a definition,
/// an initial term, and a step bound (`-` for none), tab-separated.
pub fn serve(name_config: &str, path_tower: &Path) -> Result<()> {
    let channel = Channel::open()?;
    let registration: Registration = harness::load("metacirc/k-in-st")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    let tower = TowerConfig::from_file(path_tower)?;
    channel.serve(|request| {
        let fields: Vec<&str> = request.split('\t').collect();
        let [definition, init, depth] = fields.as_slice() else {
            return Err(Error::from(format!("malformed K-in-ST request: {request}")));
        };
        let depth = match *depth {
            "-" => None,
            depth => Some(
                depth
                    .parse()
                    .map_err(|error| Error::from(format!("{depth}: {error}")))?,
            ),
        };
        let paths = [PathBuf::from(definition), PathBuf::from(init)];
        Ok(("done".to_owned(), run_spec(&tower, config.runner(), &paths, depth)))
    })
}

/// The directory of what is built for the cases.
fn dir_build() -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR")
        .map_or_else(|| PathBuf::from("p4spectec/target"), PathBuf::from)
        .join("k-in-st")
}

/// Runs the suites under the named configuration, comparing each case with `krun`.
///
/// `name_suite` restricts the run to the registered suite of that name.
/// With a `timeout`, a worker runs spec-k.
pub fn run(name_config: &str, name_suite: Option<&str>, timeout: Option<Duration>) -> Result<()> {
    let registration: Registration = harness::load("metacirc/k-in-st")?;
    let config = Configuration::find(&registration.configs, name_config)?;
    if config.skip_pending("k-in-st") {
        return Ok(());
    }
    if let Some(name) = name_suite
        && !registration.suites.iter().any(|suite| suite.name == name)
    {
        return Err(Error::from(format!("unknown K-in-ST suite {name}")));
    }
    let update = std::env::var_os("KRUN_EXPECT").is_some();
    let text_mode = format!("metacirc k-in-st {}", config.describe());
    let start = Instant::now();
    let mut executed = 0;
    let mut mismatches = Vec::new();
    let mut timeouts = Vec::new();
    for suite in &registration.suites {
        // Skip suites of other names and suites without a tower for the base
        if name_suite.is_some_and(|name| suite.name != name) {
            continue;
        }
        let Some(path_tower) = suite.towers.get(config.base) else {
            continue;
        };
        let tower = TowerConfig::from_file(path_tower)
            .map_err(|error| Error::from(format!("{}: {error}", suite.name)))?;
        let Some(groups) = suite::groups(suite)? else {
            continue;
        };
        let mut spec = match timeout {
            Some(timeout) => {
                let args = vec![
                    "worker".to_owned(),
                    "k-in-st".to_owned(),
                    config.name.clone(),
                    worker::text_path(path_tower)?.to_owned(),
                ];
                Spec::Worker(Worker::new(args, timeout))
            }
            None => Spec::InProcess(&tower, config.runner()),
        };
        for (case, programs) in groups {
            for program in programs {
                let start_program = Instant::now();
                let name = program.expected.display().to_string();
                // A program that cannot be checked fails; the others still run
                let verdict = check(&mut spec, &case, &program, update)
                    .unwrap_or_else(|error| Verdict::Fail(format!("not checked: {error}")));
                let secs = start_program.elapsed().as_secs_f64();
                match &verdict {
                    Verdict::Pass(pass) => eprintln!("{name}: {pass} ({secs:.1}s)"),
                    Verdict::Fail(difference) => {
                        eprintln!("{name}: FAIL, {difference} ({secs:.1}s)");
                        mismatches.push(format!("{name} ({}): {difference}", suite.name));
                    }
                    Verdict::Timeout => {
                        eprintln!("{name}: TIMEOUT ({secs:.1}s)");
                        timeouts.push(name);
                    }
                }
                executed += 1;
            }
        }
    }
    let text_summary = format!(
        "{text_mode}: executed={executed} mismatched={} timeout={} elapsed={:.3}s",
        mismatches.len(),
        timeouts.len(),
        start.elapsed().as_secs_f64()
    );
    eprintln!("{text_summary}");
    // Report every mismatched program before failing
    if !mismatches.is_empty() {
        return Err(Error::from(format!(
            "{text_mode}: runs differ from krun's:\n{}",
            mismatches.join("\n")
        )));
    }
    eprintln!("{text_mode}: all {} runs that ended matched krun's", executed - timeouts.len());
    Ok(())
}

/// Runs a program through the tower and compares the run with krun's, after
/// writing krun's under `update`.
fn check(spec: &mut Spec, case: &Case, program: &Program, update: bool) -> Result<Verdict> {
    let path_init = case.init(program)?;
    let depth = program.depth()?;
    let input = program.input.as_deref();
    let path_expected = program.expected("out");
    let path_search = program.expected("search");
    if update {
        let output = case.krun(&path_init, depth, input)?;
        fs::create_dir_all(program.expected.parent().unwrap_or(Path::new(".")))?;
        fs::write(&path_expected, output + "\n")?;
    }
    let expected = fs::read_to_string(&path_expected).map_err(|error| {
        Error::from(format!("{}: {error} (KRUN_EXPECT=1 writes it)", path_expected.display()))
    })?;
    let paths_program = [case.path_definition(), path_init.clone()];
    let Some(output) = spec.run(&paths_program, depth)? else {
        return Ok(Verdict::Timeout);
    };
    let differs = compare(&case.info, &output, &expected);
    // krun's final configuration is one that search finds
    if differs.is_err() && same_final(&case.info, &output, &expected) {
        if update && path_search.exists() {
            fs::remove_file(&path_search)?;
        }
        return Ok(Verdict::Pass("pass in another order"));
    }
    if update {
        // Only a run that ends elsewhere needs the finals K allows; without them it fails
        if let Err(difference) = &differs {
            if let Err(error) = case.search(&path_init, depth, input, &path_search) {
                let reason = error.to_string();
                let reason = reason.lines().next().unwrap_or_default();
                return Ok(Verdict::Fail(format!(
                    "{difference}, and K's search gave no finals: {reason}"
                )));
            }
        } else if path_search.exists() {
            fs::remove_file(&path_search)?;
        }
    }
    let Err(difference) = differs else {
        return Ok(Verdict::Pass("pass"));
    };
    if !path_search.exists() {
        return Ok(Verdict::Fail(difference));
    }
    let finals = fs::read(&path_search)?;
    Ok(match reached(&case.info, &output, &finals) {
        Ok(()) => Verdict::Pass("pass by search"),
        Err(error) => Verdict::Fail(format!("{difference}, and {error}")),
    })
}
