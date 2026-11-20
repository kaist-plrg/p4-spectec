//! Corpus discovery and execution for the integration tests
//!
//! `collect` discovers sources under a root,
//! and `collect_excludes` reads exclusion lists.
//! A test runs a corpus through a worker:
//! `Corpus::files(dir, ".p4")?.excludes(dirs)?.run(worker, args)?.expect_all(Outcome::Pass)`
//! gives each case the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set,
//! prints one line per case, and fails on any case off its outcome.

use std::{
    collections::BTreeSet,
    fs,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use super::{
    Error, Result,
    worker::{self, Reply, Worker},
};

/// Names the environment variable setting the per-case timeout in seconds.
const ENV_TIMEOUT: &str = "P4SPECTEC_TEST_TIMEOUT";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Pass,
    Fail,
    ParseFail,
    ReparseFail,
    RoundtripFail,
    Exclude,
    Timeout,
}

impl Outcome {
    /// Spells the outcome as recorded in expected files.
    pub fn name(self) -> &'static str {
        match self {
            Self::Pass => "pass",
            Self::Fail => "fail",
            Self::ParseFail => "parse-fail",
            Self::ReparseFail => "reparse-fail",
            Self::RoundtripFail => "roundtrip-fail",
            Self::Exclude => "exclude",
            Self::Timeout => "timeout",
        }
    }

    /// Reads an outcome spelled as in expected files.
    pub fn from_name(name: &str) -> Result<Self> {
        [
            Self::Pass,
            Self::Fail,
            Self::ParseFail,
            Self::ReparseFail,
            Self::RoundtripFail,
            Self::Exclude,
            Self::Timeout,
        ]
        .into_iter()
        .find(|outcome| outcome.name() == name)
        .ok_or_else(|| Error::from(format!("unknown outcome {name}")))
    }
}

pub fn collect(dir: &Path, suffix: &str) -> Result<Vec<PathBuf>> {
    let mut paths = fs::read_dir(dir)?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::io::Result<Vec<_>>>()?;
    paths.sort();
    let mut files = Vec::new();
    for path in paths {
        if fs::metadata(&path)?.is_dir() {
            files.extend(collect(&path, suffix)?);
        } else if path.to_str().is_some_and(|path| path.ends_with(suffix)) {
            files.push(path);
        }
    }
    Ok(files)
}

pub fn collect_excludes(dir: &Path) -> Result<BTreeSet<String>> {
    let mut excludes = BTreeSet::new();
    for path in collect(dir, ".exclude")? {
        let text = fs::read_to_string(path)?;
        // Split on LF only; preserve whitespace and CR in exclusion entries
        excludes.extend(
            text.split_terminator('\n')
                .filter(|line| !line.starts_with('#'))
                .map(str::to_owned),
        );
    }
    Ok(excludes)
}

/// Returns the repository root, against which corpus paths are spelled.
pub fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// Cases of one corpus, each a list of paths relative to the repository root,
/// and the exclusion lists that skip some of them.
pub struct Corpus {
    cases: Vec<Vec<PathBuf>>,
    excludes: BTreeSet<String>,
}

impl Corpus {
    /// Makes a corpus of the given cases.
    pub fn new(cases: Vec<Vec<PathBuf>>) -> Self {
        Self { cases, excludes: BTreeSet::new() }
    }

    /// Collects one case per file ending in `suffix` under `dir`.
    pub fn files(dir: &str, suffix: &str) -> Result<Self> {
        let root = root();
        let cases = collect(&root.join(dir), suffix)?
            .into_iter()
            .map(|path| Ok(vec![path.strip_prefix(&root)?.to_path_buf()]))
            .collect::<Result<_>>()?;
        Ok(Self::new(cases))
    }

    /// Skips the cases with a path named by the `.exclude` files under `dirs`.
    pub fn excludes(mut self, dirs: &[&str]) -> Result<Self> {
        for dir in dirs {
            self.excludes.extend(collect_excludes(&root().join(dir))?);
        }
        Ok(self)
    }

    /// Runs every case not excluded in a worker of `path_program` started with `args`.
    ///
    /// Each request is the case's paths separated by tabs,
    /// and the worker answers its outcome name and details;
    /// with `P4SPECTEC_TEST_TIMEOUT` set,
    /// a case still running after that many seconds is recorded as a timeout,
    /// and the worker restarts for the next case.
    pub fn run(&self, path_program: &str, args: &[&str]) -> Result<Report> {
        // Read the per-case timeout; without one, wait for every case
        let timeout = match std::env::var(ENV_TIMEOUT) {
            Ok(text) => Some(Duration::from_secs(
                text.parse()
                    .map_err(|error| Error::from(format!("{ENV_TIMEOUT}={text}: {error}")))?,
            )),
            Err(_) => None,
        };
        let args = args.iter().map(|arg| (*arg).to_owned()).collect();
        let mut worker = Worker::new(PathBuf::from(path_program), args, timeout);
        let mut report = Report { outcomes: Vec::new() };
        for paths in &self.cases {
            let text_case = paths
                .iter()
                .map(|path| path.display().to_string())
                .collect::<Vec<_>>()
                .join(" ");
            // Record excluded cases without running them
            if paths.iter().any(|path| {
                path.to_str()
                    .is_some_and(|path| self.excludes.contains(path))
            }) {
                report.outcomes.push((text_case, Outcome::Exclude));
                continue;
            }
            // Run the case in the worker
            let request = paths
                .iter()
                .map(|path| worker::text_path(path))
                .collect::<Result<Vec<_>>>()?
                .join("\t");
            let start = Instant::now();
            let (outcome, details) = match worker.request(&request)? {
                Reply::Done { status, details } => (Outcome::from_name(&status)?, details),
                Reply::Timeout => (Outcome::Timeout, String::new()),
            };
            // Print its outcome and duration, then any details indented
            let secs = start.elapsed().as_secs_f64();
            println!("{:<7} {secs:>7.2}s {text_case}", outcome.name());
            for line in details.lines() {
                println!("    {line}");
            }
            report.outcomes.push((text_case, outcome));
        }
        Ok(report)
    }
}

/// Outcomes of one corpus run, in corpus order.
pub struct Report {
    outcomes: Vec<(String, Outcome)>,
}

impl Report {
    /// Fails unless every case run reached `outcome_expect`.
    ///
    /// Excluded cases are skipped, and a timeout only warns.
    pub fn expect_all(&self, outcome_expect: Outcome) -> Result<()> {
        let count = |outcome| {
            self.outcomes
                .iter()
                .filter(|(_, other)| *other == outcome)
                .count()
        };
        println!(
            "collected={} excluded={} pass={} fail={} timeout={}",
            self.outcomes.len(),
            count(Outcome::Exclude),
            count(Outcome::Pass),
            count(Outcome::Fail),
            count(Outcome::Timeout),
        );
        let mut texts_wrong = Vec::new();
        for (text_case, outcome) in &self.outcomes {
            match outcome {
                // Skip cases the exclusion lists name
                Outcome::Exclude => {}
                // Warn on a timeout, which says nothing about the outcome
                Outcome::Timeout => eprintln!("warning: timeout {text_case}"),
                // Collect cases off the expected outcome
                _ if *outcome != outcome_expect => {
                    texts_wrong.push(format!("{} {text_case}", outcome.name()));
                }
                // Accept cases that reached it
                _ => {}
            }
        }
        if texts_wrong.is_empty() {
            return Ok(());
        }
        Err(Error::from(format!(
            "{} cases did not {}:\n{}",
            texts_wrong.len(),
            outcome_expect.name(),
            texts_wrong.join("\n")
        )))
    }
}
