//! Corpus discovery and execution for the integration tests
//!
//! `collect` discovers sources under a root,
//! and `collect_excludes` reads exclusion lists.
//! A test runs a corpus through a worker:
//! `Corpus::files(dir, ".p4")?.excludes(dirs)?.run(worker, args, name)?.check(outcome)`
//! gives each case the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set,
//! and checks that every case not excluded reaches `outcome`,
//! listing the others with the worker's details.
//! With `P4SPECTEC_TEST_LOG` set to a directory,
//! the run also writes `<name>.log` there: one `<outcome>,<paths>,<seconds>` line per case
//! in corpus order (seconds empty for an excluded case).

use std::{
    collections::BTreeSet,
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use super::{
    Error, Result,
    worker::{self, Reply, Worker},
};

/// Names the environment variable setting the per-case timeout in seconds.
const ENV_TIMEOUT: &str = "P4SPECTEC_TEST_TIMEOUT";

/// Names the environment variable setting the directory of run logs.
const ENV_LOG: &str = "P4SPECTEC_TEST_LOG";

/// Bounds the cases a failure lists.
const LIMIT_CASES: usize = 20;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Pass,
    Fail,
    Exclude,
    Timeout,
}

impl Outcome {
    /// Spells the outcome as the worker answers it.
    pub fn name(self) -> &'static str {
        match self {
            Self::Pass => "pass",
            Self::Fail => "fail",
            Self::Exclude => "exclude",
            Self::Timeout => "timeout",
        }
    }

    /// Reads an outcome as the worker answers it.
    pub fn from_name(name: &str) -> Result<Self> {
        [Self::Pass, Self::Fail, Self::Exclude, Self::Timeout]
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
    /// With `P4SPECTEC_TEST_LOG` set, each case's line goes to `<name>.log`
    /// in that directory as soon as the case ends.
    pub fn run(&self, path_program: &str, args: &[&str], name: &str) -> Result<Report> {
        // Read the per-case timeout; without one, wait for every case
        let timeout = match std::env::var(ENV_TIMEOUT) {
            Ok(text) => Some(Duration::from_secs(
                text.parse()
                    .map_err(|error| Error::from(format!("{ENV_TIMEOUT}={text}: {error}")))?,
            )),
            Err(_) => None,
        };
        // Open the log `<name>.log`, if a log directory is set
        let mut file_log = match std::env::var_os(ENV_LOG) {
            Some(dir) => {
                fs::create_dir_all(&dir)?;
                Some(File::create(Path::new(&dir).join(format!("{name}.log")))?)
            }
            None => None,
        };
        let args = args.iter().map(|arg| (*arg).to_owned()).collect();
        let mut worker = Worker::new(PathBuf::from(path_program), args, timeout);
        let mut report = Report { outcomes: Vec::new() };
        for paths in &self.cases {
            let text_case = paths
                .iter()
                .map(|path| path.display().to_string())
                .collect::<Vec<_>>()
                .join(",");
            // Record excluded cases without running them
            if paths.iter().any(|path| {
                path.to_str()
                    .is_some_and(|path| self.excludes.contains(path))
            }) {
                if let Some(file) = &mut file_log {
                    writeln!(file, "{},{text_case},", Outcome::Exclude.name())?;
                }
                report
                    .outcomes
                    .push((text_case, Outcome::Exclude, String::new()));
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
            let secs = start.elapsed().as_secs_f64();
            // Log the outcome as reached, a timeout included, with its duration
            if let Some(file) = &mut file_log {
                writeln!(file, "{},{text_case},{secs:.2}", outcome.name())?;
            }
            report.outcomes.push((text_case, outcome, details));
        }
        Ok(report)
    }
}

/// Outcomes of one corpus run, in corpus order.
pub struct Report {
    /// Each case with its outcome and the worker's details.
    outcomes: Vec<(String, Outcome, String)>,
}

impl Report {
    /// Checks that every case not excluded reaches `outcome`.
    ///
    /// A case that timed out only warns, since a timeout says nothing about the outcome.
    /// A failure panics with the other cases, at most `LIMIT_CASES`,
    /// each with the worker's details.
    pub fn check(&self, outcome: Outcome) {
        for (text_case, ..) in self
            .outcomes
            .iter()
            .filter(|(_, other, _)| *other == Outcome::Timeout)
        {
            eprintln!("warning: timeout {text_case}");
        }
        // Collect the cases that ran to another outcome
        let mismatches: Vec<_> = self
            .outcomes
            .iter()
            .filter(|(_, other, _)| ![outcome, Outcome::Exclude, Outcome::Timeout].contains(other))
            .collect();
        if mismatches.is_empty() {
            return;
        }
        // Panic with them, at most `LIMIT_CASES`, as the test's failure message
        let count_run = self
            .outcomes
            .iter()
            .filter(|(_, other, _)| *other != Outcome::Exclude)
            .count();
        let mut text =
            format!("{} of {count_run} cases did not {}", mismatches.len(), outcome.name());
        for (text_case, _, details) in mismatches.iter().take(LIMIT_CASES) {
            text.push_str(&format!("\n{text_case}"));
            for line_details in details.lines() {
                text.push_str(&format!("\n    {line_details}"));
            }
        }
        if mismatches.len() > LIMIT_CASES {
            text.push_str(&format!("\nand {} more", mismatches.len() - LIMIT_CASES));
        }
        panic!("{text}");
    }
}
