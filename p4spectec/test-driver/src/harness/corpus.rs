//! Corpus discovery and execution for the integration tests
//!
//! `collect` discovers sources under a root,
//! and `collect_excludes` reads exclusion lists.
//! A test runs a corpus through a worker:
//! `Corpus::files(dir, ".p4")?.excludes(dirs)?.run(worker, args, name)?.check(path_expected)`
//! gives each case the timeout of `P4SPECTEC_TEST_TIMEOUT`, if set,
//! and compares the outcomes with a snapshot,
//! printing the worker's details of every case whose outcome changed.
//! With `P4SPECTEC_TEST_LOG` set to a directory,
//! the run also writes `<name>.log` there: the snapshot's lines in corpus order,
//! each followed by its duration in seconds (empty for an excluded case).

use std::{
    collections::{BTreeMap, BTreeSet},
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Pass,
    Fail,
    Exclude,
    Timeout,
}

impl Outcome {
    /// Spells the outcome as recorded in snapshots.
    pub fn name(self) -> &'static str {
        match self {
            Self::Pass => "pass",
            Self::Fail => "fail",
            Self::Exclude => "exclude",
            Self::Timeout => "timeout",
        }
    }

    /// Reads an outcome spelled as in snapshots.
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
    /// Compares the outcome of every case with the snapshot at `path_expected`.
    ///
    /// Each line is `<outcome>,<paths>`, in corpus order.
    /// A case that timed out keeps its line of the snapshot,
    /// since a timeout says nothing about the outcome.
    /// A changed case with details from the worker prints its paths and details,
    /// the reason the snapshot diff leaves out.
    /// `INSTA_UPDATE=always` rewrites the snapshot instead.
    pub fn check(&self, path_expected: &Path) {
        let count = |outcome| {
            self.outcomes
                .iter()
                .filter(|(_, other, _)| *other == outcome)
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
        // Index the snapshot's lines by case, past its metadata header
        let text_expected = fs::read_to_string(path_expected).unwrap_or_default();
        let (_, text_expected) = text_expected.split_once("\n---\n").unwrap_or_default();
        let lines_expected: BTreeMap<&str, &str> = text_expected
            .lines()
            .filter_map(|line| line.split_once(',').map(|(_, text_case)| (text_case, line)))
            .collect();
        let mut text = String::new();
        for (text_case, outcome, details) in &self.outcomes {
            let line = match (outcome, lines_expected.get(text_case.as_str())) {
                // Keep the recorded outcome of a case that timed out
                (Outcome::Timeout, Some(line)) => {
                    eprintln!("warning: timeout {text_case}");
                    (*line).to_owned()
                }
                // Record any other outcome as reached
                _ => format!("{},{text_case}", outcome.name()),
            };
            // Print why a case changed its outcome
            if lines_expected.get(text_case.as_str()) != Some(&line.as_str()) && !details.is_empty()
            {
                println!("{text_case}");
                for line_details in details.lines() {
                    println!("    {line_details}");
                }
            }
            text.push_str(&line);
            text.push('\n');
        }
        let name = path_expected.file_stem().unwrap().to_str().unwrap();
        insta::with_settings!({
            snapshot_path => path_expected.parent().unwrap(),
            prepend_module_to_snapshot => false,
            omit_expression => true,
        }, {
            insta::assert_snapshot!(name, text);
        });
    }
}
