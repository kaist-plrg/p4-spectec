//! Source discovery and per-file results for corpus suites
//!
//! `collect` discovers sources under a root,
//! and `collect_excludes` reads exclusion lists.
//! `Results` compares per-test outcomes with one CSV expected file per suite.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
};

use indicatif::{ProgressBar, ProgressStyle};

use super::{Error, Result};

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
            if path.file_name().is_some_and(|name| name != "include") {
                files.extend(collect(&path, suffix)?);
            }
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

/// Collects per-test outcomes of one suite for comparison with its expected file.
///
/// The expected file is CSV without a header or quoting,
/// one line `<outcome>,<path>...` per test.
pub struct Results {
    path_expected: PathBuf,
    outcomes: BTreeMap<Vec<PathBuf>, Outcome>,
}

impl Results {
    pub fn new(path_expected: &Path) -> Self {
        Self { path_expected: path_expected.to_path_buf(), outcomes: BTreeMap::new() }
    }

    /// Records one test under its paths, which must be unique and CSV-safe.
    pub fn record(&mut self, paths: &[&Path], outcome: Outcome) -> Result<()> {
        for path in paths {
            if !path
                .to_str()
                .is_some_and(|path| !path.contains([',', '"', '\r', '\n']))
            {
                return Err(Error::from(format!("invalid result path: {}", path.display())));
            }
        }
        let key = paths.iter().map(|path| path.to_path_buf()).collect();
        if self.outcomes.insert(key, outcome).is_some() {
            return Err(Error::from(format!("duplicate result: {}", paths[0].display())));
        }
        Ok(())
    }

    /// Compares each test's outcome with the expected file, printing mismatches to stderr.
    ///
    /// A timeout matches any expected outcome with a warning.
    /// Returns whether every test matched.
    pub fn check(self) -> Result<bool> {
        let display = self.path_expected.display();
        let text = fs::read_to_string(&self.path_expected)
            .map_err(|error| Error::from(format!("{display}: {error}")))?;
        let mut expected = BTreeMap::new();
        for (index, line) in text.lines().enumerate() {
            let number = index + 1;
            let mut fields = line.split(',');
            let outcome = Outcome::from_name(fields.next().unwrap_or_default())
                .map_err(|error| Error::from(format!("{display}:{number}: {error}")))?;
            let paths = fields.map(PathBuf::from).collect::<Vec<_>>();
            if paths.is_empty() {
                return Err(Error::from(format!("{display}:{number}: no test paths")));
            }
            if expected.insert(paths, (number, outcome)).is_some() {
                return Err(Error::from(format!("{display}:{number}: duplicate test")));
            }
        }
        let mut matched = true;
        for (paths, outcome) in self.outcomes {
            let text_paths = text_paths(&paths);
            match expected.remove(&paths) {
                None => {
                    matched = false;
                    eprintln!("{display}: {text_paths}: got {}, not expected", outcome.name());
                }
                Some((number, outcome_expected)) if outcome == Outcome::Timeout => {
                    eprintln!(
                        "warning: {display}:{number}: {text_paths}: timeout, expected {}",
                        outcome_expected.name()
                    );
                }
                Some((number, outcome_expected)) if outcome != outcome_expected => {
                    matched = false;
                    eprintln!(
                        "{display}:{number}: {text_paths}: expected {}, got {}",
                        outcome_expected.name(),
                        outcome.name()
                    );
                }
                Some(_) => {}
            }
        }
        let mut unrun = expected.into_iter().collect::<Vec<_>>();
        unrun.sort_by_key(|(_, (number, _))| *number);
        for (paths, (number, outcome_expected)) in unrun {
            matched = false;
            eprintln!(
                "{display}:{number}: {}: expected {}, not run",
                text_paths(&paths),
                outcome_expected.name()
            );
        }
        Ok(matched)
    }
}

/// Joins test paths as in expected files.
fn text_paths(paths: &[PathBuf]) -> String {
    paths
        .iter()
        .map(|path| path.display().to_string())
        .collect::<Vec<_>>()
        .join(",")
}

/// Creates a progress bar over `len` tests.
pub fn progress_bar(len: usize) -> Result<ProgressBar> {
    let style = ProgressStyle::with_template("[{bar:24}] {pos}/{len} {elapsed_precise} {msg}")?;
    Ok(ProgressBar::new(len as u64).with_style(style))
}
