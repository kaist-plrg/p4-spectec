//! Corpus runners over P4 and spec-language source directories
//!
//! `collect` discovers sources under a root,
//! and `collect_excludes` reads exclusion lists.
//! `Results` compares per-file outcomes with one expected file per suite.

mod log;
mod output;

pub mod micro;
pub mod p4parse;
pub mod run;
pub mod sim;

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
};

use expect_test::ExpectFile;

use crate::{Error, Result};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Pass,
    Fail,
    ParseFail,
    ReparseFail,
    RoundtripFail,
    Exclude,
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
        }
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

pub struct Results {
    expected: ExpectFile,
    records: BTreeMap<PathBuf, Outcome>,
}

impl Results {
    pub fn new(expected: ExpectFile) -> Self {
        Self { expected, records: BTreeMap::new() }
    }

    pub fn record(&mut self, path: &Path, outcome: Outcome) -> Result<()> {
        if !path
            .to_str()
            .is_some_and(|path| !path.contains(['\t', '\r', '\n']))
        {
            return Err(Error::from(format!("invalid result path: {}", path.display())));
        }
        if self.records.insert(path.to_owned(), outcome).is_some() {
            return Err(Error::from(format!("duplicate result: {}", path.display())));
        }
        Ok(())
    }

    pub fn check(&self) {
        let mut actual = String::new();
        for (path, outcome) in &self.records {
            actual.push_str(outcome.name());
            actual.push('\t');
            actual.push_str(&path.to_string_lossy());
            actual.push('\n');
        }
        self.expected.assert_eq(&actual);
    }
}
