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

use expect_test::{ExpectFile, expect_file};

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

/// Collects per-test records of one suite for comparison with its expected file.
///
/// Each record is a status line, `<status>\t<path>...`,
/// followed by optional detail lines;
/// records are sorted by their paths.
pub struct Results {
    expected: ExpectFile,
    records: BTreeMap<Vec<PathBuf>, String>,
}

impl Results {
    pub fn new(path_expected: &Path) -> Self {
        Self { expected: expect_file![path_expected], records: BTreeMap::new() }
    }

    /// Records one test under its paths, which must be unique and line-safe.
    pub fn record(&mut self, paths: &[&Path], status: &str, details: &str) -> Result<()> {
        let mut text = status.to_owned();
        for path in paths {
            let Some(text_path) = path
                .to_str()
                .filter(|path| !path.contains(['\t', '\r', '\n']))
            else {
                return Err(Error::from(format!("invalid result path: {}", path.display())));
            };
            text.push('\t');
            text.push_str(text_path);
        }
        text.push('\n');
        text.push_str(details);
        let key = paths.iter().map(|path| path.to_path_buf()).collect();
        if self.records.insert(key, text).is_some() {
            return Err(Error::from(format!("duplicate result: {}", paths[0].display())));
        }
        Ok(())
    }

    pub fn check(self) {
        self.expected
            .assert_eq(&self.records.into_values().collect::<String>());
    }
}
