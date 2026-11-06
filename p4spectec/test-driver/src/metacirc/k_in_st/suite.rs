//! The registration of K-in-ST suites, and the definitions and programs they name
//!
//! A suite takes its definitions from `root`, a directory of cases written for
//! spec-k (a directory with one `<name>.k` and its programs, `<program>.<ext>`),
//! or `def` in `dir` of the checkout named by the environment variable `src`,
//! with `programs` and their expectations in `expected`. A program's standard
//! input is `<program>.in`, and its expectations `<name>.out`, `<name>.search`,
//! and `<name>.depth`.

use std::{
    fs,
    path::{Path, PathBuf},
};

use serde::Deserialize;

use super::{build::Case, dir_build};
use crate::{
    harness::{Error, Result},
    metacirc::config::{ByBase, Configuration},
};

/// Supplies configurations and suites of K-in-ST runs.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Registration {
    pub configs: Vec<Configuration>,
    pub suites: Vec<KInStSuite>,
}

/// Registers definitions and their programs, run through one tower per base:
/// the cases under `root`, or the definition `def` in `dir` of the checkout `src`.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct KInStSuite {
    pub name: String,
    pub towers: ByBase<PathBuf>,
    /// The directory of cases written for spec-k.
    pub root: Option<PathBuf>,
    /// The extension of the programs of its cases.
    #[serde(default = "ext_default")]
    pub ext: String,
    /// The environment variable that gives the checkout of the definition.
    pub src: Option<String>,
    /// The directory of the definition in the checkout.
    pub dir: Option<PathBuf>,
    /// The definition, in `dir`.
    pub def: Option<PathBuf>,
    /// The programs, in `dir` (or in the checkout, from `{src}`).
    #[serde(default)]
    pub programs: Vec<PathBuf>,
    /// The directory of the programs' `<program>.in`, in `dir`, if not beside them.
    pub inputs: Option<PathBuf>,
    /// The directory of what krun gives for the programs.
    pub expected: Option<PathBuf>,
    /// Extra options of kompile.
    #[serde(default)]
    pub kompile: Vec<String>,
    /// Extra options of krun.
    #[serde(default)]
    pub krun: Vec<String>,
    /// Extra clang flags of the K library, as for a plugin.
    #[serde(default)]
    pub runtime: Vec<String>,
    /// The step bound of every run.
    pub depth: Option<u64>,
    /// A command the programs pass through before krun takes them.
    #[serde(default)]
    pub prepare: Vec<String>,
    /// Whether the definition is KORE already, and the programs initial terms.
    #[serde(default)]
    pub kore: bool,
    /// A command that makes the initial term of a program, in place of krun.
    #[serde(default)]
    pub init: Vec<String>,
}

/// The extension of programs when a suite names none.
fn ext_default() -> String {
    "prog".to_owned()
}

/// A program of a definition.
pub struct Program {
    /// The program, as krun takes it.
    pub path: PathBuf,
    /// Its name, which names its initial term and its expectations.
    pub name: String,
    /// Its standard input, if any.
    pub input: Option<PathBuf>,
    /// What krun gives for it, without the extension (`.out`, `.search`, `.depth`).
    pub expected: PathBuf,
}

impl Program {
    /// A program, with its input beside it or in `dir_inputs`, and its expectations
    /// in `dir_expected`; `path` may end in `#<name>`, which names it.
    fn new(path: PathBuf, dir_inputs: Option<&Path>, dir_expected: &Path) -> Self {
        let text = path.to_string_lossy().into_owned();
        let (path, name) = match text.rsplit_once('#') {
            Some((path, name)) => (PathBuf::from(path), name.to_owned()),
            None => {
                let stem = path
                    .file_stem()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .into_owned();
                (path, stem)
            }
        };
        let file = path
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .into_owned();
        let input = dir_inputs
            .unwrap_or_else(|| path.parent().unwrap_or(Path::new(".")))
            .join(format!("{file}.in"));
        let input = input.is_file().then_some(input);
        let expected = dir_expected.join(&name);
        Self { path, name, input, expected }
    }

    /// The expectation of the program with extension `ext`.
    pub fn expected(&self, ext: &str) -> PathBuf {
        PathBuf::from(format!("{}.{ext}", self.expected.display()))
    }

    /// The step bound of the program's runs: the suite's, or `<name>.depth`.
    pub fn depth(&self, suite: &KInStSuite) -> Result<Option<u64>> {
        let path = self.expected("depth");
        if suite.depth.is_some() || !path.exists() {
            return Ok(suite.depth);
        }
        let text = fs::read_to_string(&path)?;
        let depth = text
            .trim()
            .parse()
            .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
        Ok(Some(depth))
    }
}

/// A definition, built, with its programs.
pub type Group<'a> = (Case<'a>, Vec<Program>);

/// The definitions of a suite, built, with their programs;
/// none if the checkout they are in is not given.
pub fn groups(suite: &KInStSuite) -> Result<Option<Vec<Group<'_>>>> {
    if let Some(root) = &suite.root {
        return cases(root)?
            .into_iter()
            .map(|dir_case| group_case(suite, &dir_case))
            .collect::<Result<_>>()
            .map(Some);
    }
    let missing =
        |field: &str| Error::from(format!("{}: a suite needs `root` or `{field}`", suite.name));
    let src = suite.src.as_deref().ok_or_else(|| missing("src"))?;
    let Some(checkout) = std::env::var_os(src) else {
        eprintln!("metacirc k-in-st {}: skipped, {src} is not set", suite.name);
        return Ok(None);
    };
    let checkout = checkout.to_string_lossy();
    let dir = PathBuf::from(&*checkout).join(suite.dir.as_deref().ok_or_else(|| missing("dir"))?);
    let source = dir.join(suite.def.as_deref().ok_or_else(|| missing("def"))?);
    let dir_expected = suite
        .expected
        .as_deref()
        .ok_or_else(|| missing("expected"))?;
    let dir_inputs = suite.inputs.as_ref().map(|inputs| dir.join(inputs));
    let programs = suite
        .programs
        .iter()
        .map(|path| {
            // A path may start at the checkout, `{src}`, rather than in `dir`
            let path = PathBuf::from(path.to_string_lossy().replace("{src}", &checkout));
            Program::new(dir.join(path), dir_inputs.as_deref(), dir_expected)
        })
        .collect();
    let case = Case::build(&source, suite, &checkout, dir_build().join(&suite.name))?;
    Ok(Some(vec![(case, programs)]))
}

/// The case in `dir_case`, its one `.k` file built, with its programs in order.
fn group_case<'a>(suite: &'a KInStSuite, dir_case: &Path) -> Result<Group<'a>> {
    let entries = files(dir_case)?;
    let mut sources = entries.iter().filter(|path| has_extension(path, "k"));
    let (Some(source), None) = (sources.next(), sources.next()) else {
        return Err(Error::from(format!("{}: a case has one .k file", dir_case.display())));
    };
    let case = Case::build(source, suite, "", dir_build().join(dir_case))?;
    let mut programs: Vec<_> = entries
        .iter()
        .filter(|path| has_extension(path, &suite.ext))
        .map(|path| Program::new(path.clone(), None, dir_case))
        .collect();
    programs.sort_by(|a, b| a.path.cmp(&b.path));
    Ok((case, programs))
}

/// The cases under `root`: the directories that hold a `.k` file, in order.
fn cases(root: &Path) -> Result<Vec<PathBuf>> {
    let mut cases = Vec::new();
    let mut entries = files(root)?;
    entries.sort();
    if entries.iter().any(|path| has_extension(path, "k")) {
        cases.push(root.to_owned());
    }
    for entry in entries {
        if entry.is_dir() {
            cases.extend(self::cases(&entry)?);
        }
    }
    Ok(cases)
}

/// The entries of a directory.
fn files(dir: &Path) -> Result<Vec<PathBuf>> {
    Ok(fs::read_dir(dir)?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::io::Result<Vec<_>>>()?)
}

/// Whether `path` is a file with extension `ext`.
fn has_extension(path: &Path, ext: &str) -> bool {
    path.is_file() && path.extension().is_some_and(|extension| extension == ext)
}
