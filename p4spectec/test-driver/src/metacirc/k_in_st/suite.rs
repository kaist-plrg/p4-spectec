//! The registration of K-in-ST suites, and the definitions and programs they name
//!
//! A suite takes its definitions from `root`, a directory of cases written for
//! spec-k (a directory with one `<name>.k` and its programs, `<program>.<ext>`),
//! or `def` in `dir` of the checkout named by the environment variable `src`,
//! with its programs and their expectations in `expected`. A program's standard
//! input is `<program>.in`, and its expectations `<name>.out`, `<name>.search`,
//! and `<name>.depth`.
//!
//! The programs of a checkout are the files `programs` matches, but those `skip`
//! matches and those `skip_lists` name; with `json_tests`, each test of a
//! JSON file is one. A program is named by its path below the patterns' common
//! directory, without extensions, and a test by its file's name and its own.

use std::{
    collections::BTreeSet,
    fs,
    path::{Component, Path, PathBuf},
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
    /// Patterns of the programs, in `dir` (or in the checkout, from `{src}`).
    #[serde(default)]
    pub programs: Vec<String>,
    /// Patterns of programs left out, as `programs`.
    #[serde(default)]
    pub skip: Vec<String>,
    /// The checkout's lists of programs it does not run.
    #[serde(default)]
    pub skip_lists: Vec<SkipList>,
    /// Whether each test of a JSON file is a program, rather than the file.
    #[serde(default)]
    pub json_tests: bool,
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
    /// A command the programs pass through before krun takes them.
    #[serde(default)]
    pub prepare: Vec<String>,
    /// A command that makes the initial term of a program, in place of krun.
    #[serde(default)]
    pub init: Vec<String>,
}

/// The extension of programs when a suite names none.
fn ext_default() -> String {
    "prog".to_owned()
}

/// A list of programs not run: per line a path in `dir`, with `,<test>` for a test
/// of a JSON file (`*` for any).
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SkipList {
    pub list: String,
    pub dir: String,
}

/// A program of a definition.
pub struct Program {
    /// The program, as krun takes it.
    pub path: PathBuf,
    /// The test in it, for a file of several.
    pub test: Option<String>,
    /// Its name, which names its initial term and its expectations.
    pub name: String,
    /// Its standard input, if any.
    pub input: Option<PathBuf>,
    /// What krun gives for it, without the extension (`.out`, `.search`, `.depth`).
    pub expected: PathBuf,
}

impl Program {
    /// A program named `name`, with its input beside it or in `dir_inputs`, and
    /// its expectations in `dir_expected`; a name may hold a directory.
    fn new(
        path: PathBuf,
        test: Option<String>,
        name: String,
        dir_inputs: Option<&Path>,
        dir_expected: &Path,
    ) -> Self {
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
        Self { path, test, name, input, expected }
    }

    /// The expectation of the program with extension `ext`.
    pub fn expected(&self, ext: &str) -> PathBuf {
        PathBuf::from(format!("{}.{ext}", self.expected.display()))
    }

    /// The step bound of the program's runs, `<name>.depth`, if any.
    pub fn depth(&self) -> Result<Option<u64>> {
        let path = self.expected("depth");
        if !path.exists() {
            return Ok(None);
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
    // A pattern may start at the checkout, `{src}`, rather than in `dir`
    let resolve = |pattern: &String| dir.join(pattern.replace("{src}", &checkout));
    let patterns: Vec<PathBuf> = suite.programs.iter().map(resolve).collect();
    // What is left out: a path, and for a test of a JSON file its name
    let mut listed = Vec::new();
    for pattern in suite.skip.iter().map(resolve) {
        listed.extend(glob(&pattern)?.into_iter().map(|path| (path, None)));
    }
    for skip_list in &suite.skip_lists {
        let dir_list = resolve(&skip_list.dir);
        for (path, test) in self::skip_list(&resolve(&skip_list.list))? {
            listed.push((dir_list.join(path), test));
        }
    }
    let base = common_dir(patterns.iter().map(|pattern| fixed_dir(pattern)));
    let mut programs = Vec::new();
    let mut names = BTreeSet::new();
    for pattern in &patterns {
        for path in glob(pattern)? {
            let name = name_of(path.strip_prefix(&base).unwrap_or(&path));
            let tests = if suite.json_tests {
                json_keys(&path)?.into_iter().map(Some).collect()
            } else {
                vec![None]
            };
            for test in tests {
                let left_out = listed.iter().any(|(path_listed, test_listed)| {
                    *path_listed == path
                        && test_listed.as_ref().is_none_or(|listed| {
                            test.as_ref().is_some_and(|test| wildcard(listed, test))
                        })
                });
                if left_out {
                    continue;
                }
                let name = match &test {
                    Some(test) => format!("{name}/{test}"),
                    None => name.clone(),
                };
                if !names.insert(name.clone()) {
                    return Err(Error::from(format!("{}: two programs named {name}", suite.name)));
                }
                let program =
                    Program::new(path.clone(), test, name, dir_inputs.as_deref(), dir_expected);
                programs.push(program);
            }
        }
    }
    let case = Case::build(&source, suite, &checkout, dir_build().join(&suite.name))?;
    Ok(Some(vec![(case, programs)]))
}

/// Whether `text` matches `pattern`, where `*` stands for any text.
fn wildcard(pattern: &str, text: &str) -> bool {
    match pattern.split_once('*') {
        None => pattern == text,
        Some((head, rest)) => {
            let Some(text) = text.strip_prefix(head) else {
                return false;
            };
            (0..=text.len())
                .filter(|&i| text.is_char_boundary(i))
                .any(|i| wildcard(rest, &text[i..]))
        }
    }
}

/// The files a pattern matches, in order; `*` matches within a component.
fn glob(pattern: &Path) -> Result<Vec<PathBuf>> {
    let mut paths = vec![PathBuf::new()];
    for component in pattern.components() {
        let Component::Normal(part) = component else {
            paths.iter_mut().for_each(|path| path.push(component));
            continue;
        };
        let part = part.to_string_lossy();
        if !part.contains('*') {
            paths.iter_mut().for_each(|path| path.push(&*part));
            continue;
        }
        let mut next = Vec::new();
        for path in paths.iter().filter(|path| path.is_dir()) {
            for entry in files(path)? {
                let name = entry
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .into_owned();
                if wildcard(&part, &name) {
                    next.push(entry);
                }
            }
        }
        paths = next;
    }
    let mut paths: Vec<PathBuf> = paths.into_iter().filter(|path| path.is_file()).collect();
    paths.sort();
    Ok(paths)
}

/// The directory of a pattern before its first `*`.
fn fixed_dir(pattern: &Path) -> PathBuf {
    let fixed: PathBuf = pattern
        .components()
        .take_while(|component| !component.as_os_str().to_string_lossy().contains('*'))
        .collect();
    if fixed == pattern { fixed.parent().map(Path::to_path_buf).unwrap_or_default() } else { fixed }
}

/// The longest directory all of `dirs` are in.
fn common_dir(dirs: impl Iterator<Item = PathBuf>) -> PathBuf {
    dirs.reduce(|common, dir| {
        common
            .components()
            .zip(dir.components())
            .take_while(|(a, b)| a == b)
            .map(|(a, _)| a)
            .collect()
    })
    .unwrap_or_default()
}

/// A program's name: its path without the extensions of its file.
fn name_of(path: &Path) -> String {
    let file = path.file_name().unwrap_or_default().to_string_lossy();
    let stem = file.split_once('.').map_or(&*file, |(stem, _)| stem);
    path.with_file_name(stem).to_string_lossy().into_owned()
}

/// The entries of a skip list, without blank lines and `#` comments.
fn skip_list(path: &Path) -> Result<Vec<(PathBuf, Option<String>)>> {
    let text = fs::read_to_string(path)
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    Ok(text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(|line| match line.split_once(',') {
            Some((path, test)) => (PathBuf::from(path), Some(test.to_owned())),
            None => (PathBuf::from(line), None),
        })
        .collect())
}

/// The keys of the JSON object in a file, its tests.
fn json_keys(path: &Path) -> Result<Vec<String>> {
    let text = fs::read_to_string(path)?;
    let value: serde_json::Value = serde_json::from_str(&text)
        .map_err(|error| Error::from(format!("{}: {error}", path.display())))?;
    let object = value
        .as_object()
        .ok_or_else(|| Error::from(format!("{}: not a JSON object", path.display())))?;
    Ok(object.keys().cloned().collect())
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
        .map(|path| {
            let stem = path
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned();
            Program::new(path.clone(), None, stem, None, dir_case)
        })
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
