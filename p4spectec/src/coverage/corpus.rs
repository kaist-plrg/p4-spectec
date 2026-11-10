//! Collection of programs and STF tests for coverage runs
//!
//! Each input path is a file, taken as given, or a directory searched recursively
//! in name order, skipping `include` directories.
//! Exclusion lists and P4/STF pairing follow the test driver:
//! a P4 file pairs with each STF file at the same relative directory and stem,
//! or under a directory named after the P4 file's stem;
//! a patch file replaces an input with the same stem.

use std::{
    collections::{BTreeSet, HashSet},
    fs,
    path::{Path, PathBuf},
};

use super::error::{self, CoverError};

/// Collected inputs, with the counts of what was left out.
pub struct Collected<T> {
    /// The inputs to run, in collection order.
    pub items: Vec<T>,
    /// The number of inputs named in an exclusion list.
    pub excluded: usize,
    /// Explicitly given P4 files dropped for another architecture.
    pub filtered: Vec<PathBuf>,
}

/// An input file: its root directory, its path under the root, and how it was given.
struct Input {
    dir: PathBuf,
    path: PathBuf,
    explicit: bool,
}

impl Input {
    fn full(&self) -> PathBuf {
        self.dir.join(&self.path)
    }
}

// == File collection

/// Collects files with `suffix` under `dir`, recursively and in name order.
fn collect_dir(dir: &Path, suffix: &str) -> Result<Vec<PathBuf>, CoverError> {
    let mut paths = fs::read_dir(dir)
        .and_then(|entries| {
            entries
                .map(|entry| entry.map(|entry| entry.path()))
                .collect::<std::io::Result<Vec<_>>>()
        })
        .map_err(|error_io| error::input_read_failed(dir, &error_io))?;
    paths.sort();
    let mut files = Vec::new();
    for path in paths {
        let metadata =
            fs::metadata(&path).map_err(|error_io| error::input_read_failed(&path, &error_io))?;
        if metadata.is_dir() {
            if path.file_name().is_some_and(|name| name != "include") {
                files.extend(collect_dir(&path, suffix)?);
            }
        } else if path.to_str().is_some_and(|path| path.ends_with(suffix)) {
            files.push(path);
        }
    }
    Ok(files)
}

/// Collects files and directory contents in order, keeping the first of duplicates.
fn collect_inputs(paths: &[PathBuf], suffix: &str) -> Result<Vec<Input>, CoverError> {
    let mut inputs = Vec::new();
    let mut seen = HashSet::new();
    for path in paths {
        let metadata =
            fs::metadata(path).map_err(|error_io| error::input_read_failed(path, &error_io))?;
        let found = if metadata.is_dir() {
            collect_dir(path, suffix)?
                .into_iter()
                .map(|file| {
                    let relative = file
                        .strip_prefix(path)
                        .expect("collected under its directory");
                    Input { dir: path.clone(), path: relative.to_owned(), explicit: false }
                })
                .collect()
        } else {
            let dir = path.parent().unwrap_or(Path::new("")).to_owned();
            let file = PathBuf::from(path.file_name().unwrap_or(path.as_os_str()));
            vec![Input { dir, path: file, explicit: true }]
        };
        inputs.extend(found.into_iter().filter(|input| seen.insert(input.full())));
    }
    Ok(inputs)
}

/// Reads the entries of every `.exclude` file under the directories.
///
/// Lines starting with `#` are comments; other lines name excluded paths verbatim.
pub fn collect_excludes(dirs: &[PathBuf]) -> Result<BTreeSet<String>, CoverError> {
    let mut excludes = BTreeSet::new();
    for dir in dirs {
        for path in collect_dir(dir, ".exclude")? {
            let text = fs::read_to_string(&path)
                .map_err(|error_io| error::input_read_failed(&path, &error_io))?;
            // Split on LF only; preserve whitespace and CR in exclusion entries
            excludes.extend(
                text.split_terminator('\n')
                    .filter(|line| !line.starts_with('#'))
                    .map(str::to_owned),
            );
        }
    }
    Ok(excludes)
}

fn is_excluded(excludes: &BTreeSet<String>, path: &Path) -> bool {
    path.to_str().is_some_and(|path| excludes.contains(path))
}

// == Programs

/// Collects programs with the file name suffix, leaving out excluded ones.
pub fn collect_programs(
    paths: &[PathBuf],
    suffix: &str,
    excludes: &BTreeSet<String>,
) -> Result<Collected<PathBuf>, CoverError> {
    let mut items = Vec::new();
    let mut excluded = 0;
    for input in collect_inputs(paths, suffix)? {
        let path = input.full();
        if is_excluded(excludes, &path) {
            excluded += 1;
        } else {
            items.push(path);
        }
    }
    Ok(Collected { items, excluded, filtered: Vec::new() })
}

// == P4/STF pairs

/// The model include that marks a P4 program for the architecture.
fn arch_include(arch: &str) -> Result<&'static str, CoverError> {
    match arch {
        "v1model" => Ok("v1model.p4"),
        "ebpf" => Ok("ebpf_model.p4"),
        "psa" => Ok("bmv2/psa.p4"),
        _ => Err(error::architecture_unsupported(arch)),
    }
}

/// Replaces each input with the patch of the same stem.
fn patch(inputs: &mut [Input], patches: &[Input]) {
    for input in inputs {
        if let Some(patch) = patches
            .iter()
            .find(|patch| patch.path.file_stem() == input.path.file_stem())
        {
            input.dir.clone_from(&patch.dir);
            input.path.clone_from(&patch.path);
        }
    }
}

/// Collects P4/STF pairs for the architecture, leaving out excluded pairs.
pub fn collect_pairs(
    arch: &str,
    paths_p4: &[PathBuf],
    paths_stf: &[PathBuf],
    dirs_patch: &[PathBuf],
    excludes: &BTreeSet<String>,
) -> Result<Collected<(PathBuf, PathBuf)>, CoverError> {
    let include = arch_include(arch)?;
    // Keep programs that include the architecture model
    let mut inputs_p4 = Vec::new();
    let mut filtered = Vec::new();
    for input in collect_inputs(paths_p4, ".p4")? {
        let path = input.full();
        let text = fs::read_to_string(&path)
            .map_err(|error_io| error::input_read_failed(&path, &error_io))?;
        if text.contains(&format!("#include <{include}>"))
            || text.contains(&format!("#include \"{include}\""))
        {
            inputs_p4.push(input);
        } else if input.explicit {
            filtered.push(path);
        }
    }
    let mut inputs_stf = collect_inputs(paths_stf, ".stf")?;
    for dir in dirs_patch {
        let dir = std::slice::from_ref(dir);
        patch(&mut inputs_p4, &collect_inputs(dir, ".p4")?);
        patch(&mut inputs_stf, &collect_inputs(dir, ".stf")?);
    }
    // Pair by stem, either side by side or under a directory named after the program
    let mut items = Vec::new();
    let mut excluded = 0;
    for input_p4 in &inputs_p4 {
        let stem_p4 = input_p4.path.file_stem();
        for input_stf in &inputs_stf {
            let dir_stf = input_stf.path.parent().unwrap_or(Path::new(""));
            if stem_p4 == Some(dir_stf.as_os_str())
                || (input_p4.path.parent() == input_stf.path.parent()
                    && stem_p4 == input_stf.path.file_stem())
            {
                let (path_p4, path_stf) = (input_p4.full(), input_stf.full());
                if is_excluded(excludes, &path_p4) || is_excluded(excludes, &path_stf) {
                    excluded += 1;
                } else {
                    items.push((path_p4, path_stf));
                }
            }
        }
    }
    Ok(Collected { items, excluded, filtered })
}
