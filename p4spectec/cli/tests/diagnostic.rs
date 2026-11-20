//! Diagnostics of the product's command line, one test per command
//!
//! Each `.args` file under `testdata/spec/cli/<command>` holds one argument per line.
//! A test runs the product with them from `testdata/spec`,
//! requires it to fail (or, where written, to pass),
//! and compares its stderr with the expectation beside the argument file:
//! `<stem>.expect`, or, when absent, the one the languages share,
//! named by the stem without its `al`, `sl`, or `pl` segment.
//! `UPDATE_EXPECT=1` rewrites the expectation instead.

use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};

use expect_test::expect_file;

/// Returns `testdata/spec`, the directory the product runs from.
fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../testdata/spec")
}

/// Returns the argument files of `dir`, relative to `testdata/spec`, sorted.
fn args_files(dir: &str) -> Vec<PathBuf> {
    let mut paths: Vec<PathBuf> = fs::read_dir(root().join(dir))
        .expect("command directory")
        .map(|entry| entry.expect("directory entry").path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "args"))
        .map(|path| Path::new(dir).join(path.file_name().unwrap()))
        .collect();
    assert!(!paths.is_empty(), "no argument files in {dir}");
    paths.sort();
    paths
}

/// Returns the expectation of the argument file at `path`.
fn expected(path: &Path) -> PathBuf {
    let path_own = path.with_extension("expect");
    if root().join(&path_own).exists() {
        return path_own;
    }
    // Fall back to the expectation the languages share
    let stem = path.file_stem().unwrap().to_string_lossy();
    let segments: Vec<_> = stem
        .split('-')
        .filter(|segment| !["al", "sl", "pl"].contains(segment))
        .collect();
    path.with_file_name(format!("{}.expect", segments.join("-")))
}

/// Runs the product with the arguments of `path` from `dir`
/// and compares its stderr with `path_expected`.
fn check(path: &Path, dir: &Path, success: bool, path_expected: &Path) {
    let text = fs::read_to_string(root().join(path)).expect("argument file");
    let output = Command::new(env!("CARGO_BIN_EXE_p4spectec"))
        .args(text.lines())
        .current_dir(dir)
        .output()
        .expect("run the product");
    assert_eq!(output.status.success(), success, "{}: exit status", path.display());
    let stderr = String::from_utf8(output.stderr).expect("UTF-8 stderr");
    expect_file![root().join(path_expected)].assert_eq(&stderr);
}

/// Runs every argument file of `dir`, each of which must fail.
fn check_failures(dir: &str) {
    for path in args_files(dir) {
        check(&path, &root(), false, &expected(&path));
    }
}

#[test]
fn command() {
    check_failures("cli/command");
}

#[test]
fn run() {
    check_failures("cli/run");
}

#[test]
fn sim() {
    check_failures("cli/sim");
}

#[test]
fn splice() {
    // Both splice warning-original.adoc onto itself, warn, and pass;
    // they run on copies so the fixture stays unchanged
    for path in args_files("cli/splice") {
        let dir = tempfile::tempdir().expect("temporary directory");
        fs::create_dir_all(dir.path().join("cli/splice")).unwrap();
        for name in ["minimal.watsup", "warning-original.adoc"] {
            let path_input = Path::new("cli/splice").join(name);
            fs::copy(root().join(&path_input), dir.path().join(&path_input)).unwrap();
        }
        check(&path, dir.path(), true, Path::new("cli/splice/warning-original.expect"));
    }
}
