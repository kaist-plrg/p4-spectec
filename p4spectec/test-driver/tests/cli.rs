//! Parsing and validation of the product's command line
//!
//! A `Fixtures` names a directory under `tests/p4spectec`
//! whose `.args` files hold one argument per line,
//! each a command line the product must reject before running any command.
//! Its test runs each in the test process from `tests/p4spectec`
//! and compares its stderr with the snapshot `<stem>.snap` beside the argument file.
//! `INSTA_UPDATE=always` rewrites the snapshot instead.

use std::{
    ffi::OsString,
    fs,
    io::{self, Read, Seek},
    os::fd::AsFd,
    path::{Path, PathBuf},
    process::ExitCode,
};

// = Helpers

/// Returns `tests/p4spectec`, the directory the product runs from.
fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../tests/p4spectec")
}

/// Returns the argument files of `dir`, relative to `tests/p4spectec`, sorted.
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

/// Runs the command line with the arguments of `path` from `tests/p4spectec`,
/// requires it to fail, and compares its stderr with the snapshot beside `path`.
fn check(path: &Path) {
    let text = fs::read_to_string(root().join(path)).expect("argument file");
    let args = ["p4spectec"]
        .into_iter()
        .chain(text.lines())
        .map(OsString::from);
    std::env::set_current_dir(root()).expect("working directory");
    // Send stderr to a file while the command runs
    // ponytail: a panic in the command leaves stderr in the file, restore by a guard if that bites
    let mut file_stderr = tempfile::tempfile().expect("stderr file");
    let fd_stderr = io::stderr()
        .as_fd()
        .try_clone_to_owned()
        .expect("stderr descriptor");
    rustix::stdio::dup2_stderr(&file_stderr).expect("redirect stderr");
    let code = p4spectec_cli::main(args);
    rustix::stdio::dup2_stderr(&fd_stderr).expect("restore stderr");
    assert!(code != ExitCode::SUCCESS, "{}: command line accepted", path.display());
    let mut stderr = String::new();
    file_stderr.rewind().expect("stderr file");
    file_stderr
        .read_to_string(&mut stderr)
        .expect("UTF-8 stderr");
    let path_expected = path.with_extension("snap");
    let name = path_expected.file_stem().unwrap().to_str().unwrap();
    insta::with_settings!({
        snapshot_path => root().join(path_expected.parent().unwrap()),
        prepend_module_to_snapshot => false,
        omit_expression => true,
    }, {
        insta::assert_snapshot!(name, stderr);
    });
}

// = Suites

/// One directory of rejected command lines under `tests/p4spectec/`.
struct Fixtures {
    /// Holds the `.args` files, relative to `tests/p4spectec`.
    dir: &'static str,
}

impl Fixtures {
    /// Runs every command line of the suite, each of which must be rejected.
    fn check(&self) {
        for path in args_files(self.dir) {
            check(&path);
        }
    }
}

// = Tests

const COMMAND: Fixtures = Fixtures { dir: "cli/command" };

#[test]
fn command() {
    COMMAND.check();
}
