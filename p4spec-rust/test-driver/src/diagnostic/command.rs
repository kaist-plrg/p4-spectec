//! Command admission exercised through the product executable
//!
//! Each case runs in an empty directory with an absent specification.
//! The expected command diagnostic must precede any source or document access.
//! The caller compares complete stderr after checking stdout and exit status.

use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
};

use super::failure;
use crate::Result;

/// Owns one subprocess directory without changing the driver's working directory.
struct Directory(PathBuf);

impl Directory {
    /// Creates an isolated directory for a registered command case.
    fn new(name: &str) -> Result<Self> {
        let path = env::temp_dir().join(format!("p4spec-command-{}-{name}", std::process::id()));
        fs::create_dir(&path)?;
        Ok(Self(path))
    }
}

impl Drop for Directory {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

/// Executes a negative command through argument parsing and final rendering.
pub fn run(path_cli: &Path, name: &str) -> Result<String> {
    let (args, code) = match name {
        "command-splice-file-count-mismatch" => (
            vec!["--splice", "a.adoc", "--splice", "b.adoc", "--out", "out.adoc"],
            "command/splice-file-count-mismatch",
        ),
        "command-splice-output-conflict" => (
            vec!["--inplace", "--splice", "a.adoc", "--out", "out.adoc"],
            "command/splice-output-conflict",
        ),
        "command-splice-input-required" => (vec![], "command/splice-input-required"),
        _ => return Err(failure(name, "unknown command case")),
    };
    // Run the supplied product binary before any specification is available
    let directory = Directory::new(name)?;
    let output = Command::new(path_cli)
        .current_dir(&directory.0)
        .args(["splice", "missing.watsup"])
        .args(args)
        .output()
        .map_err(|error| {
            failure(name, format!("cannot execute {}: {error}", path_cli.display()))
        })?;
    // Reject success, argument-parser errors, and accidental document output
    if output.status.code() != Some(1) || !output.stdout.is_empty() {
        return Err(failure(
            name,
            format!(
                "expected exit 1 and empty stdout, got {} and {:?}; stderr: {}",
                output.status,
                output.stdout,
                String::from_utf8_lossy(&output.stderr),
            ),
        ));
    }
    // Admission must leave the empty directory untouched
    if fs::read_dir(&directory.0)?.next().is_some() {
        return Err(failure(name, "command admission created files"));
    }
    // Reject unrelated failures even when deliberately promoting snapshots
    let text = String::from_utf8(output.stderr).map_err(|error| failure(name, error))?;
    if !text.starts_with(&format!("error[{code}]: ")) {
        return Err(failure(name, format!("expected {code}, got {text}")));
    }
    Ok(text)
}
