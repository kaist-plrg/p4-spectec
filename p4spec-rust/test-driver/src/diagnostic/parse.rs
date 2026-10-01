//! Real SpecTec parser inputs for diagnostic acceptance
//!
//! File fixtures and constructed byte inputs match the pinned OCaml harness.
//! Filesystem cases run in temporary directories with stable relative identities.
//! Constructed byte inputs retain the reference harness's source-free locations.

use std::{env, fs, path::PathBuf, rc::Rc};

#[cfg(unix)]
use std::{io, os::unix::fs::PermissionsExt};

use p4spec_rust::diagnostic::Report;

use p4spec_rust::frontend::parse::{parse_files, parse_mixop, parse_utf8_bytes};

use crate::Result;

use super::failure;

// = Helpers

/// Requires the parser to reject a negative input.
fn rejected<T>(name: &str, result: std::result::Result<T, Box<Report>>) -> Result<Box<Report>> {
    match result {
        Ok(_) => Err(failure(name, "parser unexpectedly accepted negative input")),
        Err(report) => Ok(report),
    }
}

// = Temporary directories

/// Removes temporary resources and restores the driver's fixture directory.
struct Directory {
    path: PathBuf,
    path_previous: PathBuf,
}

impl Directory {
    /// Enters an isolated directory without placing generated files in the checkout.
    fn enter(name: &str) -> Result<Self> {
        // Exclusive creation rejects stale resources rather than reusing them
        let path_previous = env::current_dir()?;
        let path = env::temp_dir().join(format!("p4spec-diagnostic-{}-{name}", std::process::id()));
        fs::create_dir(&path)?;
        let directory = Self { path, path_previous };
        env::set_current_dir(&directory.path)?;
        Ok(directory)
    }
}

impl Drop for Directory {
    fn drop(&mut self) {
        // Restore permissions even when parsing or validation fails
        #[cfg(unix)]
        let _ = fs::set_permissions(
            self.path.join("unreadable-dir"),
            fs::Permissions::from_mode(0o700),
        );
        let _ = env::set_current_dir(&self.path_previous);
        let _ = fs::remove_dir_all(&self.path);
    }
}

// = Filesystem cases

/// Rejects a directory only after proving that the process cannot read it.
#[cfg(unix)]
fn unreadable_directory(name: &str) -> Result<Box<Report>> {
    // Build the same permission failure as the pinned reference harness
    let _directory = Directory::enter(name)?;
    let path = "unreadable-dir";
    fs::create_dir(path)?;
    fs::set_permissions(path, fs::Permissions::from_mode(0o000))?;

    // Privileged execution must not count accidental success as a negative pass
    match fs::read_dir(path) {
        // Check the effective failure, not just the permission bits
        Err(error) if error.kind() == io::ErrorKind::PermissionDenied => {}
        // Preserve unexpected filesystem failures as setup errors
        Err(error) => return Err(failure(name, format!("permission setup failed: {error}"))),
        // Require an unprivileged process for this reference case
        Ok(_) => return Err(failure(name, "directory remains readable with mode 000")),
    }
    rejected(name, parse_files([path]))
}

#[cfg(not(unix))]
fn unreadable_directory(name: &str) -> Result<Box<Report>> {
    Err(failure(name, "directory permission case requires Unix permissions"))
}

// = Entry point

/// Runs the public parser and requires a diagnostic for each negative input.
pub fn run(name: &str) -> Result<Box<Report>> {
    // File fixtures use the same relative identity in parsing and rendering
    if name.ends_with(".watsup") {
        return rejected(name, parse_files([format!("parse/{name}")]));
    }

    // Construct the seven inputs that the reference creates without fixture files
    let bytes: &[u8] = match name {
        // Missing files retain a file-only span
        "parse-io-error" => {
            let _directory = Directory::enter(name)?;
            return rejected(name, parse_files(["missing-input.watsup"]));
        }
        // Directory traversal fails before file parsing starts
        "parse-directory-io-error" => return unreadable_directory(name),
        // Mixfix parsing locates errors within its virtual source
        "parse-malformed-mixop" => return rejected(name, parse_mixop("")),
        // OCaml decimal escape 011 denotes the vertical-tab byte
        "parse-illegal-control-in-text-literal" => b"\"\x0b",
        // Invalid UTF-8 is passed without replacement characters
        "parse-malformed-utf8" => b"\x80",
        // The same invalid byte reaches the block-comment lexer
        "parse-malformed-utf8-in-comment" => b"(;\x80;)",
        // A control byte outside a literal is a distinct lexical context
        "parse-misplaced-control-char" => b"\x0b",
        // Every listed case must have a real input
        _ => return Err(failure(name, "unknown constructed parser case")),
    };

    rejected(name, parse_utf8_bytes(Rc::from("<string>"), bytes))
}
