//! Invocation of the C preprocessor for P4 file inputs
//!
//! Include directories and the source path are passed to `cc -E`;
//! stdout becomes lexer input and a nonzero status becomes a located P4 error.
//! A file containing `#include <core.p4>` is therefore expanded
//! before tokenization.

use std::{
    path::{Path, PathBuf},
    process::Command,
};

use crate::lang::common::source::{FileId, Position, Span};

use super::error::{P4Error, P4ErrorKind};

/// A span naming the file, for errors with no position.
fn span_file(path: &Path) -> Span {
    let position = Position::new(FileId::intern(&path.to_string_lossy()), 0, 0);
    Span::new(position, position)
}

/// Runs `cc -E` on the file and returns its output.
pub fn preprocess(includes: &[PathBuf], path: impl AsRef<Path>) -> Result<String, P4Error> {
    let path = path.as_ref();
    let mut command = Command::new("cc");
    for include in includes {
        command.arg(format!("-I{}", include.display()));
    }
    // Treat the file as C with no predefined macros or system headers
    command.args(["-undef", "-nostdinc", "-E", "-x", "c"]);
    command.arg(path);
    let output = command.output();
    // Failing to start the compiler is reported like a failed run
    let output = output.map_err(|error| {
        let kind = P4ErrorKind::Preprocess { status: None, stderr: error.to_string() };
        P4Error::new(span_file(path), kind)
    })?;
    // A nonzero status carries the compiler's diagnostics
    if !output.status.success() {
        let kind = P4ErrorKind::Preprocess {
            status: output.status.code(),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        };
        return Err(P4Error::new(span_file(path), kind));
    }
    // The output must be UTF-8 to be lexed
    let source = String::from_utf8(output.stdout);
    source.map_err(|error| {
        let kind =
            P4ErrorKind::Preprocess { status: output.status.code(), stderr: error.to_string() };
        P4Error::new(span_file(path), kind)
    })
}
