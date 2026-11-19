//! Diagnostics for coverage input collection and report output

use std::{io, path::Path};

use crate::diagnostic::{Diagnostic, Report, Severity};

/// A diagnostic from collecting coverage inputs or preparing a coverage run.
pub type CoverError = Box<Report>;

fn report(code: &str, message: impl Into<String>) -> CoverError {
    Box::new(
        Diagnostic::new(
            "coverage",
            Severity::Error,
            Some(code.to_owned()),
            message,
            vec![],
            vec![],
        )
        .into(),
    )
}

const INPUT_READ_FAILED: &str = "coverage/input-read-failed";

/// Reports an input file or directory that cannot be read.
pub(super) fn input_read_failed(path: &Path, error_io: &io::Error) -> CoverError {
    report(INPUT_READ_FAILED, format!("cannot read {}: {error_io}", path.display()))
}

const ARCHITECTURE_UNSUPPORTED: &str = "coverage/architecture-unsupported";

/// Reports an architecture without a known model include.
pub(super) fn architecture_unsupported(arch: &str) -> CoverError {
    report(ARCHITECTURE_UNSUPPORTED, format!("architecture {arch} is not supported"))
}

const OUTPUT_WRITE_FAILED: &str = "coverage/output-write-failed";

/// Reports a coverage report file that cannot be written.
pub(super) fn output_write_failed(path: &Path, error_io: &io::Error) -> CoverError {
    report(OUTPUT_WRITE_FAILED, format!("cannot write {}: {error_io}", path.display()))
}
