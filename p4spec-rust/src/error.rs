//! Diagnostics for command-owned argument checks
//!
//! Command-owned checks construct reports before reading specification files.
//! The CLI renders reports and selects exit codes at its output boundary.

use p4spec_rust::diagnostic::{Diagnostic, Report, Severity};

/// Constructs a command diagnostic without a specification source location.
fn command(code: &str, message: impl Into<String>) -> Box<Report> {
    Box::new(
        Diagnostic::new(
            "command",
            Severity::Error,
            Some(code.to_owned()),
            message,
            Vec::new(),
            Vec::new(),
        )
        .into(),
    )
}

// == Splice admission

const SPLICE_OUTPUT_CONFLICT: &str = "command/splice-output-conflict";

/// Reports conflicting in-place and explicit output destinations.
pub(crate) fn splice_output_conflict() -> Box<Report> {
    command(SPLICE_OUTPUT_CONFLICT, "options `--inplace` and `--out` cannot be used together")
}

const SPLICE_INPUT_REQUIRED: &str = "command/splice-input-required";

/// Reports an empty skeleton input list in either output mode.
pub(crate) fn splice_input_required() -> Box<Report> {
    command(SPLICE_INPUT_REQUIRED, "splice requires at least one input file")
}

const SPLICE_FILE_COUNT_MISMATCH: &str = "command/splice-file-count-mismatch";

/// Reports both counts when explicit output paths cannot pair with inputs.
pub(crate) fn splice_file_count_mismatch(num_input: usize, num_output: usize) -> Box<Report> {
    // Match each count's singular or plural noun
    let text_input = if num_input == 1 { "file" } else { "files" };
    let text_output = if num_output == 1 { "file" } else { "files" };
    // Preserve both counts in the source-independent summary
    command(
        SPLICE_FILE_COUNT_MISMATCH,
        format!(
            "splice expects equal numbers of input and output files, but got \
             {num_input} input {text_input} and {num_output} output {text_output}"
        ),
    )
}
