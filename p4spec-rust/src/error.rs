//! Command failures and splice admission diagnostics
//!
//! Command-owned checks construct reports before reading specification files.
//! Other variants retain the errors supplied by their owning operations.
//! The CLI renders reports and selects exit codes at its output boundary.

use p4spec_rust::{
    backend_specdoc::splicer,
    diagnostic::{Diagnostic, Report, Severity},
    interface::p4::error::P4Error,
    interp::shared::error::Error as InterpError,
    runner, sim_plugin,
};

// == Errors

/// A command failure with its user-facing diagnostic category.
#[derive(Debug, thiserror::Error)]
pub(crate) enum CliError {
    /// Command admission failed before processing inputs.
    #[error(transparent)]
    Command(Box<Report>),
    /// Document splicing failed.
    #[error(transparent)]
    Splice(#[from] splicer::Error),
    /// Specification processing failed.
    #[error(transparent)]
    Spec(#[from] p4spec_rust::Error),
    /// Interpreter construction failed.
    #[error(transparent)]
    Runner(#[from] runner::BuildError),
    /// Simulator construction failed.
    #[error(transparent)]
    Simulator(#[from] sim_plugin::BuildError),
    /// Simulation failed.
    #[error(transparent)]
    Simulation(#[from] sim_plugin::runner::Error),
    /// Parsing the input program failed.
    #[error("syntax error: {0}")]
    Syntax(#[from] P4Error),
    /// Evaluating the input program failed.
    #[error("runtime error: {0}")]
    Runtime(#[from] InterpError),
}

/// Constructs a command diagnostic without a specification source location.
fn command(code: &str, message: impl Into<String>) -> CliError {
    CliError::Command(Box::new(
        Diagnostic::new(
            "command",
            Severity::Error,
            Some(code.to_owned()),
            message,
            Vec::new(),
            Vec::new(),
        )
        .into(),
    ))
}

// == Splice admission

const SPLICE_OUTPUT_CONFLICT: &str = "command/splice-output-conflict";

/// Reports conflicting in-place and explicit output destinations.
pub(crate) fn splice_output_conflict() -> CliError {
    command(SPLICE_OUTPUT_CONFLICT, "options `--inplace` and `--out` cannot be used together")
}

const SPLICE_INPUT_REQUIRED: &str = "command/splice-input-required";

/// Reports an empty skeleton input list in either output mode.
pub(crate) fn splice_input_required() -> CliError {
    command(SPLICE_INPUT_REQUIRED, "splice requires at least one input file")
}

const SPLICE_FILE_COUNT_MISMATCH: &str = "command/splice-file-count-mismatch";

/// Reports both counts when explicit output paths cannot pair with inputs.
pub(crate) fn splice_file_count_mismatch(num_input: usize, num_output: usize) -> CliError {
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
