//! Stage diagnostics from registered CLI argument files
//!
//! Each line of the input file is one exact product argument.
//! Registered input files are copied into a temporary working directory.
//! Cases without copied inputs use the diagnostic fixture working directory.
//! Expectations contain the product's stderr verbatim.

use std::{
    fs,
    path::{Component, Path},
    process::Command,
};

use crate::Result;

use super::{Case, failure};

/// Executes registered arguments and returns the product's stderr.
pub fn run(path_cli: &Path, case: &Case) -> Result<String> {
    // Read the complete invocation from the argument file
    let text = std::fs::read_to_string(case.path_input())?;
    let mut command = Command::new(path_cli);
    command.args(text.lines());
    // Isolate registered inputs that the command may replace
    let directory = if case.inputs.is_empty() { None } else { Some(tempfile::tempdir()?) };
    if let Some(directory) = &directory {
        for path in case.paths_input() {
            if path
                .components()
                .any(|part| !matches!(part, Component::Normal(_) | Component::CurDir))
            {
                return Err(failure(
                    &case.name,
                    "isolated CLI inputs must stay inside the fixture directory",
                ));
            }
            let path_output = directory.path().join(&path);
            fs::create_dir_all(path_output.parent().expect("fixture has a parent"))?;
            fs::copy(path, path_output)?;
        }
        command.current_dir(directory.path());
    }
    // Check the command result independently of diagnostic severity
    let output = command.output()?;
    case.check_outcome(output.status.success())?;
    let text = String::from_utf8(output.stderr).map_err(|error| failure(&case.name, error))?;
    // Require the intended diagnostic before comparing or promoting stderr
    let code = case
        .code
        .as_deref()
        .ok_or_else(|| failure(&case.name, "CLI input requires an expected diagnostic code"))?;
    if !text.contains(&format!("[{code}]:")) {
        return Err(failure(&case.name, format!("missing expected diagnostic {code}")));
    }
    Ok(text)
}
