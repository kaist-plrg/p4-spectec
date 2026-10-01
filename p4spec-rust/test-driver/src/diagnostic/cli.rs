//! Stage diagnostics from registered CLI argument files
//!
//! Each line of the input file is one exact product argument.
//! Paths are relative to the diagnostic fixture working directory.
//! Expectations contain the product's stderr verbatim.

use std::{path::Path, process::Command};

use crate::Result;

use super::{Case, failure};

/// Executes registered arguments and returns the product's stderr.
pub fn run(path_cli: &Path, case: &Case) -> Result<String> {
    // Read the complete invocation from the argument file
    let text = std::fs::read_to_string(case.path_input())?;
    // Capture stderr without comparing exit status or program stdout
    let output = Command::new(path_cli).args(text.lines()).output()?;
    let text = String::from_utf8(output.stderr).map_err(|error| failure(&case.name, error))?;
    // Require the intended diagnostic before comparing or promoting stderr
    let code = case
        .code
        .as_deref()
        .ok_or_else(|| failure(&case.name, "CLI input requires an expected diagnostic code"))?;
    if !text.contains(&format!("error[{code}]:")) {
        return Err(failure(&case.name, format!("missing expected diagnostic {code}")));
    }
    Ok(text)
}
