//! CLI rejection from registered argument files
//!
//! Each line of the input file is one exact product argument.
//! Expectations contain the product's stderr verbatim.

use std::{path::Path, process::Command};

use crate::Result;

use super::{Case, failure};

/// Executes registered arguments and returns the product's stderr.
pub fn run(path_cli: &Path, case: &Case) -> Result<String> {
    let text = std::fs::read_to_string(case.path_input())?;
    let paths = case.paths_input();
    let mut used = vec![false; paths.len()];
    let mut args = Vec::new();
    // Resolve exact auxiliary-input placeholders declared in the argument file
    for arg in text.lines() {
        if let Some(text_idx) = arg
            .strip_prefix("{input:")
            .and_then(|arg| arg.strip_suffix('}'))
        {
            let idx: usize = text_idx
                .parse()
                .map_err(|error| failure(&case.name, error))?;
            let path = paths
                .get(idx)
                .ok_or_else(|| failure(&case.name, "invalid auxiliary input index"))?;
            args.push(path.to_string_lossy().into_owned());
            used[idx] = true;
        } else {
            args.push(arg.to_owned());
        }
    }
    if used.iter().any(|used| !used) {
        return Err(failure(&case.name, "unused auxiliary command input"));
    }
    // Capture stderr without comparing exit status or program stdout
    let output = Command::new(path_cli).args(args).output()?;
    let text = String::from_utf8(output.stderr).map_err(|error| failure(&case.name, error))?;
    // Require the intended diagnostic before comparing or promoting stderr
    let code = case
        .code
        .as_deref()
        .ok_or_else(|| failure(&case.name, "command requires an expected diagnostic code"))?;
    if !text.contains(&format!("error[{code}]:")) {
        return Err(failure(&case.name, format!("missing expected command diagnostic {code}")));
    }
    Ok(text)
}
