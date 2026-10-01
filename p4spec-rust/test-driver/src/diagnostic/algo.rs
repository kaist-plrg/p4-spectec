//! Pinned algorithmic conversion failures
//!
//! Every fixture must parse and elaborate before the algorithmic check fails.
//! The runner compares the complete rendered report with its adjacent expectation.

use std::path::Path;

use p4spec_rust::diagnostic::Report;

use p4spec_rust::frontend::parse::parse_files;

use p4spec_rust::pass::elaborate;

use p4spec_rust::pass::algo;

use crate::Result;

use super::{Case, failure};

/// Loads and compares this module's diagnostic fixtures.
pub fn run(path: &Path, path_cli: Option<&Path>) -> Result<()> {
    let groups = super::load(path, false)?;
    super::run_registered("algo", &groups, path_cli, Some(run_case), Default::default())
}

/// Runs one source fixture through its intended algorithmic failure.
fn run_case(case: &Case) -> Result<Vec<Report>> {
    let name = case.name.as_str();
    // Earlier stage failures are setup errors, never accepted snapshots
    let spec_el = parse_files([case.path_input()]).map_err(|report| {
        failure(name, format!("parser failed before algorithmic conversion: {report}"))
    })?;
    let spec_il = elaborate::convert(spec_el).map_err(|report| {
        failure(name, format!("elaboration failed before algorithmic conversion: {report}"))
    })?;
    // Reject unexpected success even during snapshot promotion
    let report = algo::convert(spec_il)
        .err()
        .ok_or_else(|| failure(name, "algorithmic conversion unexpectedly succeeded"))?;
    Ok(vec![*report])
}
