//! Pinned algorithmic conversion failures
//!
//! Every fixture must parse and elaborate before the algorithmic check fails.
//! The runner compares the complete rendered report with its adjacent expectation.

use p4spec_rust::diagnostic::Report;

use p4spec_rust::frontend::parse::parse_files;

use p4spec_rust::pass::elaborate;

use p4spec_rust::pass::algo;

use crate::Result;

use super::failure;

/// Runs one source fixture through its intended algorithmic failure.
pub fn run(name: &str) -> Result<Vec<Report>> {
    // Earlier stage failures are setup errors, never accepted snapshots
    let spec_el = parse_files([format!("algo/{name}")]).map_err(|report| {
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
