//! Real elaboration inputs for diagnostic snapshot acceptance
//!
//! Fixtures must parse before elaboration is exercised.
//! Warnings and errors are returned in emission order for the shared runner
//! to render and compare with the expected text.

use p4spec_rust::diagnostic::Report;

use p4spec_rust::frontend::parse::parse_files;

use p4spec_rust::pass::elaborate::convert_with_warnings;

use crate::Result;

use super::failure;

/// Parses and elaborates one fixture, returning all emitted diagnostics.
pub fn run(name: &str) -> Result<Vec<Report>> {
    // Reject setup failures from the earlier frontend stage
    let spec_el = parse_files([format!("elab/{name}")])
        .map_err(|report| failure(name, format!("parser failed before elaboration: {report}")))?;

    // Preserve warnings emitted before the final elaboration result
    let (result, mut reports) = convert_with_warnings(spec_el);
    if let Err(report) = result {
        reports.push(*report);
    }
    Ok(reports)
}
