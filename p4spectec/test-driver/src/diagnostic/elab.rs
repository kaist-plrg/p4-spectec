//! Real elaboration inputs for diagnostic snapshot acceptance
//!
//! Fixtures must parse before elaboration is exercised.
//! Warnings and errors are returned in emission order for the shared runner
//! to render and compare with the expected text.

use std::path::Path;

use p4spectec::diagnostic::Report;

use p4spectec::frontend::parse::parse_files;

use p4spectec::pass::elaborate::convert_with_warnings;

use crate::Result;

use super::{Case, failure};

/// Loads and compares this module's diagnostic fixtures.
pub fn run(path: &Path, path_cli: Option<&Path>) -> Result<()> {
    let groups = super::load(path, false)?;
    super::run_registered("elab", &groups, path_cli, Some(run_case), Default::default())
}

/// Parses and elaborates one fixture, returning all emitted diagnostics.
fn run_case(case: &Case) -> Result<Vec<Report>> {
    let name = case.name.as_str();
    // Reject setup failures from the earlier frontend stage
    let spec_el = parse_files([case.path_input()])
        .map_err(|report| failure(name, format!("parser failed before elaboration: {report}")))?;

    // Preserve warnings emitted before the final elaboration result
    let (result, mut reports) = convert_with_warnings(spec_el);
    // Only explicitly registered baseline controls may elaborate successfully
    if result.is_ok() && !case.allow_success {
        return Err(failure(name, "elaboration accepted negative input"));
    }
    if let Err(report) = result {
        reports.push(*report);
    }
    Ok(reports)
}
