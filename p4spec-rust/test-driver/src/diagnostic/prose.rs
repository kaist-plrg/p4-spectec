//! Pinned prose-hint failures from the reference annotate suite
//!
//! Earlier stages must succeed before prosify rejects a source hint.
//! The shared runner compares each complete rendered report with its expectation.

use std::path::Path;

use p4spec_rust::diagnostic::Report;

use p4spec_rust::frontend::parse::parse_files;

use p4spec_rust::pass::elaborate;

use p4spec_rust::pass::algo;

use p4spec_rust::pass::structure;

use p4spec_rust::pass::prosify;

use crate::Result;

use super::{Case, failure};

/// Loads and compares this module's diagnostic fixtures.
pub fn run(path: &Path, path_cli: Option<&Path>) -> Result<()> {
    let groups = super::load(path, false)?;
    super::run_registered("prose", &groups, path_cli, Some(run_case), Default::default())
}

/// Runs one source fixture through its intended prose failure.
fn run_case(case: &Case) -> Result<Vec<Report>> {
    let name = case.name.as_str();
    // Earlier stage failures are setup errors, never accepted snapshots
    let spec_el = parse_files([case.path_input()]).map_err(|report| {
        failure(name, format!("parser failed before prose conversion: {report}"))
    })?;
    let spec_il = elaborate::convert(spec_el).map_err(|report| {
        failure(name, format!("elaboration failed before prose conversion: {report}"))
    })?;
    let spec_al = algo::convert(spec_il).map_err(|report| {
        failure(name, format!("algorithmic conversion failed before prose conversion: {report}"))
    })?;
    let spec_sl = structure::convert(spec_al, false).map_err(|report| {
        failure(name, format!("structuring failed before prose conversion: {report}"))
    })?;
    // Reject unexpected success even during snapshot promotion
    let report = prosify::convert(spec_sl)
        .err()
        .ok_or_else(|| failure(name, "prose conversion unexpectedly succeeded"))?;
    Ok(vec![*report])
}
