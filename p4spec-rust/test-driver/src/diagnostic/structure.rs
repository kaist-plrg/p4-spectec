//! Source-driven structured conversion rejection
//!
//! Parsing, elaboration, and AL conversion must succeed before structuring fails.
//! Both rule-group modes exercise the same registered source independently.

use std::path::Path;

use p4spec_rust::diagnostic::Report;

use p4spec_rust::pass::structure;

use crate::Result;

use super::{Case, failure};

/// Loads and compares this module's diagnostic fixtures.
pub fn run(path: &Path, path_cli: Option<&Path>) -> Result<()> {
    let groups = super::load(path, false)?;
    super::run_registered("structure", &groups, path_cli, Some(run_case), Default::default())
}

/// Requires a structure failure after successful source-to-AL conversion.
fn run_case(case: &Case) -> Result<Vec<Report>> {
    // Keep earlier pass failures out of the structure snapshot
    let spec_al = p4spec_rust::algo(&[case.path_input()]).map_err(|report| {
        failure(&case.name, format!("conversion failed before structuring: {report}"))
    })?;
    let without_rule_groups = match case.args.as_slice() {
        [] => false,
        [arg] if arg == "without-rule-groups" => true,
        _ => return Err(failure(&case.name, "invalid structure arguments")),
    };
    // Reject success even when promoting expectations
    let report = structure::convert(spec_al, without_rule_groups)
        .err()
        .ok_or_else(|| failure(&case.name, "structuring accepted negative input"))?;
    Ok(vec![*report])
}
