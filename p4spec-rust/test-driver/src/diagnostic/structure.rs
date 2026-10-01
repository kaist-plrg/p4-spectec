//! Source-driven structured conversion rejection
//!
//! Parsing, elaboration, and AL conversion must succeed before structuring fails.
//! Both rule-group modes exercise the same registered source independently.

use p4spec_rust::{diagnostic::Report, pass::structure};

use crate::Result;

use super::{Case, failure};

/// Requires a structure failure after successful source-to-AL conversion.
pub fn run(case: &Case) -> Result<Vec<Report>> {
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
