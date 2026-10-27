//! Source-driven structured conversion rejection
//!
//! Parsing, elaboration, and AL conversion must succeed before structuring fails.
//! Both rule-group modes exercise the same registered source independently.

use p4spectec::diagnostic::Report;

use p4spectec::pass::structure;

use crate::Result;

use super::{Case, failure};

/// Requires a structure failure after successful source-to-AL conversion.
pub(super) fn run_case(case: &Case) -> Result<Vec<Report>> {
    // Keep earlier pass failures out of the structure snapshot
    let spec_al = p4spectec::algo(std::slice::from_ref(&case.input)).map_err(|report| {
        failure(&case.name, format!("conversion failed before structuring: {report}"))
    })?;
    // Reject success even when promoting expectations
    let report = structure::convert(spec_al, case.without_rule_groups)
        .err()
        .ok_or_else(|| failure(&case.name, "structuring accepted negative input"))?;
    Ok(vec![*report])
}
