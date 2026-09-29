//! Pinned prose-hint failures from the reference annotate suite
//!
//! Earlier stages must succeed before prosify rejects a source hint.
//! The shared runner compares each complete rendered report with its expectation.

use super::failure;
use crate::Result;
use p4spec_rust::{
    diagnostic::Report,
    frontend::parse::parse_files,
    pass::{algo, elaborate, prosify, structure},
};

/// Runs one source fixture through its intended prose failure.
pub fn run(name: &str) -> Result<Vec<Report>> {
    // Earlier stage failures are setup errors, never accepted snapshots
    let spec_el = parse_files([format!("prose/{name}")]).map_err(|report| {
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
