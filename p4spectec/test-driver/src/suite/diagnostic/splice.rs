//! Source-backed negative document skeleton acceptance
//!
//! Registered auxiliary specification files are parsed and prosified normally.
//! The primary .adoc fixture then reaches the public splice API.

use p4spectec::diagnostic::Report;

use p4spectec::specdoc::splicer;

use crate::harness::Result;

use super::{Case, failure};

/// Splices a registered skeleton against its declared source specification.
pub(super) fn run_case(case: &Case) -> Result<Vec<Report>> {
    let paths = &case.inputs;
    if paths.is_empty() {
        return Err(failure(&case.name, "splice requires source specification inputs"));
    }
    // Obtain both document inputs from the product pipeline
    let spec_el = p4spectec::frontend::parse::parse_files(paths).map_err(|report| {
        failure(&case.name, format!("parsing failed before splicing: {report}"))
    })?;
    let spec_pl = p4spectec::prosify(paths).map_err(|report| {
        failure(&case.name, format!("conversion failed before splicing: {report}"))
    })?;
    let path = &case.input;
    let text = std::fs::read_to_string(path)?;
    let text_path = path.to_string_lossy();
    // Require an actual marker rejection before comparing its report
    let report = splicer::splice_strings(&spec_el, &spec_pl, &[(&text_path, &text)])
        .err()
        .ok_or_else(|| failure(&case.name, "splicer accepted negative input"))?;
    Ok(vec![*report])
}
