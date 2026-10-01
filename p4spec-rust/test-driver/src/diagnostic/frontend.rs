//! File-driven SpecTec parser rejection
//!
//! Binary lexical fixtures pass unchanged through the public file parser.
//! A .mixop file supplies text to the public mixfix parser.

use std::path::Path;

use p4spec_rust::diagnostic::Report;

use p4spec_rust::frontend::parse::{parse_files, parse_mixop};

use crate::Result;

use super::{Case, failure};

/// Loads and compares this module's diagnostic fixtures.
pub fn run(path: &Path, path_cli: Option<&Path>) -> Result<()> {
    let groups = super::load(path, false)?;
    super::run_registered("frontend", &groups, path_cli, Some(run_case), Default::default())
}

/// Requires the registered parser input to produce a diagnostic.
fn run_case(case: &Case) -> Result<Vec<Report>> {
    let path = case.path_input();
    // Mixfix signatures are a distinct public parser entry point
    let report = if path.extension().is_some_and(|ext| ext == "mixop") {
        parse_mixop(&std::fs::read_to_string(path)?).err()
    } else {
        parse_files([path]).err()
    };
    // Unexpected acceptance cannot be promoted into a negative expectation
    let report = report.ok_or_else(|| failure(&case.name, "parser accepted negative input"))?;
    Ok(vec![*report])
}
