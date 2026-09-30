//! Source syntax rejection through the production P4 parser

use p4spec_rust::{
    diagnostic::Report,
    interface::p4::{error::P4ErrorKind, parse},
    lang::data::value::ValueArena,
};

use super::failure;
use crate::Result;

/// Returns the actual source rejection for snapshot comparison.
pub fn run(name: &str) -> Result<Vec<Report>> {
    let mut arena = ValueArena::new();
    let path = "interp/p4-syntax.p4";
    let source = std::fs::read_to_string(path)?;
    let error = parse::parse_string(&mut arena, path, &source)
        .err()
        .ok_or_else(|| failure(name, "invalid P4 unexpectedly parsed"))?;
    if !matches!(error.kind, P4ErrorKind::Syntax(_)) {
        return Err(failure(name, "fixture failed outside syntax admission"));
    }
    Ok(vec![*error.into_report()])
}
