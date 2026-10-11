//! Source syntax rejection through the production P4 parser

use p4spectec::lang::data::arena::Arena;

use p4spectec::diagnostic::Report;

use p4spectec::interface::p4::{error::P4ErrorKind, parse};

use crate::Result;

use super::{Case, failure};

/// Returns the actual source rejection for snapshot comparison.
pub fn run(case: &Case) -> Result<Vec<Report>> {
    let name = case.name.as_str();
    let mut arena = Arena::new();
    let path = case.path_input();
    let source = std::fs::read_to_string(&path)?;
    let error = parse::parse_string(&mut arena, &path, &source)
        .err()
        .ok_or_else(|| failure(name, "invalid P4 unexpectedly parsed"))?;
    if !matches!(error.kind, P4ErrorKind::Syntax(_)) {
        return Err(failure(name, "fixture failed outside syntax admission"));
    }
    Ok(vec![*error.into_report()])
}
