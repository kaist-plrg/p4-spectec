//! Simulator selection through the production build boundary
//!
//! Unknown architectures fail before specification loading and have no location.

use super::failure;
use crate::Result;
use p4spec_rust::{
    diagnostic::{Report, ReportKind},
    lang::data::value::external::Encoding,
    runner::{Config, Spec},
    sim_plugin,
};

/// Rejects an unsupported architecture through the actual simulator builder.
pub(super) fn run(name: &str) -> Result<Vec<Report>> {
    if name != "sim-unsupported-architecture" {
        return Err(failure(name, "unknown boundary case"));
    }
    let report = match sim_plugin::build(
        Spec::Al(vec![]),
        "unsupported",
        Config::new(true, false, false),
        Encoding::default(),
    ) {
        Ok(_) => return Err(failure(name, "unsupported architecture accepted")),
        Err(report) => report,
    };
    // Reject accidental load failures or fabricated source locations
    let ReportKind::Cause(diagnostic) = &report.kind else {
        return Err(failure(name, "expected architecture diagnostic"));
    };
    if diagnostic.code.as_deref() != Some("sim/architecture-unsupported")
        || diagnostic.source != "sim"
        || !diagnostic.labels.is_empty()
    {
        return Err(failure(name, "wrong architecture diagnostic"));
    }
    Ok(vec![*report])
}
