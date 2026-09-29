//! Source-driven AsciiDoc diagnostic acceptance
//!
//! Each fixture passes through parsing, elaboration, algorithmic conversion,
//! structuring, and prose conversion before the document renderer runs.
//! The driver validates reports without constructing or modifying document nodes.

use p4spec_rust::{
    backend_specdoc::adoc,
    diagnostic::{Report, ReportKind, Severity},
    prosify_with_warnings,
};

use super::failure;
use crate::Result;

/// Renders one complete specification and requires its intended markup warning.
pub fn run(name: &str) -> Result<Vec<Report>> {
    let path = format!("specdoc/{name}.watsup");
    // Earlier diagnostics reject the fixture before backend acceptance
    let (result, reports) = prosify_with_warnings([path.as_str()]);
    let spec_pl = result.map_err(|report| {
        failure(name, format!("conversion failed before AsciiDoc rendering: {report}"))
    })?;
    if !reports.is_empty() {
        return Err(failure(name, "conversion warned before AsciiDoc rendering"));
    }

    // The public renderer supplies normal link resolution for every definition
    let mut warnings = Vec::new();
    adoc::pl::render_spec(&mut warnings, &spec_pl);
    let code = match name {
        "adoc-nested-link" => "adoc/link-nested",
        "adoc-empty-body" => "adoc/link-body-empty",
        "adoc-invalid-text" => "adoc/link-text-invalid",
        _ => return Err(failure(name, "unknown AsciiDoc case")),
    };
    if warnings.is_empty() {
        return Err(failure(name, "AsciiDoc unexpectedly rendered without warnings"));
    }

    // Require the intended warning and its actual input source before promotion
    for report in &warnings {
        let ReportKind::Cause(diagnostic) = &report.kind else {
            return Err(failure(name, "expected a backend diagnostic cause"));
        };
        if diagnostic.code.as_deref() != Some(code) || diagnostic.severity != Severity::Warning {
            return Err(failure(name, "unexpected backend diagnostic"));
        }
        if diagnostic.labels.is_empty()
            || diagnostic
                .labels
                .iter()
                .any(|label| label.span.left.file.as_ref() != path)
        {
            return Err(failure(name, "backend diagnostic lost its input source location"));
        }
    }
    Ok(warnings)
}
