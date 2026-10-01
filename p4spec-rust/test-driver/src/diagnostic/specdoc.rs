//! Source-driven AsciiDoc warning acceptance
//!
//! Source conversion must succeed without warnings before rendering markup.
//! Every backend warning must retain a label on the registered source file.

use p4spec_rust::diagnostic::{Report, ReportKind, Severity};
use p4spec_rust::specdoc::adoc;

use crate::Result;

use super::{Case, failure};

/// Renders one source specification and requires located markup warnings.
pub fn run(case: &Case) -> Result<Vec<Report>> {
    let path = case.path_input();
    // Earlier diagnostics reject setup before backend acceptance
    let (result, reports) = p4spec_rust::prosify_with_warnings(std::slice::from_ref(&path));
    let spec_pl = result.map_err(|report| {
        failure(&case.name, format!("conversion failed before AsciiDoc rendering: {report}"))
    })?;
    if !reports.is_empty() {
        return Err(failure(&case.name, "conversion warned before AsciiDoc rendering"));
    }
    let mut warnings = Vec::new();
    adoc::pl::render_spec(&mut warnings, &spec_pl);
    if warnings.is_empty() {
        return Err(failure(&case.name, "AsciiDoc rendered without warnings"));
    }
    // Require backend causes with the real input's source location
    for report in &warnings {
        let ReportKind::Cause(diagnostic) = &report.kind else {
            return Err(failure(&case.name, "expected backend diagnostic cause"));
        };
        if diagnostic.severity != Severity::Warning
            || !diagnostic
                .code
                .as_deref()
                .is_some_and(|code| code.starts_with("adoc/"))
            || diagnostic.labels.is_empty()
            || diagnostic
                .labels
                .iter()
                .any(|label| label.span.left.file.as_ref() != path.to_string_lossy())
        {
            return Err(failure(&case.name, "unexpected backend diagnostic"));
        }
    }
    Ok(warnings)
}
