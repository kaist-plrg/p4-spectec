//! Source-driven AsciiDoc warning acceptance
//!
//! Source conversion must succeed without warnings before rendering markup.
//! Every backend warning must retain a label on the registered source file.

use std::path::Path;

use p4spectec::diagnostic::{Report, ReportKind, Severity};

use p4spectec::specdoc::adoc;

use crate::Result;

use super::{Case, failure};

/// Loads and compares this module's diagnostic fixtures.
pub fn run(path: &Path, path_cli: Option<&Path>) -> Result<()> {
    let groups = super::load(path, false)?;
    super::run_registered("specdoc", &groups, path_cli, Some(run_case), Default::default())
}

/// Renders one source specification and requires located markup warnings.
fn run_case(case: &Case) -> Result<Vec<Report>> {
    let path = case.path_input();
    // Earlier diagnostics reject setup before backend acceptance
    let (result, reports) = p4spectec::prosify_with_warnings(std::slice::from_ref(&path));
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
                .any(|label| label.span.left.file.name() != path.to_string_lossy())
        {
            return Err(failure(&case.name, "unexpected backend diagnostic"));
        }
    }
    Ok(warnings)
}
