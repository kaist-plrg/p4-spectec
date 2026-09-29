//! Native backend diagnostics through public rendering and splicing APIs
//!
//! LaTeX cases use parsed source spans, including unsupported EL hint forms.
//! AsciiDoc cases retain their fragment location and unchanged fallback text.

use p4spec_rust::{
    backend_specdoc::{
        adoc::pl::doc::{
            doc::{Link, Prose},
            serialize,
        },
        anchor::AnchorContext,
        latex, splicer,
    },
    diagnostic::{Report, ReportKind, Severity},
    frontend::parse::parse_files,
};

use super::failure;
use crate::Result;

/// Requires the intended backend code before snapshotting any report.
fn require(name: &str, report: &Report, code: &str, severity: Severity) -> Result<()> {
    // Reject unrelated frames before inspecting backend diagnostic fields
    let ReportKind::Cause(diagnostic) = &report.kind else {
        return Err(failure(name, "expected a backend diagnostic cause"));
    };
    // Require the intended failure or warning before accepting a snapshot
    if diagnostic.code.as_deref() != Some(code) || diagnostic.severity != severity {
        return Err(failure(name, "unexpected backend diagnostic"));
    }
    // Keep the parsed source identity through the rendering boundary
    let path = if name.starts_with("latex-") {
        format!("specdoc/{name}.watsup")
    } else {
        "specdoc/render.watsup".to_owned()
    };
    if diagnostic.labels.len() != 1 || diagnostic.labels[0].span.left.file.as_ref() != path {
        return Err(failure(name, "backend diagnostic lost its original source location"));
    }
    Ok(())
}

/// Exercises backend-owned failures and warnings using located fragments.
pub fn run(name: &str) -> Result<Vec<Report>> {
    let path = if name.starts_with("latex-") {
        format!("specdoc/{name}.watsup")
    } else {
        "specdoc/render.watsup".to_owned()
    };
    let spec_el = parse_files([path]).map_err(|error| failure(name, error))?;
    // Direct EL rendering diagnoses forms that elaboration would reject earlier
    if name.starts_with("latex-") {
        let code = match name {
            "latex-hole" => "latex/hole-unsupported",
            "latex-fuse" => "latex/fuse-unsupported",
            "latex-unparen" => "latex/unparen-unsupported",
            "latex-raw" => "latex/raw-latex-unsupported",
            "latex-link-target" => "latex/link-target-invalid",
            _ => return Err(failure(name, "unknown LaTeX case")),
        };
        // Custom resolvers are part of the public LaTeX entry-point contract
        let anchor_ctx = AnchorContext::new(&|_, _| Some("invalid-target".into()), &|_, _| None);
        let report = latex::render_def(&anchor_ctx, &spec_el[0])
            .err()
            .ok_or_else(|| failure(name, "LaTeX unexpectedly succeeded"))?;
        require(name, &report, code, Severity::Error)?;
        // Unsupported expressions also traverse the production splice boundary
        if name != "latex-link-target" {
            let report_splice =
                splicer::splice_strings(&spec_el, &vec![], &[("body.adoc", "${func-latex: f}")])
                    .err()
                    .ok_or_else(|| failure(name, "splice unexpectedly succeeded"))?;
            require(name, &report_splice, code, Severity::Error)?;
            return Ok(vec![*report_splice]);
        }
        return Ok(vec![*report]);
    }
    // Each fallback keeps the same visible document while collecting one warning
    let (prose, code, text_expect) = match name {
        "adoc-empty-target" => (
            Prose::link(Link::Direct(String::new()), Prose::text("x")),
            "adoc/link-target-empty",
            "xref:[x]",
        ),
        "adoc-nested-link" => (
            Prose::link(
                Link::Direct("outer".into()),
                Prose::link(Link::Direct("inner".into()), Prose::text("x")),
            ),
            "adoc/link-nested",
            "xref:outer[x]",
        ),
        "adoc-empty-body" => (
            Prose::link(Link::Direct("target".into()), Prose::Empty),
            "adoc/link-body-empty",
            "xref:target[]",
        ),
        "adoc-invalid-text" => (
            Prose::link(Link::Direct("target".into()), Prose::text("[x]<y>")),
            "adoc/link-text-invalid",
            "[x]<y>",
        ),
        _ => return Err(failure(name, "unknown AsciiDoc case")),
    };
    let anchor_ctx = AnchorContext::new(&|_, _| None, &|_, _| None);
    let mut warnings = Vec::new();
    let text = serialize::ser_prose(&anchor_ctx, &mut warnings, &spec_el[0].span, &prose);
    if text != text_expect || warnings.len() != 1 {
        return Err(failure(name, "AsciiDoc fallback or warning count changed"));
    }
    require(name, &warnings[0], code, Severity::Warning)?;
    Ok(warnings)
}
