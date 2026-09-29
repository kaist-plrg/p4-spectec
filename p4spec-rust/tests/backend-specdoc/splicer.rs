//! Located splicing failures, warning order, and staged file replacement

use std::fs;

use p4spec_rust::{
    backend_specdoc::splicer::{splice_files_with_warnings, splice_strings_with_warnings},
    diagnostic::{Diagnostic, Label, Report, ReportKind, Severity},
    lang::el::ast as el,
};

use crate::{directory::Directory, spec_fixture};

fn cause(report: &Report) -> &Diagnostic {
    let ReportKind::Cause(diagnostic) = &report.kind else {
        panic!("expected a diagnostic cause");
    };
    diagnostic
}

#[test]
fn latex_failures_preserve_expression_locations_and_warning_order() {
    let exp = p4spec_rust::phrase! { node: el::ExpKind::Bool(true), span: Default::default() };
    let op = p4spec_rust::phrase! { node: el::FuseOpKind::Fuse, span: Default::default() };
    for (code, exp_kind, message) in [
        (
            "latex/hole-unsupported",
            el::ExpKind::Hole(el::Hole::Num(0)),
            "LaTeX rendering is undefined for a hole expression",
        ),
        (
            "latex/fuse-unsupported",
            el::ExpKind::Fuse(Box::new(exp.clone()), op, Box::new(exp.clone())),
            "LaTeX rendering is undefined for a fuse expression",
        ),
        (
            "latex/unparen-unsupported",
            el::ExpKind::Unparen(Box::new(exp)),
            "LaTeX rendering is undefined for an unparen expression",
        ),
        (
            "latex/raw-latex-unsupported",
            el::ExpKind::Latex("x".to_owned()),
            "raw LaTeX expressions are not allowed in canonical rendering",
        ),
    ] {
        let mut spec_el = spec_fixture::parse("dec $f : nat\ndef $f = %0\n").unwrap();
        let el::DefKind::FuncDef(def) = &mut spec_el[1].node else { panic!("function definition") };
        // Construct renderer inputs that elaboration would reject before splicing
        def.exp.node = exp_kind;
        let el::DefKind::FuncDef(def) = &spec_el[1].node else { panic!("function definition") };
        let anchor_ctx =
            p4spec_rust::backend_specdoc::anchor::AnchorContext::new(&|_, _| None, &|_, _| None);
        let report_direct =
            p4spec_rust::backend_specdoc::latex::render_def(&anchor_ctx, &spec_el[1]).unwrap_err();
        let sources = [
            ("body.adoc", "${func-prose: missing}\n${func-latex: f}"),
            ("title.adoc", "${func-title-latex: f}\n${func-title-latex: f}"),
        ];
        let (result, warnings) = splice_strings_with_warnings(&spec_el, &vec![], &sources);
        let report = result.unwrap_err();
        let diagnostic = cause(&report);
        assert_eq!(diagnostic.source, "latex");
        assert_eq!(diagnostic.severity, Severity::Error);
        assert_eq!(diagnostic.code.as_deref(), Some(code));
        assert_eq!(diagnostic.message, message);
        assert_eq!(diagnostic.code, cause(&report_direct).code);
        assert_eq!(diagnostic.labels, cause(&report_direct).labels);
        assert_eq!(diagnostic.notes, cause(&report_direct).notes);
        assert_eq!(
            diagnostic.labels,
            [Label::primary(&def.exp.span, "cannot render this expression")]
        );
        assert!(diagnostic.notes.is_empty());
        assert!(report.children.is_empty());
        assert_eq!(warnings.len(), 2);
        assert_eq!(cause(&warnings[0]).code.as_deref(), Some("splice/target-duplicate"));
        assert_eq!(cause(&warnings[0]).labels[0].span.left.file.as_ref(), "title.adoc");
        assert_eq!(cause(&warnings[0]).labels[0].span.left.line, 2);
        assert_eq!(cause(&warnings[1]).code.as_deref(), Some("splice/key-not-found"));
        assert_eq!(cause(&warnings[1]).labels[0].span.left.file.as_ref(), "body.adoc");
    }
}

#[test]
fn malformed_later_skeleton_prevents_rendering_and_output_creation() {
    let directory = Directory::new("splice-parse");
    let path_input = directory.0.join("input.adoc");
    let path_bad = directory.0.join("bad.adoc");
    let path_output = directory.0.join("output.adoc");
    let path_new = directory.0.join("new/output.adoc");
    fs::write(&path_input, "${func-prose: missing}").unwrap();
    fs::write(&path_bad, "${func-prose: @}").unwrap();
    fs::write(&path_output, "original").unwrap();
    let (result, warnings) = splice_files_with_warnings(
        &vec![],
        &vec![],
        &[(path_input, path_output.clone()), (path_bad, path_new.clone())],
    );
    assert_eq!(cause(&result.unwrap_err()).code.as_deref(), Some("splice/identifier-invalid"));
    assert!(warnings.is_empty());
    assert_eq!(fs::read_to_string(path_output).unwrap(), "original");
    assert!(!path_new.parent().unwrap().exists());
}

#[test]
fn rendering_failure_retains_warnings_without_replacing_destinations() {
    let directory = Directory::new("splice-render");
    let spec_el = spec_fixture::parse("def $f = %0").unwrap();
    let path_input = directory.0.join("input.adoc");
    let path_bad = directory.0.join("bad.adoc");
    let path_output = directory.0.join("output.adoc");
    let path_new = directory.0.join("new/output.adoc");
    fs::write(&path_input, "${func-prose: missing}").unwrap();
    fs::write(&path_bad, "${func-latex: f}").unwrap();
    fs::write(&path_output, "original").unwrap();
    let (result, warnings) = splice_files_with_warnings(
        &spec_el,
        &vec![],
        &[(path_input, path_output.clone()), (path_bad, path_new.clone())],
    );
    assert_eq!(cause(&result.unwrap_err()).code.as_deref(), Some("latex/hole-unsupported"));
    assert_eq!(warnings.len(), 1);
    assert_eq!(cause(&warnings[0]).code.as_deref(), Some("splice/key-not-found"));
    assert_eq!(fs::read_to_string(path_output).unwrap(), "original");
    assert!(!path_new.parent().unwrap().exists());
}

#[test]
fn staging_failure_keeps_all_outputs_and_cleans_pending_files() {
    let directory = Directory::new("splice-stage");
    let path_input = directory.0.join("input.adoc");
    let path_output = directory.0.join("output.adoc");
    let path_blocked = directory.0.join("blocked");
    fs::write(&path_input, "${func-prose: missing}").unwrap();
    fs::write(&path_output, "original").unwrap();
    fs::write(&path_blocked, "regular file").unwrap();
    let (result, warnings) = splice_files_with_warnings(
        &vec![],
        &vec![],
        &[
            (path_input.clone(), path_output.clone()),
            (path_input, path_blocked.join("output.adoc")),
        ],
    );
    assert_eq!(cause(&result.unwrap_err()).code.as_deref(), Some("splice/io"));
    assert_eq!(fs::read_to_string(path_output).unwrap(), "original");
    assert_eq!(fs::read_dir(&directory.0).unwrap().count(), 3);
    assert_eq!(warnings.len(), 20);
    assert!(
        warnings[..2]
            .iter()
            .all(|report| cause(report).code.as_deref() == Some("splice/key-not-found"))
    );
    assert!(
        warnings[2..]
            .iter()
            .all(|report| cause(report).code.as_deref() == Some("splice/keys-unused"))
    );
}

#[test]
fn commit_failure_preserves_earlier_replacements_and_cleans_remaining_files() {
    let directory = Directory::new("splice-commit");
    let path_input = directory.0.join("input.adoc");
    let path_output = directory.0.join("output.adoc");
    let path_blocked = directory.0.join("blocked");
    let path_later = directory.0.join("later.adoc");
    fs::write(&path_input, "replacement").unwrap();
    fs::write(&path_output, "original").unwrap();
    fs::create_dir(&path_blocked).unwrap();
    fs::write(&path_later, "later original").unwrap();
    let (result, _) = splice_files_with_warnings(
        &vec![],
        &vec![],
        &[
            (path_input.clone(), path_output.clone()),
            (path_input.clone(), path_blocked.clone()),
            (path_input, path_later.clone()),
        ],
    );
    assert_eq!(cause(&result.unwrap_err()).code.as_deref(), Some("splice/io"));
    assert_eq!(fs::read_to_string(path_output).unwrap(), "replacement");
    assert_eq!(fs::read_to_string(path_later).unwrap(), "later original");
    assert!(path_blocked.is_dir());
    assert_eq!(fs::read_dir(&directory.0).unwrap().count(), 4);
}

#[test]
fn overlapping_input_output_paths_read_original_documents_before_replacement() {
    let directory = Directory::new("splice-overlap");
    let path_a = directory.0.join("a.adoc");
    let path_b = directory.0.join("b.adoc");
    fs::write(&path_a, "document A").unwrap();
    fs::write(&path_b, "document B").unwrap();
    splice_files_with_warnings(
        &vec![],
        &vec![],
        &[(path_a.clone(), path_b.clone()), (path_b.clone(), path_a.clone())],
    )
    .0
    .unwrap();
    assert_eq!(fs::read_to_string(path_a).unwrap(), "document B");
    assert_eq!(fs::read_to_string(path_b).unwrap(), "document A");
}

#[cfg(unix)]
#[test]
fn file_replacement_preserves_relative_symlinks_and_permissions() {
    use std::os::unix::fs::{PermissionsExt, symlink};
    let directory = Directory::new("splice-symlink");
    let path_target = directory.0.join("target.adoc");
    let path_link = directory.0.join("link.adoc");
    fs::write(&path_target, "${func-prose: missing}").unwrap();
    fs::set_permissions(&path_target, fs::Permissions::from_mode(0o640)).unwrap();
    symlink("target.adoc", &path_link).unwrap();
    splice_files_with_warnings(&vec![], &vec![], &[(path_link.clone(), path_link.clone())])
        .0
        .unwrap();
    assert_eq!(fs::read_link(&path_link).unwrap().to_str(), Some("target.adoc"));
    assert_eq!(fs::read_to_string(&path_target).unwrap(), "****\n\n****");
    assert_eq!(fs::metadata(path_target).unwrap().permissions().mode() & 0o777, 0o640);
    assert_eq!(fs::read_dir(&directory.0).unwrap().count(), 2);
}
