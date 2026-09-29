use std::{
    fs,
    path::PathBuf,
    process,
    time::{SystemTime, UNIX_EPOCH},
};

use p4spec_rust::interface::p4::preprocessor::preprocess;

fn temporary_file() -> PathBuf {
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    std::env::temp_dir().join(format!("p4spec-{nonce}-{}.p4", process::id()))
}

#[test]
fn test_preprocessing_expands_macros_without_system_headers() {
    let path = temporary_file();
    fs::write(&path, "#define WIDTH 8\nbit<WIDTH> field;\n").unwrap();

    let output = preprocess(&[], &path).unwrap();

    fs::remove_file(&path).unwrap();
    assert!(output.contains("bit<8> field;"));
    assert!(output.contains(path.to_string_lossy().as_ref()));
}

#[test]
fn test_preprocessing_reports_a_typed_failure_for_missing_input() {
    let error = preprocess(&[], "/definitely/missing/p4spec-input.p4").unwrap_err();
    assert!(matches!(error, p4spec_rust::interface::p4::error::P4Error::Input(_)));
    let p4spec_rust::diagnostic::ReportKind::Cause(diagnostic) = &error.report().kind else {
        panic!("expected cause")
    };
    assert_eq!(diagnostic.code.as_deref(), Some("p4/preprocessor-failed"));
    assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), "/definitely/missing/p4spec-input.p4");
    assert_eq!(diagnostic.labels[0].span.left.line, 0);
    assert_eq!(diagnostic.labels[0].span.left, diagnostic.labels[0].span.right);
}

#[test]
fn test_macro_expansion_rejection_keeps_logical_line_only() {
    use p4spec_rust::{
        diagnostic::ReportKind,
        interface::p4::{error::P4Error, parse::parse_file},
        lang::data::value::ValueArena,
    };
    let path = temporary_file();
    fs::write(&path, "#define LONG 123456789012345678901234567890\nconst bit<8> x = LONG + ;\n")
        .unwrap();
    let error = parse_file(&mut ValueArena::new(), &[], &path).unwrap_err();
    fs::remove_file(&path).unwrap();
    assert!(matches!(error, P4Error::Syntax(_)));
    let ReportKind::Cause(diagnostic) = &error.report().kind else { panic!("expected cause") };
    assert!(diagnostic.labels[0].line_only);
    assert_eq!(diagnostic.labels[0].span.left.file.as_ref(), path.to_str().unwrap());
    assert_eq!(diagnostic.labels[0].span.left.line, 2);
    assert!(diagnostic.labels[0].span.left.column > 24);
}
