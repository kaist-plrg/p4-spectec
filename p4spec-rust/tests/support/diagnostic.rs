use p4spec_rust::{diagnostic::ReportKind, runner::ExternError};

/// Checks the code and message of a host failure.
pub fn assert_diagnostic(error: impl Into<ExternError>, code: Option<&str>, message: &str) {
    let report = error.into().into_report();
    let ReportKind::Cause(diagnostic) = &report.kind else {
        panic!("expected a diagnostic cause, got {report:?}")
    };
    assert_eq!(diagnostic.code.as_deref(), code);
    assert_eq!(diagnostic.message, message);
}
