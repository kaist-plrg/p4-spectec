use crate::interp::report::ReportExt;
use p4spec_rust::{
    diagnostic::{Diagnostic, Label, Report, ReportKind, Severity},
    interp::shared::backtrack::{self, Backtrack, Failure, WithFrame},
    lang::common::source::{Position, Span},
};

fn span(line: usize) -> Span {
    Span::new(Position::new("choice.watsup", line, 0), Position::new("choice.watsup", line, 1))
}

fn report() -> Box<Report> {
    let mut report: Report = Diagnostic::new(
        "host",
        Severity::Warning,
        Some("host/custom".into()),
        "host rejection",
        vec![Label::primary(&span(2), "input"), Label::secondary(&span(3), "origin")],
        vec!["host note".into()],
    )
    .into();
    report
        .children
        .push(Report::frame(span(4), "nested host context", vec![]));
    Box::new(report)
}

#[test]
fn nesting_keeps_failure_class_and_complete_incoming_report() {
    for fatal in [false, true] {
        let report = report();
        let text = report.render();
        let result: Backtrack<()> =
            Err(if fatal { Failure::Fatal(report) } else { Failure::Mismatch(vec![*report]) });
        let failure = result
            .with_frame(span(1), || "invocation".into())
            .unwrap_err();
        assert_eq!(matches!(&failure, Failure::Fatal(_)), fatal);
        let report = failure.into_report();
        let frame = if fatal { &*report } else { &report.children[0] };
        assert!(
            matches!(&frame.kind, ReportKind::Frame { span: loc, message } if loc == &span(1) && message == "invocation")
        );
        assert_eq!(frame.children.len(), 1);
        assert_eq!(frame.children[0].render(), text);
        let diagnostic = frame.children[0].diagnostic();
        assert_eq!(diagnostic.source, "host");
        assert_eq!(diagnostic.severity, Severity::Warning);
        assert_eq!(diagnostic.code.as_deref(), Some("host/custom"));
        assert_eq!(
            diagnostic.labels,
            vec![Label::primary(&span(2), "input"), Label::secondary(&span(3), "origin")]
        );
        assert_eq!(diagnostic.notes, ["host note"]);
    }
    let result: Backtrack<_> = Ok(7);
    assert_eq!(
        result
            .with_frame(span(1), || panic!("success formatted a failure"))
            .unwrap(),
        7
    );
}

#[test]
fn exhausted_mismatch_is_promoted_only_at_output() {
    let failure = Failure::Mismatch(Vec::new()).with_frame(span(1), "empty alternatives");
    assert!(matches!(&failure, Failure::Mismatch(reports) if reports.len() == 1));
    let report = failure.into_report();
    assert!(
        matches!(&report.kind, ReportKind::Frame { message, .. } if message == "execution failed")
    );
    let report = report.children.first().unwrap();
    assert_eq!(report.span(), span(1));
    assert!(report.children.is_empty());
}

#[test]
fn fatal_output_preserves_the_report_without_an_execution_wrapper() {
    let report = report();
    let text = report.render();
    let report = Failure::Fatal(report).into_report();
    assert_eq!(report.render(), text);
}

#[test]
fn lifting_local_failures_preserves_existing_locations_and_children() {
    let report = report();
    let text = report.render();
    let result: Backtrack<()> = backtrack::from_result(Err(report), &span(9));
    let Failure::Fatal(report) = result.unwrap_err() else { panic!("expected fatal") };
    assert_eq!(report.render(), text);
}

#[test]
fn lifting_numeric_failure_locates_its_cause() {
    let error = p4spec_rust::lang::common::prim::num::NumericError::NegativeNatural((-1).into());
    let result: Backtrack<()> = backtrack::from_result(Err(error), &span(3));
    let Failure::Fatal(report) = result.unwrap_err() else { panic!("expected fatal") };
    assert_eq!(report.span(), span(3));
    assert_eq!(report.diagnostic().code.as_deref(), Some("runtime/numeric-invalid"));
}

#[test]
fn lifting_type_and_match_failures_fills_only_unknown_locations() {
    use p4spec_rust::runtime::ops::{
        typ::{TypeError, TypeErrorKind},
        value::MatchError,
    };
    for (span_error, span_expect) in [(Span::default(), span(3)), (span(2), span(2))] {
        let reports: [Box<Report>; 2] = [
            TypeError { kind: TypeErrorKind::UndefinedType("X".into()), span: span_error.clone() }
                .into(),
            MatchError::UnexpectedTypeVariable { span: span_error.clone() }.into(),
        ];
        for report in reports {
            assert_eq!(report.diagnostic().labels.is_empty(), span_error == Span::default());
            let result: Backtrack<()> = backtrack::from_result(Err(report), &span(3));
            let Failure::Fatal(report) = result.unwrap_err() else { panic!("expected fatal") };
            assert_eq!(report.span(), span_expect);
        }
    }
}

#[test]
fn successful_checks_do_not_construct_diagnostics() {
    backtrack::check(true, span(1), || panic!("successful check constructed diagnostic")).unwrap();
}
