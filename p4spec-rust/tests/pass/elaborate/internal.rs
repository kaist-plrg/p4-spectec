use crate::{
    diagnostic::{Diagnostic, Label, LabelStyle, Report, ReportKind, Severity},
    lang::{
        common::source::{Position, Span},
        il::ast::TypKind,
    },
    pass::elaborate::{
        backtrack::{Backtrack, choose_sequential, finish},
        context::Context,
        error,
    },
    runtime::{
        envs::elab::TDEnv,
        ops::typ::{TypeErrorKind, expand_typ},
    },
};

#[test]
fn test_runtime_type_failure_keeps_its_category_and_source_span() {
    let span = Span::new(
        Position::new("elaboration.watsup", 7, 2),
        Position::new("elaboration.watsup", 7, 8),
    );
    let id = crate::phrase! {
        node: "Missing".to_owned(),
        span: span.clone(),
    };
    let typ = crate::phrase! {
        node: TypKind::Var(id, vec![]),
        span: span.clone(),
    };
    let type_error = expand_typ(&TDEnv::new(), &typ).unwrap_err();

    assert_eq!(type_error.kind, TypeErrorKind::TypeUndefined("Missing".to_owned()));
    assert_eq!(type_error.span, span);
    let report = error::typ::type_operation_invalid("expand type", type_error);
    let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected type cause") };
    assert_eq!(diagnostic.code.as_deref(), Some("elab/type-operation-invalid"));
    assert_eq!(diagnostic.labels[0].span, span);
    assert!(diagnostic.message.contains("Missing"));
}

fn foreign_report() -> Box<Report> {
    let span =
        Span::new(Position::new("foreign.watsup", 2, 1), Position::new("foreign.watsup", 2, 4));
    let diagnostic = Diagnostic {
        severity: Severity::Warning,
        code: Some("foreign/specific".to_owned()),
        message: "original cause".to_owned(),
        labels: vec![Label {
            style: LabelStyle::Secondary,
            span,
            message: "original label".to_owned(),
        }],
        notes: vec!["original note".to_owned()],
        source: "foreign",
    };
    let mut report = Report::from(diagnostic);
    report.children.push(Report {
        kind: ReportKind::Frame { span: Span::default(), message: "original child".to_owned() },
        children: Vec::new(),
    });
    Box::new(report)
}

fn assert_preserved(report: &Report) {
    let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected cause") };
    assert_eq!(diagnostic.source, "foreign");
    assert_eq!(diagnostic.code.as_deref(), Some("foreign/specific"));
    assert_eq!(diagnostic.severity, Severity::Warning);
    assert_eq!(diagnostic.message, "original cause");
    assert_eq!(diagnostic.notes, ["original note"]);
    assert_eq!(diagnostic.labels[0].style, LabelStyle::Secondary);
    assert_eq!(diagnostic.labels.len(), 1);
    assert_eq!(
        diagnostic.labels[0].span,
        Span::new(Position::new("foreign.watsup", 2, 1), Position::new("foreign.watsup", 2, 4))
    );
    assert_eq!(diagnostic.labels[0].message, "original label");
    assert_eq!(report.children.len(), 1);
    assert!(
        matches!(&report.children[0].kind, ReportKind::Frame { message, .. } if message == "original child")
    );
}

#[test]
fn test_attempt_preserves_a_complete_foreign_report() {
    let report = finish(Backtrack::<()>::Fatal(vec![*foreign_report()])).unwrap_err();
    assert_preserved(&report);
}

fn failure(message: &str, span: Span) -> Report {
    let diagnostic = Diagnostic {
        severity: Severity::Error,
        code: None,
        message: message.to_owned(),
        labels: vec![Label { style: LabelStyle::Primary, span, message: String::new() }],
        notes: Vec::new(),
        source: "test",
    };
    diagnostic.into()
}

#[test]
fn test_backtracking_preserves_nested_reports_and_alternative_order() {
    let failure_first =
        Backtrack::<()>::Mismatch(vec![*foreign_report()]).nest(Span::default(), "outer attempt");
    let failure_second = Backtrack::Mismatch(vec![failure("second alternative", Span::default())]);
    let mut ctx = Context::new();
    let report =
        finish(choose_sequential(&mut ctx, |_| failure_first, |_| failure_second)).unwrap_err();
    assert_eq!(report.children.len(), 2);
    assert_eq!(report.children[0].children.len(), 1);
    assert_preserved(&report.children[0].children[0]);
    let ReportKind::Cause(diagnostic) = &report.children[1].kind else { panic!("expected cause") };
    assert_eq!(diagnostic.message, "second alternative");
}

#[test]
fn test_finished_attempt_keeps_its_inner_reports_when_wrapped() {
    let report_inner =
        finish(Backtrack::<()>::Mismatch(vec![failure("inner failure", Span::default())]))
            .unwrap_err();
    let report_outer = finish(
        Backtrack::<()>::Mismatch(vec![*report_inner]).nest(Span::default(), "outer search"),
    )
    .unwrap_err();
    let report = &report_outer.children[0];
    let ReportKind::Cause(diagnostic) = &report.kind else {
        panic!("expected preserved inner cause")
    };
    assert_eq!(diagnostic.message, "inner failure");
}

#[test]
fn test_silent_failed_alternative_rolls_back_and_commits_the_winner() {
    let id_failed = crate::phrase!(node: "failed".to_owned(), span: Span::default());
    let id_winner = crate::phrase!(node: "winner".to_owned(), span: Span::default());
    let mut ctx = Context::new();
    let value = finish(choose_sequential(
        &mut ctx,
        |ctx| {
            ctx.frees.insert(id_failed.clone());
            Backtrack::Mismatch(vec![])
        },
        |ctx| {
            assert!(!ctx.frees.contains(&id_failed));
            ctx.frees.insert(id_winner.clone());
            Backtrack::Success(7)
        },
    ))
    .unwrap();
    assert_eq!(value, 7);
    assert!(!ctx.frees.contains(&id_failed));
    assert!(ctx.frees.contains(&id_winner));
}

#[test]
fn test_all_failed_alternatives_keep_context_and_ordered_causes() {
    let id = crate::phrase!(node: "failed".to_owned(), span: Span::default());
    let mut ctx = Context::new();
    let result = choose_sequential(
        &mut ctx,
        |ctx| {
            ctx.frees.insert(id.clone());
            Backtrack::<()>::Mismatch(vec![failure("first", Span::default())])
        },
        |ctx| {
            assert!(!ctx.frees.contains(&id));
            ctx.frees.insert(id.clone());
            Backtrack::Mismatch(vec![failure("second", Span::default())])
        },
    );
    assert!(!ctx.frees.contains(&id));
    let report = finish(result).unwrap_err();
    assert_eq!(report.children.len(), 2);
    for (report, message) in report.children.iter().zip(["first", "second"]) {
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected cause") };
        assert_eq!(diagnostic.message, message);
    }
}

#[test]
fn test_fatal_failure_aborts_sequential_fallback() {
    let id_failed = crate::phrase!(node: "failed".to_owned(), span: Span::default());
    let mut ctx = Context::new();
    let result: Backtrack<()> = choose_sequential(
        &mut ctx,
        |ctx| {
            ctx.frees.insert(id_failed.clone());
            Backtrack::Fatal(vec![failure("fatal", Span::default())])
        },
        |_| panic!("fatal failure tried the next alternative"),
    );
    assert!(matches!(result, Backtrack::Fatal(_)));
    assert!(!ctx.frees.contains(&id_failed));
}

#[test]
fn test_nesting_does_not_wrap_a_fatal_report() {
    let result = Backtrack::<()>::Fatal(vec![failure("fatal", Span::default())])
        .nest(Span::default(), "attempt context");
    let report = finish(result).unwrap_err();
    let ReportKind::Cause(diagnostic) = &report.kind else { panic!("expected direct cause") };
    assert_eq!(diagnostic.message, "fatal");
    assert!(report.children.is_empty());
}

#[test]
fn test_unavailable_alternative_rolls_back_before_successful_retry() {
    let id_failed = crate::phrase!(node: "failed".to_owned(), span: Span::default());
    let id_winner = crate::phrase!(node: "winner".to_owned(), span: Span::default());
    let mut ctx = Context::new();
    let result = choose_sequential(
        &mut ctx,
        |ctx| {
            ctx.frees.insert(id_failed.clone());
            Backtrack::Unavailable(vec![*foreign_report()])
        },
        |ctx| {
            assert!(!ctx.frees.contains(&id_failed));
            ctx.frees.insert(id_winner.clone());
            Backtrack::Success(7)
        },
    );
    assert!(matches!(result, Backtrack::Success(7)));
    assert!(!ctx.frees.contains(&id_failed));
    assert!(ctx.frees.contains(&id_winner));
}

#[test]
fn test_recoverable_alternatives_preserve_state_causes_and_context() {
    for first_unavailable in [true, false] {
        for second_unavailable in [true, false] {
            let id = crate::phrase!(node: "failed".to_owned(), span: Span::default());
            let mut ctx = Context::new();
            let result = choose_sequential(
                &mut ctx,
                |ctx| {
                    ctx.frees.insert(id.clone());
                    let reports = vec![failure("first", Span::default())];
                    if first_unavailable {
                        Backtrack::<()>::Unavailable(reports)
                    } else {
                        Backtrack::Mismatch(reports)
                    }
                },
                |ctx| {
                    assert!(!ctx.frees.contains(&id));
                    ctx.frees.insert(id.clone());
                    let reports = vec![failure("second", Span::default())];
                    if second_unavailable {
                        Backtrack::Unavailable(reports)
                    } else {
                        Backtrack::Mismatch(reports)
                    }
                },
            );
            assert_eq!(
                matches!(&result, Backtrack::Unavailable(_)),
                first_unavailable && second_unavailable,
            );
            assert!(!ctx.frees.contains(&id));
            let report = finish(result).unwrap_err();
            assert_eq!(report.children.len(), 2);
            for (report, text) in report.children.iter().zip(["first", "second"]) {
                let ReportKind::Cause(diagnostic) = &report.kind else {
                    panic!("expected original cause")
                };
                assert_eq!(diagnostic.message, text);
            }
        }
    }
}

#[test]
fn test_fatal_retry_rolls_back_after_unavailable_rule() {
    let id = crate::phrase!(node: "failed".to_owned(), span: Span::default());
    let mut ctx = Context::new();
    let result = choose_sequential(
        &mut ctx,
        |ctx| {
            ctx.frees.insert(id.clone());
            Backtrack::<()>::Unavailable(vec![])
        },
        |ctx| {
            assert!(!ctx.frees.contains(&id));
            ctx.frees.insert(id.clone());
            Backtrack::Fatal(vec![*foreign_report()])
        },
    );
    assert!(matches!(&result, Backtrack::Fatal(_)));
    assert!(!ctx.frees.contains(&id));
    assert_preserved(&finish(result).unwrap_err());
}

#[test]
fn test_unwrap_preserves_every_failure_state() {
    use crate::pass::elaborate::backtrack::{success, unwrap};

    fn propagate(result: Backtrack<usize>) -> Backtrack<usize> {
        let value = unwrap!(result);
        success!(value + 1)
    }

    assert!(matches!(propagate(Backtrack::Success(7)), Backtrack::Success(8)));
    let result = propagate(Backtrack::Unavailable(vec![*foreign_report()]));
    assert!(matches!(&result, Backtrack::Unavailable(_)));
    assert_preserved(&finish(result).unwrap_err());
    let result = propagate(Backtrack::Mismatch(vec![*foreign_report()]));
    assert!(matches!(&result, Backtrack::Mismatch(_)));
    assert_preserved(&finish(result).unwrap_err());
    let result = propagate(Backtrack::Fatal(vec![*foreign_report()]));
    assert!(matches!(&result, Backtrack::Fatal(_)));
    assert_preserved(&finish(result).unwrap_err());
}

#[test]
fn test_unavailable_promotion_keeps_causes_and_fatal_barriers() {
    let result = Backtrack::<()>::Unavailable(vec![*foreign_report()]).unavailable_as_mismatch();
    assert!(matches!(&result, Backtrack::Mismatch(_)));
    assert_preserved(&finish(result).unwrap_err());
    let result = Backtrack::<()>::Fatal(vec![*foreign_report()]).unavailable_as_mismatch();
    assert!(matches!(&result, Backtrack::Fatal(_)));
    assert_preserved(&finish(result).unwrap_err());
    let result = Backtrack::<()>::Unavailable(vec![*foreign_report()]).recoverable_as_failure();
    assert!(matches!(&result, Backtrack::Fatal(_)));
    assert_preserved(&finish(result).unwrap_err());
}

#[test]
fn test_unavailable_mapping_and_nesting_preserve_state() {
    let result = Backtrack::<usize>::Unavailable(vec![*foreign_report()])
        .map(|_| panic!("unavailable result was mapped"))
        .nest(Span::default(), "outer attempt");
    assert!(matches!(&result, Backtrack::Unavailable(_)));
    let report = finish(result).unwrap_err();
    assert_eq!(report.children.len(), 1);
    assert_preserved(&report.children[0]);
}
