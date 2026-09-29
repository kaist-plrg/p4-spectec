use super::{report as report_cause, span};
use codespan_reporting::term::DisplayStyle;
use p4spec_rust::{
    diagnostic::{Diagnostic, Label, RenderConfig, Renderer, Report, ReportKind, Severity},
    lang::common::source::Span,
};

fn frame(message: &str, children: Vec<Report>) -> Report {
    Report {
        kind: ReportKind::Frame { span: Span::default(), message: message.to_owned() },
        children,
    }
}

fn failure(message: &str, children: Vec<Report>) -> Report {
    Report {
        kind: ReportKind::Cause(Diagnostic {
            severity: Severity::Error,
            code: Some("test/failure".to_owned()),
            message: message.to_owned(),
            labels: vec![],
            notes: vec![],
            source: "test",
        }),
        children,
    }
}

fn chain(depth: usize, mut report: Report) -> Report {
    for level in (1..=depth).rev() {
        report = frame(&format!("level {level}"), vec![report]);
    }
    report
}

#[test]
fn source_locations_fill_only_unlocated_causes() {
    let span_call = span("input", 2, 0, 2, 3);
    let span_other = span("input", 3, 0, 3, 3);
    let report = failure("failure", vec![]).with_span(&span_call);
    assert_eq!(super::cause(&report).labels, [Label::primary(&span_call, "")]);
    let report = report.with_span(&span_other);
    assert_eq!(super::cause(&report).labels, [Label::primary(&span_call, "")]);
    let mut report = failure("related", vec![]);
    super::cause_mut(&mut report)
        .labels
        .push(Label::secondary(&span_other, "origin"));
    let report = report.with_span(&span_call);
    assert_eq!(super::cause(&report).labels, [Label::secondary(&span_other, "origin")]);
    let report = Report::frame(span_other.clone(), "call", vec![failure("child", vec![])])
        .with_span(&span_call);
    assert!(matches!(&report.kind, ReportKind::Frame { span, .. } if span == &span_other));
    assert!(super::cause(&report.children[0]).labels.is_empty());
}

#[test]
fn appended_children_preserve_existing_reports_and_order() {
    let report = failure("parent", vec![failure("first", vec![])])
        .with_children(vec![frame("second", vec![failure("nested", vec![])])])
        .with_children(vec![failure("third", vec![])]);
    let text = Renderer::new(RenderConfig::default())
        .render_to_string(&report)
        .unwrap();
    assert!(text.find("first").unwrap() < text.find("second").unwrap(), "{text}");
    assert!(text.find("nested").unwrap() < text.find("third").unwrap(), "{text}");
    assert_eq!(report.children.len(), 3);
    assert_eq!(report.children[1].children.len(), 1);
}

#[test]
fn maximum_depth_counts_causes_and_frames_on_the_deepest_branch() {
    let report = failure("leaf", vec![]);
    assert_eq!(report.depth_max(), 1);
    let report = frame("root", vec![chain(3, report), failure("shallow", vec![])]);
    assert_eq!(report.depth_max(), 5);
}

#[test]
fn short_frames_keep_locations_while_internal_and_sibling_causes_stay_rich() {
    let mut cause = report_cause(span("input", 2, 0, 2, 3));
    cause.children.push(report_cause(span("input", 3, 0, 3, 3)));
    let report = Report::frame(
        span("input", 1, 0, 1, 5),
        "while invoking R",
        vec![
            cause,
            Report::frame(
                span("input", 1, 0, 1, 5),
                "while invoking S",
                vec![report_cause(span("input", 4, 0, 4, 3))],
            ),
        ],
    );
    let mut renderer = Renderer::new(RenderConfig {
        frame_style: Some(DisplayStyle::Short),
        ..Default::default()
    });
    renderer.insert_source("input", "frame\nbad\nbad\nbad\n");
    let text = renderer.render_to_string(&report).unwrap();
    assert!(text.starts_with("input:1:1: note: while invoking R\n├─ error["), "{text}");
    assert!(text.contains("└─ input:1:1: note: while invoking S\n   └─ error["), "{text}");
    assert!(!text.contains("1 │ frame"), "{text}");
    for line in [2, 3, 4] {
        assert!(text.contains(&format!("{line} │ bad")), "{text}");
    }
    assert_eq!(text.matches("^^^ invalid escape").count(), 3, "{text}");
    assert_eq!(text.matches("use a supported escape").count(), 3, "{text}");
    assert_eq!(report.children.len(), 2);
    assert_eq!(report.children[0].children.len(), 1);
}

#[test]
fn compact_frames_preserve_fallback_locations_without_source_snippets() {
    for style in [DisplayStyle::Short, DisplayStyle::Medium] {
        let mut renderer =
            Renderer::new(RenderConfig { frame_style: Some(style), ..Default::default() });
        renderer.insert_source("control", "bad\u{1b}[31m");
        for (span, loc) in [
            (
                span("missing/frame.watsup", 4, 2, 4, 2),
                "missing/frame.watsup:4:3 (source unavailable)",
            ),
            (span("generated", 0, 0, 0, 0), "at generated"),
            (span("control", 1, 0, 1, 3), "snippet omitted: source contains control characters"),
        ] {
            let report = Report::frame(span, "while invoking R", vec![]);
            let text = renderer.render_to_string(&report).unwrap();
            assert!(text.contains(loc), "{text}");
            assert!(!text.contains('│'), "{text}");
            assert!(!text.contains('\u{1b}'), "{text}");
        }
    }
}

#[test]
fn frame_style_inherits_the_snippet_style_unless_overridden() {
    let report = Report::frame(span("input", 1, 0, 1, 3), "context", vec![]);
    for (style, rich) in [(DisplayStyle::Rich, true), (DisplayStyle::Short, false)] {
        let mut config = RenderConfig::default();
        config.snippet.display_style = style;
        let mut renderer = Renderer::new(config);
        renderer.insert_source("input", "bad");
        let text = renderer.render_to_string(&report).unwrap();
        assert_eq!(text.contains("1 │ bad"), rich, "{text}");
        assert!(text.contains("input:1:1"), "{text}");
    }
}

#[test]
fn deep_trace_chains_restart_indentation_without_hiding_reports() {
    let report = frame("root", vec![chain(8, failure("leaf", vec![]))]);
    let text = Renderer::new(RenderConfig::default())
        .render_to_string(&report)
        .unwrap();
    assert_eq!(
        text,
        concat!(
            "note: root\n\n",
            "└─ note: level 1\n\n",
            "   └─ note: level 2\n\n",
            "      └─ note: level 3\n\n",
            "         └─ note: level 4\n\n",
            "⋮ (depth 5, continued)\n",
            "└─ note: level 5\n\n",
            "   └─ note: level 6\n\n",
            "      └─ note: level 7\n\n",
            "         └─ note: level 8\n\n",
            "⋮ (depth 9, continued)\n",
            "└─ error[test/failure]: leaf\n\n",
        )
    );
    assert_eq!(report.children.len(), 1);
}

#[test]
fn deep_pending_siblings_keep_their_connectors_until_the_branch_finishes() {
    let report = frame(
        "root",
        vec![
            chain(10, failure("first leaf\ncontinued message", vec![])),
            chain(4, failure("last leaf", vec![])),
        ],
    );
    let text = Renderer::new(RenderConfig::default())
        .render_to_string(&report)
        .unwrap();
    let (first, last) = text.split_once("\n└─ note: level 1\n").unwrap();
    assert!(!first.contains("continued)"), "{text}");
    assert!(!text.contains("ancestors]"), "{text}");
    for line in first.lines().skip(2) {
        assert!(line.starts_with('├') || line.starts_with('│'), "{line:?}\n{text}");
    }
    assert!(first.contains("continued message"), "{text}");
    assert!(last.contains("⋮ (depth 5, continued)\n└─ error[test/failure]: last leaf"), "{text}");
}

#[test]
fn folded_branches_restore_the_anchor_before_rendering_the_next_sibling() {
    let report = frame(
        "root",
        vec![chain(
            4,
            frame(
                "branch",
                vec![
                    chain(5, failure("first leaf", vec![])),
                    chain(4, failure("last leaf", vec![])),
                ],
            ),
        )],
    );
    let text = Renderer::new(RenderConfig::default())
        .render_to_string(&report)
        .unwrap();
    let (first, last) = text.split_once("\n   └─ note: level 1\n").unwrap();
    assert_eq!(first.matches("continued)").count(), 1, "{text}");
    assert!(first.contains("⋮ (depth 5, continued)\n└─ note: branch"), "{text}");
    assert!(first.contains("│"), "{text}");
    assert!(first.contains("first leaf"), "{text}");
    assert!(last.contains("⋮ (depth 9, continued)\n└─ note: level 4"), "{text}");
    assert!(last.ends_with("   └─ error[test/failure]: last leaf\n\n"), "{text}");
}

#[test]
fn folded_snippets_keep_their_source_and_ascii_character_set() {
    let report = frame("root", vec![chain(4, report_cause(span("input", 1, 0, 1, 3)))]);
    let mut config = RenderConfig::default();
    config.snippet.chars = codespan_reporting::term::Chars::ascii();
    let mut renderer = Renderer::new(config);
    renderer.insert_source("input", "bad");
    let text = renderer.render_to_string(&report).unwrap();
    assert!(
        text.contains("... (depth 5, continued)\n`- error[parse/text-escape-invalid]"),
        "{text}"
    );
    assert!(text.contains("input:1:1"), "{text}");
    assert!(text.contains("1 | bad"), "{text}");
    assert!(text.contains("^^^ invalid escape"), "{text}");
    assert!(text.is_ascii(), "{text}");
}

#[test]
fn folding_does_not_consume_the_trace_budget_or_announce_hidden_nodes() {
    let report = frame("root", vec![chain(8, failure("leaf", vec![]))]);
    for (limit, folded, tail) in [
        (4, false, "            └─ ... further reports omitted (trace limit: 4)\n"),
        (5, true, "   └─ ... further reports omitted (trace limit: 5)\n"),
    ] {
        let text = Renderer::new(RenderConfig { trace_limit: limit, ..Default::default() })
            .render_to_string(&report)
            .unwrap();
        assert_eq!(text.contains("⋮ (depth 5, continued)"), folded, "{text}");
        assert_eq!(text.matches("note: level").count(), limit, "{text}");
        assert!(text.ends_with(tail), "{text}");
        assert!(!text.contains("leaf"), "{text}");
    }
}

#[test]
fn summary_does_not_flatten_trace_diagnostics() {
    let mut report = report_cause(span("absent.watsup", 2, 1, 2, 3));
    report.children.push(super::report(Span::default()));
    let summary = report.to_string();
    assert!(summary.contains("parse/text-escape-invalid"));
    assert!(summary.contains("invalid escape in text literal"));
    assert!(!summary.contains("use a supported escape"));
    let ReportKind::Cause(cause) = &report.children[0].kind else { panic!("diagnostic preserved") };
    assert_eq!(cause.source, "parse");
    assert_eq!(cause.notes, ["use a supported escape"]);
}

#[test]
fn deep_mixed_traces_render_and_drop_on_a_small_stack() {
    std::thread::Builder::new()
        .stack_size(256 * 1024)
        .spawn(|| {
            let mut trace = frame("leaf", vec![]);
            for idx in 0..20_000 {
                trace = if idx % 2 == 0 {
                    let mut cause = report_cause(Span::default());
                    cause.children.push(trace);
                    cause
                } else {
                    frame(&format!("frame {idx}"), vec![trace])
                };
            }
            let mut report = report_cause(Span::default());
            report.children.push(trace);
            assert_eq!(report.depth_max(), 20_002);
            let mut renderer =
                Renderer::new(RenderConfig { trace_limit: 20_001, ..Default::default() });
            let text = renderer.render_to_string(&report).unwrap();
            assert!(text.contains("leaf"));
            assert!(text.contains("⋮ (depth 5, continued)"));
            assert!(!text.contains("ancestors]"));
            assert!(text.len() < 10_000_000, "deep indentation must stay bounded");
            assert!(format!("{report:?}").len() < 1_000);
            drop(report);
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn trace_limit_preserves_branches_and_the_underlying_reports() {
    let mut report = report_cause(Span::default());
    for message in ["first branch", "second branch"] {
        report.children.push(frame(message, vec![]));
    }
    let mut renderer = Renderer::new(RenderConfig { trace_limit: 1, ..Default::default() });
    let text = renderer.render_to_string(&report).unwrap();
    assert!(text.contains("first branch"));
    assert!(!text.contains("second branch"));
    assert!(text.contains("└─ ... further reports omitted (trace limit: 1)"));
    assert_eq!(report.children.len(), 2);
    let text = Renderer::new(RenderConfig::default())
        .render_to_string(&report)
        .unwrap();
    assert!(text.find("first branch").unwrap() < text.find("second branch").unwrap());
}

#[test]
fn root_frames_and_mixed_branches_preserve_depth_and_order() {
    let report = frame(
        "execution",
        vec![
            frame("call", vec![failure("argument", vec![failure("type", vec![])])]),
            failure("alternative", vec![]),
        ],
    );
    let text = Renderer::new(RenderConfig::default())
        .render_to_string(&report)
        .unwrap();
    assert_eq!(
        text,
        concat!(
            "note: execution\n\n",
            "├─ note: call\n│\n",
            "│  └─ error[test/failure]: argument\n│\n",
            "│     └─ error[test/failure]: type\n│\n",
            "└─ error[test/failure]: alternative\n\n",
        )
    );
    assert_eq!(report.to_string(), "note: execution");
}

#[test]
fn cause_roots_preserve_nested_metadata_and_source_snippets() {
    let mut report = report_cause(span("root", 1, 0, 1, 4));
    let mut cause_inner = super::report(span("cause", 1, 0, 1, 5));
    let ReportKind::Cause(diagnostic) = &mut cause_inner.kind else { unreachable!() };
    diagnostic.severity = Severity::Warning;
    diagnostic.code = None;
    diagnostic.source = "nested";
    let frame_inner = Report {
        kind: ReportKind::Frame {
            span: span("frame", 1, 0, 1, 5),
            message: "trying candidate".to_owned(),
        },
        children: vec![cause_inner],
    };
    report.children = vec![frame_inner, frame("next candidate", vec![])];
    let mut renderer = Renderer::new(RenderConfig::default());
    renderer.insert_source("root", "root");
    renderer.insert_source("frame", "frame");
    renderer.insert_source("cause", "cause");
    let text = renderer.render_to_string(&report).unwrap();
    assert_eq!(
        text,
        concat!(
            "error[parse/text-escape-invalid]: invalid escape in text literal\n",
            "  ┌─ root:1:1\n",
            "  │\n",
            "1 │ root\n",
            "  │ ^^^^ invalid escape\n",
            "  │\n",
            "  = use a supported escape\n\n",
            "├─ note: trying candidate\n",
            "│    ┌─ frame:1:1\n",
            "│    │\n",
            "│  1 │ frame\n",
            "│    │ -----\n",
            "│\n",
            "│  └─ warning: invalid escape in text literal\n",
            "│       ┌─ cause:1:1\n",
            "│       │\n",
            "│     1 │ cause\n",
            "│       │ ^^^^^ invalid escape\n",
            "│       │\n",
            "│       = use a supported escape\n",
            "│       = source: nested\n",
            "│\n",
            "└─ note: next candidate\n\n",
        )
    );

    // The same frame retains its snippet when promoted to the root
    let text = renderer.render_to_string(&report.children[0]).unwrap();
    assert!(text.starts_with("note: trying candidate"), "{text}");
    assert!(text.contains("frame:1:1"), "{text}");
    assert!(text.contains("1 │ frame"), "{text}");
    assert!(text.contains("└─ warning:"), "{text}");
}

#[test]
fn zero_trace_budget_keeps_either_root_kind_and_all_stored_children() {
    for report in [
        frame("root", vec![failure("child", vec![])]),
        failure("root", vec![frame("child", vec![])]),
    ] {
        let text = Renderer::new(RenderConfig { trace_limit: 0, ..Default::default() })
            .render_to_string(&report)
            .unwrap();
        assert!(text.starts_with(&report.to_string()), "{text}");
        assert!(text.contains("└─ ... further reports omitted (trace limit: 0)"), "{text}");
        assert!(!text.contains("child"), "{text}");
        assert_eq!(report.children.len(), 1);
    }
    let report = frame("root", vec![]);
    let text = Renderer::new(RenderConfig { trace_limit: 0, ..Default::default() })
        .render_to_string(&report)
        .unwrap();
    assert_eq!(text, "note: root\n\n");
}

#[test]
fn truncation_closes_each_unfinished_branch() {
    let report = frame(
        "root",
        vec![
            frame("parent", vec![failure("shown", vec![]), failure("hidden", vec![])]),
            failure("sibling", vec![]),
        ],
    );
    let text = Renderer::new(RenderConfig { trace_limit: 2, ..Default::default() })
        .render_to_string(&report)
        .unwrap();
    assert_eq!(
        text,
        concat!(
            "note: root\n\n",
            "├─ note: parent\n│\n",
            "│  ├─ error[test/failure]: shown\n│  │\n",
            "│  └─ ... further reports omitted (trace limit: 2)\n",
            "└─ ... further reports omitted (trace limit: 2)\n",
        )
    );
    assert_eq!(report.children.len(), 2);
    assert_eq!(report.children[0].children.len(), 2);
}

#[test]
fn ascii_snippets_use_ascii_tree_connections() {
    let report = frame("root", vec![frame("first", vec![]), failure("last", vec![])]);
    let mut config = RenderConfig::default();
    config.snippet.chars = codespan_reporting::term::Chars::ascii();
    let text = Renderer::new(config).render_to_string(&report).unwrap();
    assert_eq!(
        text,
        concat!("note: root\n\n", "|- note: first\n|\n", "`- error[test/failure]: last\n\n",)
    );
}

#[test]
fn multiline_messages_and_notes_stay_on_their_branch() {
    let mut report_inner = failure("first line\nsecond line", vec![]);
    let ReportKind::Cause(diagnostic) = &mut report_inner.kind else { unreachable!() };
    diagnostic
        .notes
        .push("first note\ncontinued note".to_owned());
    let report = frame("root", vec![report_inner, frame("last", vec![])]);
    let text = Renderer::new(RenderConfig::default())
        .render_to_string(&report)
        .unwrap();
    assert!(text.contains("├─ error[test/failure]: first line"), "{text}");
    assert!(text.contains("│  second line"), "{text}");
    let section = text.split("└─ note: last").next().unwrap();
    for line in section.lines().skip(3) {
        assert!(line.starts_with('│'), "disconnected line: {line:?}\n{text}");
    }
    assert!(section.contains("first note"), "{text}");
    assert!(section.contains("continued note"), "{text}");
}
