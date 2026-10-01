use p4spec_rust::lang::common::source::Span;
use p4spec_rust::specdoc::adoc::pl::doc::{
    doc::{Block, Code, FallthroughLabel, Item, ItemKind, Link, Prose, Subject, Table},
    serialize,
};
use p4spec_rust::specdoc::anchor::AnchorContext;

#[test]
fn code_links_merge_adjacent_tokens_and_drop_nested_targets() {
    let code = Code::Link(
        Link::Direct(p4spec_rust::phrase! { node: "outer".into(), span: Span::default() }),
        Box::new(Code::Seq(vec![
            Code::Token("a ".into()),
            Code::Link(
                Link::Direct(p4spec_rust::phrase! { node: "inner".into(), span: Span::default() }),
                Box::new(Code::Token("b".into())),
            ),
        ])),
    );
    assert_eq!(
        serialize::ser_prose(
            &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
            &mut Vec::new(),
            &Prose::Code(code)
        ),
        "xref:outer[``a`` ``b``]"
    );
}

#[test]
fn adjacent_code_links_keep_their_own_delimiters() {
    let code = Code::Seq(vec![
        Code::link(
            Link::Direct(p4spec_rust::phrase! { node: "same".into(), span: Span::default() }),
            Code::Seq(vec![Code::Token("a[".into()), Code::Token("b]".into())]),
        ),
        Code::Seq(vec![Code::link(
            Link::Direct(p4spec_rust::phrase! { node: "same".into(), span: Span::default() }),
            Code::Token("<c>".into()),
        )]),
    ]);
    let anchor_ctx = AnchorContext::default();
    let mut warnings = Vec::new();
    assert_eq!(
        serialize::ser_code(&anchor_ctx, &mut warnings, &code),
        "<<same,a[b]>>xref:same[<c>]"
    );
    assert_eq!(
        serialize::ser_prose(&anchor_ctx, &mut warnings, &Prose::Code(code)),
        "<<same,``a[b]``>>xref:same[``<c>``]"
    );
    assert!(warnings.is_empty());
}

#[test]
fn adjacent_code_link_failures_keep_each_subject_span() {
    use p4spec_rust::{diagnostic::ReportKind, lang::common::source::Position};
    let span_a = Span::new(Position::new("a.watsup", 1, 0), Position::new("a.watsup", 1, 1));
    let span_b = Span::new(Position::new("b.watsup", 2, 0), Position::new("b.watsup", 2, 1));
    let code = Code::Seq(vec![
        Code::link(
            Link::Subject(Subject::Function(
                p4spec_rust::phrase! { node: "same".into(), span: span_a.clone() },
            )),
            Code::Token("[a]<b>".into()),
        ),
        Code::link(
            Link::Subject(Subject::Function(
                p4spec_rust::phrase! { node: "same".into(), span: span_b.clone() },
            )),
            Code::Token("[c]<d>".into()),
        ),
    ]);
    let mut warnings = Vec::new();
    let text = serialize::ser_code(
        &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, _| None),
        &mut warnings,
        &code,
    );
    assert_eq!(text, "[a]<b>[c]<d>");
    assert_eq!(warnings.len(), 2);
    for (report, (span, text)) in warnings
        .iter()
        .zip([(&span_a, "[a]<b>"), (&span_b, "[c]<d>")])
    {
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("warning cause") };
        assert_eq!(diagnostic.code.as_deref(), Some("adoc/link-text-invalid"));
        assert_eq!(&diagnostic.labels[0].span, span);
        assert!(
            diagnostic
                .notes
                .contains(&format!("Generated link text: {text:?}"))
        );
    }
}

#[test]
fn code_tokens_join_across_unresolved_and_empty_links() {
    let code = Code::Seq(vec![
        Code::Token("a".into()),
        Code::Seq(vec![
            Code::Empty,
            Code::link(
                Link::Subject(Subject::Function(
                    p4spec_rust::phrase! { node: "missing".into(), span: Span::default() },
                )),
                Code::Token("b".into()),
            ),
            Code::link(
                Link::Direct(p4spec_rust::phrase! { node: "empty".into(), span: Span::default() }),
                Code::Empty,
            ),
        ]),
        Code::Token("c".into()),
    ]);
    let mut warnings = Vec::new();
    let text = serialize::ser_prose(&AnchorContext::default(), &mut warnings, &Prose::Code(code));
    assert_eq!(text, "``abc``");
    assert_eq!(warnings.len(), 1);
    assert!(
        warnings[0]
            .to_string()
            .starts_with("warning[adoc/link-body-empty]")
    );
}

#[test]
fn unresolved_subject_keeps_body_without_cross_reference() {
    let prose = Prose::Link(
        Link::Subject(Subject::Function(
            p4spec_rust::phrase! { node: "f".into(), span: Span::default() },
        )),
        Box::new(Prose::Text("call".into())),
    );
    assert_eq!(serialize::ser_prose(&AnchorContext::default(), &mut Vec::new(), &prose), "call");
}

#[test]
fn fallthrough_labels_follow_nested_ordered_list_markers() {
    let block = Block::Seq(vec![Block::Item(Item {
        level: 0,
        kind: ItemKind::Ordered(None),
        prose_head: Prose::Text("Choose".into()),
        block_body: Box::new(Block::Seq(vec![
            Block::Item(Item {
                level: 1,
                kind: ItemKind::Ordered(Some("one".into())),
                prose_head: Prose::Fallthrough("two".into(), FallthroughLabel::Derived),
                block_body: Box::new(Block::Empty),
            }),
            Block::Item(Item {
                level: 1,
                kind: ItemKind::Ordered(Some("two".into())),
                prose_head: Prose::Text("Done".into()),
                block_body: Box::new(Block::Empty),
            }),
        ])),
    })]);
    let text = serialize::ser_block(
        &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
        &mut Vec::new(),
        &block,
    );
    assert!(text.contains("[<a href=\"#two\">→ b</a>]"), "{text}");
    assert!(
        text.contains(" .. +++<span class=\"bk-arm-anchor\" id=\"two\"></span>+++Done"),
        "{text}"
    );
}

#[test]
fn capitalization_stops_at_code_and_reaches_text_after_empty_nodes() {
    let prose = Prose::Seq(vec![Prose::Empty, Prose::Text("hello".into())]);
    assert_eq!(
        serialize::ser_prose(
            &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
            &mut Vec::new(),
            &prose.capitalize_first()
        ),
        "Hello"
    );
    let prose =
        Prose::Seq(vec![Prose::Code(Code::Token("x".into())), Prose::Text(" stays".into())]);
    assert_eq!(
        serialize::ser_prose(
            &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
            &mut Vec::new(),
            &prose.capitalize_first()
        ),
        "``x`` stays"
    );
}

#[test]
fn link_delimiters_and_quoted_code_preserve_literal_content() {
    let prose = Prose::Link(
        Link::Direct(p4spec_rust::phrase! { node: "target".into(), span: Span::default() }),
        Box::new(Prose::Text("a[b]".into())),
    );
    assert_eq!(
        serialize::ser_prose(
            &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
            &mut Vec::new(),
            &prose
        ),
        "<<target,a[b]>>"
    );
    let prose = Prose::Code(Code::Token("\"a\" \"b\"".into()));
    assert_eq!(
        serialize::ser_prose(
            &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
            &mut Vec::new(),
            &prose
        ),
        "``{quot}a{quot}`` ``{quot}b{quot}``"
    );
}

#[test]
fn table_serialization_keeps_header_and_cell_boundaries() {
    let block = Block::Table(Table {
        header: vec![Prose::Text("Input".into()), Prose::Text("Output".into())],
        rows: vec![vec![Code::Token("a".into()), Code::Token("b".into())]],
    });
    assert_eq!(
        serialize::ser_block(
            &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
            &mut Vec::new(),
            &block
        ),
        "[cols=\"2\", options=\"header\"]\n|===\n| Input | Output \n\n| a | b\n\n|==="
    );
}

#[test]
fn link_warnings_preserve_order_location_notes_and_deduplication() {
    use p4spec_rust::{
        diagnostic::{ReportKind, Severity},
        lang::common::source::Position,
    };
    let spans = [1, 2, 3, 4].map(|line| {
        Span::new(Position::new("links.watsup", line, 0), Position::new("links.watsup", line, 1))
    });
    let span_outer =
        Span::new(Position::new("outer.watsup", 1, 0), Position::new("outer.watsup", 1, 1));
    let prose = Prose::Seq(vec![
        Prose::link(
            Link::Direct(p4spec_rust::phrase! { node: String::new(), span: spans[0].clone() }),
            Prose::text("empty"),
        ),
        Prose::link(
            Link::Direct(p4spec_rust::phrase! { node: String::new(), span: spans[0].clone() }),
            Prose::text("again"),
        ),
        Prose::link(
            Link::Direct(p4spec_rust::phrase! { node: "outer".into(), span: span_outer.clone() }),
            Prose::link(
                Link::Direct(p4spec_rust::phrase! { node: "inner".into(), span: spans[1].clone() }),
                Prose::text("nested"),
            ),
        ),
        Prose::link(
            Link::Direct(p4spec_rust::phrase! { node: "body".into(), span: spans[2].clone() }),
            Prose::Empty,
        ),
        Prose::link(
            Link::Direct(p4spec_rust::phrase! { node: "label".into(), span: spans[3].clone() }),
            Prose::text("[a]<b>"),
        ),
    ]);
    let anchor_ctx = AnchorContext::default();
    let mut warnings = Vec::new();
    let text = serialize::ser_prose(&anchor_ctx, &mut warnings, &prose);
    assert_eq!(text, "xref:[empty]xref:[again]xref:outer[nested]xref:body[][a]<b>");
    let codes = [
        "adoc/link-target-empty",
        "adoc/link-nested",
        "adoc/link-body-empty",
        "adoc/link-text-invalid",
    ];
    assert_eq!(warnings.len(), codes.len());
    for ((report, code), span) in warnings.iter().zip(codes).zip(&spans) {
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("warning cause") };
        assert_eq!(diagnostic.source, "adoc");
        assert_eq!(diagnostic.severity, Severity::Warning);
        assert_eq!(diagnostic.code.as_deref(), Some(code));
        assert_eq!(&diagnostic.labels[0].span, span);
        assert!(report.children.is_empty());
        if code == "adoc/link-nested" {
            assert_eq!(diagnostic.labels.len(), 2);
            assert_eq!(diagnostic.labels[1].span, span_outer);
        } else {
            assert_eq!(diagnostic.labels.len(), 1);
        }
        if code == "adoc/link-text-invalid" {
            assert!(diagnostic.notes.iter().any(|note| note.contains("[a]<b>")));
        }
    }
    // Each serialization retains its own duplicate-warning scope
    serialize::ser_prose(&anchor_ctx, &mut warnings, &prose);
    assert_eq!(warnings.len(), 8);
}

#[test]
fn link_warnings_preserve_subject_spans_without_line_coordinates() {
    use p4spec_rust::{
        diagnostic::{LabelStyle, RenderConfig, Renderer, ReportKind},
        lang::common::source::Position,
    };

    let span_outer = Span::new(
        Position::new("declaration.watsup", 0, 0),
        Position::new("declaration.watsup", 0, 0),
    );
    let span_inner = Span::default();
    let prose = Prose::link(
        Link::Subject(Subject::Function(
            p4spec_rust::phrase! { node: "outer".into(), span: span_outer.clone() },
        )),
        Prose::link(
            Link::Subject(Subject::Function(
                p4spec_rust::phrase! { node: "inner".into(), span: span_inner.clone() },
            )),
            Prose::text("body"),
        ),
    );
    let mut warnings = Vec::new();
    let text = serialize::ser_prose(
        &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, _| None),
        &mut warnings,
        &prose,
    );
    assert_eq!(text, "xref:outer[body]");
    assert_eq!(warnings.len(), 1);
    let ReportKind::Cause(diagnostic) = &warnings[0].kind else { panic!("warning cause") };
    assert_eq!(diagnostic.code.as_deref(), Some("adoc/link-nested"));
    assert_eq!(diagnostic.labels.len(), 2);
    assert_eq!(diagnostic.labels[0].style, LabelStyle::Primary);
    assert_eq!(diagnostic.labels[0].span, span_inner);
    assert_eq!(diagnostic.labels[1].style, LabelStyle::Secondary);
    assert_eq!(diagnostic.labels[1].span, span_outer);

    let rendered = Renderer::new(RenderConfig::default())
        .render_to_string(&warnings[0])
        .unwrap();
    assert!(rendered.contains("at generated source: this inner link is suppressed"));
    assert!(rendered.contains(
        "related location at declaration.watsup: this supplies the display text for the outer link"
    ));
}

#[test]
fn code_warnings_and_table_lint_policy_remain_distinct() {
    let code = Code::Link(
        Link::Direct(p4spec_rust::phrase! { node: "outer".into(), span: Span::default() }),
        Box::new(Code::Link(
            Link::Direct(p4spec_rust::phrase! { node: "inner".into(), span: Span::default() }),
            Box::new(Code::Token("x".into())),
        )),
    );
    let anchor_ctx = AnchorContext::default();
    let mut warnings = Vec::new();
    serialize::ser_prose(&anchor_ctx, &mut warnings, &Prose::Code(code.clone()));
    assert_eq!(warnings.len(), 1);
    let p4spec_rust::diagnostic::ReportKind::Cause(diagnostic) = &warnings[0].kind else {
        panic!("warning cause")
    };
    assert_eq!(diagnostic.labels.len(), 2);
    assert_eq!(diagnostic.labels[0].span, Span::default());
    assert_eq!(diagnostic.labels[1].span, Span::default());
    warnings.clear();
    serialize::ser_block(
        &anchor_ctx,
        &mut warnings,
        &Block::Table(Table { header: vec![Prose::text("header")], rows: vec![vec![code]] }),
    );
    assert!(warnings.is_empty());
    // Delimiter failures remain visible even in table cells with lint disabled
    serialize::ser_code(
        &anchor_ctx,
        &mut warnings,
        &Code::Link(
            Link::Direct(p4spec_rust::phrase! { node: "label".into(), span: Span::default() }),
            Box::new(Code::Token("[a]<b>".into())),
        ),
    );
    assert_eq!(warnings.len(), 1);
    assert!(
        warnings[0]
            .to_string()
            .starts_with("warning[adoc/link-text-invalid]")
    );
}
