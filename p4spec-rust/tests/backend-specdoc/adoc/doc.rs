use p4spec_rust::backend_specdoc::adoc::pl::doc::{
    doc::{Block, Code, FallthroughLabel, Item, ItemKind, Link, LinkKind, Prose, Subject, Table},
    serialize,
};
use p4spec_rust::backend_specdoc::anchor::AnchorContext;
use p4spec_rust::lang::common::source::Span;

#[test]
fn code_links_merge_adjacent_tokens_and_drop_nested_targets() {
    let code = Code::Link(
        Link { kind: LinkKind::Direct("outer".into()), origin: None },
        Box::new(Code::Seq(vec![
            Code::Token("a ".into()),
            Code::Link(
                Link { kind: LinkKind::Direct("inner".into()), origin: None },
                Box::new(Code::Token("b".into())),
            ),
        ])),
    );
    assert_eq!(
        serialize::ser_prose(
            &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
            &Span::default(),
            &mut Vec::new(),
            &Prose::Code(code)
        ),
        "xref:outer[``a`` ``b``]"
    );
}

#[test]
fn unresolved_subject_keeps_body_without_cross_reference() {
    let prose = Prose::Link(
        Link { kind: LinkKind::Subject(Subject::Function("f".into())), origin: None },
        Box::new(Prose::Text("call".into())),
    );
    assert_eq!(
        serialize::ser_prose(&AnchorContext::default(), &Span::default(), &mut Vec::new(), &prose),
        "call"
    );
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
        &Span::default(),
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
            &Span::default(),
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
            &Span::default(),
            &mut Vec::new(),
            &prose.capitalize_first()
        ),
        "``x`` stays"
    );
}

#[test]
fn link_delimiters_and_quoted_code_preserve_literal_content() {
    let prose = Prose::Link(
        Link { kind: LinkKind::Direct("target".into()), origin: None },
        Box::new(Prose::Text("a[b]".into())),
    );
    assert_eq!(
        serialize::ser_prose(
            &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
            &Span::default(),
            &mut Vec::new(),
            &prose
        ),
        "<<target,a[b]>>"
    );
    let prose = Prose::Code(Code::Token("\"a\" \"b\"".into()));
    assert_eq!(
        serialize::ser_prose(
            &AnchorContext::new(&|_, id| Some(id.to_owned()), &|_, id| Some(id.to_owned())),
            &Span::default(),
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
            &Span::default(),
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
        frontend::parse::parse_text,
    };
    let spec_el = parse_text("links.watsup".into(), "def $f = true").unwrap();
    let span = &spec_el[0].span;
    let prose = Prose::Seq(vec![
        Prose::link(
            Link { kind: LinkKind::Direct(String::new()), origin: None },
            Prose::text("empty"),
        ),
        Prose::link(
            Link { kind: LinkKind::Direct(String::new()), origin: None },
            Prose::text("again"),
        ),
        Prose::link(
            Link { kind: LinkKind::Direct("outer".into()), origin: None },
            Prose::link(
                Link { kind: LinkKind::Direct("inner".into()), origin: None },
                Prose::text("nested"),
            ),
        ),
        Prose::link(Link { kind: LinkKind::Direct("body".into()), origin: None }, Prose::Empty),
        Prose::link(
            Link { kind: LinkKind::Direct("label".into()), origin: None },
            Prose::text("[a]<b>"),
        ),
    ]);
    let anchor_ctx = AnchorContext::default();
    let mut warnings = Vec::new();
    let text = serialize::ser_prose(&anchor_ctx, span, &mut warnings, &prose);
    assert_eq!(text, "xref:[empty]xref:[again]xref:outer[nested]xref:body[][a]<b>");
    let codes = [
        "adoc/link-target-empty",
        "adoc/link-nested",
        "adoc/link-body-empty",
        "adoc/link-text-invalid",
    ];
    assert_eq!(warnings.len(), codes.len());
    for (report, code) in warnings.iter().zip(codes) {
        let ReportKind::Cause(diagnostic) = &report.kind else { panic!("warning cause") };
        assert_eq!(diagnostic.source, "adoc");
        assert_eq!(diagnostic.severity, Severity::Warning);
        assert_eq!(diagnostic.code.as_deref(), Some(code));
        assert_eq!(diagnostic.labels.len(), 1);
        assert_eq!(&diagnostic.labels[0].span, span);
        assert!(report.children.is_empty());
        if code == "adoc/link-text-invalid" {
            assert!(diagnostic.notes.iter().any(|note| note.contains("[a]<b>")));
        }
    }
    // Each serialization retains its own duplicate-warning scope
    serialize::ser_prose(&anchor_ctx, span, &mut warnings, &prose);
    assert_eq!(warnings.len(), 8);
}

#[test]
fn link_warnings_preserve_origins_without_line_coordinates() {
    use p4spec_rust::{
        diagnostic::{LabelStyle, RenderConfig, Renderer, ReportKind},
        lang::common::source::Position,
    };

    let span =
        Span::new(Position::new("fragment.watsup", 1, 0), Position::new("fragment.watsup", 1, 1));
    let span_outer =
        Span::new(Position::new("template.watsup", 0, 0), Position::new("template.watsup", 0, 0));
    let span_inner = Span::default();
    let prose = Prose::link(
        Link {
            kind: LinkKind::Direct("outer".into()),
            origin: Some(p4spec_rust::phrase! {
                node: "prose_in".to_owned(), span: span_outer.clone(),
            }),
        },
        Prose::link(
            Link {
                kind: LinkKind::Direct("inner".into()),
                origin: Some(p4spec_rust::phrase! {
                    node: "prose".to_owned(), span: span_inner.clone(),
                }),
            },
            Prose::text("body"),
        ),
    );
    let mut warnings = Vec::new();
    let text = serialize::ser_prose(&AnchorContext::default(), &span, &mut warnings, &prose);
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
    assert!(rendered.contains("at generated source: `prose`: this inner link is suppressed"));
    assert!(rendered.contains("related location at template.watsup: `prose_in`"));
    assert!(!rendered.contains("fragment.watsup"));
}

#[test]
fn code_warnings_and_table_lint_policy_remain_distinct() {
    let code = Code::Link(
        Link { kind: LinkKind::Direct("outer".into()), origin: None },
        Box::new(Code::Link(
            Link { kind: LinkKind::Direct("inner".into()), origin: None },
            Box::new(Code::Token("x".into())),
        )),
    );
    let anchor_ctx = AnchorContext::default();
    let mut warnings = Vec::new();
    let span = Span::default();
    serialize::ser_prose(&anchor_ctx, &span, &mut warnings, &Prose::Code(code.clone()));
    assert_eq!(warnings.len(), 1);
    let p4spec_rust::diagnostic::ReportKind::Cause(diagnostic) = &warnings[0].kind else {
        panic!("warning cause")
    };
    assert_eq!(diagnostic.labels.len(), 1);
    assert_eq!(diagnostic.labels[0].span, span);
    warnings.clear();
    serialize::ser_block(
        &anchor_ctx,
        &span,
        &mut warnings,
        &Block::Table(Table { header: vec![Prose::text("header")], rows: vec![vec![code]] }),
    );
    assert!(warnings.is_empty());
    // Delimiter failures remain visible even in table cells with lint disabled
    serialize::ser_code(
        &anchor_ctx,
        &span,
        &mut warnings,
        &Code::Link(
            Link { kind: LinkKind::Direct("label".into()), origin: None },
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
