use p4spec_rust::backend_doc::adoc::pl::doc::{
    doc::{Block, Code, FallthroughLabel, Item, ItemKind, Link, Prose, Subject, Table},
    serialize,
};

#[test]
fn code_links_merge_adjacent_tokens_and_drop_nested_targets() {
    let code = Code::Link(
        Link::Direct("outer".into()),
        Box::new(Code::Seq(vec![
            Code::Token("a ".into()),
            Code::Link(Link::Direct("inner".into()), Box::new(Code::Token("b".into()))),
        ])),
    );
    assert_eq!(
        serialize::ser_prose(&serialize::subject_name, &Prose::Code(code)),
        "xref:outer[``a`` ``b``]"
    );
}

#[test]
fn unresolved_subject_keeps_body_without_cross_reference() {
    let prose = Prose::Link(
        Link::Subject(Subject::Function("f".into())),
        Box::new(Prose::Text("call".into())),
    );
    assert_eq!(serialize::ser_prose(&|_| None, &prose), "call");
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
    let text = serialize::ser_block(&serialize::subject_name, &block);
    assert!(text.contains("[<a href=\"#two\">→ b</a>]"), "{text}");
    assert!(
        text.contains(" .. +++<span class=\"bk-arm-anchor\" id=\"two\"></span>+++Done"),
        "{text}"
    );
}

#[test]
fn capitalization_stops_at_code_and_reaches_text_after_empty_nodes() {
    let prose = Prose::Seq(vec![Prose::Empty, Prose::Text("hello".into())]);
    assert_eq!(serialize::ser_prose(&serialize::subject_name, &prose.capitalize_first()), "Hello");
    let prose =
        Prose::Seq(vec![Prose::Code(Code::Token("x".into())), Prose::Text(" stays".into())]);
    assert_eq!(
        serialize::ser_prose(&serialize::subject_name, &prose.capitalize_first()),
        "``x`` stays"
    );
}

#[test]
fn link_delimiters_and_quoted_code_preserve_literal_content() {
    let prose = Prose::Link(Link::Direct("target".into()), Box::new(Prose::Text("a[b]".into())));
    assert_eq!(serialize::ser_prose(&serialize::subject_name, &prose), "<<target,a[b]>>");
    let prose = Prose::Code(Code::Token("\"a\" \"b\"".into()));
    assert_eq!(
        serialize::ser_prose(&serialize::subject_name, &prose),
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
        serialize::ser_block(&serialize::subject_name, &block),
        "[cols=\"2\", options=\"header\"]\n|===\n| Input | Output \n\n| a | b\n\n|==="
    );
}
