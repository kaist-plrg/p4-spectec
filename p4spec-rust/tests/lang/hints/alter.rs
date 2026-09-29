use super::*;
use p4spec_rust::lang::hints::alter::AlterHintKind;

fn alter_hint(node: AlterHintKind) -> AlterHint {
    p4spec_rust::phrase! { node: node, span: span("exp") }
}

#[test]
fn test_alter_validates_sequential_and_numbered_holes() {
    let hint = alter_hint(AlterHintKind::Seq(vec![
        alter_hint(AlterHintKind::Hole(AlterHole::Next)),
        alter_hint(AlterHintKind::Brack(
            atom("L"),
            Box::new(alter_hint(AlterHintKind::Fuse(
                Box::new(alter_hint(AlterHintKind::Hole(AlterHole::Num(2)))),
                Box::new(alter_hint(AlterHintKind::Hole(AlterHole::Next))),
            ))),
            atom("R"),
        )),
    ]));

    assert_eq!(alter_impl::validate(&hint, 3), Ok(()));
    assert_eq!(
        alter_impl::validate(&hint, 1),
        Err(AlterationError::IndexOutOfBounds {
            hole: Box::new(p4spec_rust::phrase! { node: AlterHole::Num(2), span: span("exp") }),
            index: 2,
            item_count: 1
        })
    );
    assert_eq!(
        alter_impl::validate(
            &alter_hint(AlterHintKind::Seq(vec![
                alter_hint(AlterHintKind::Hole(AlterHole::Next)),
                alter_hint(AlterHintKind::Hole(AlterHole::Next)),
            ])),
            1,
        ),
        Err(AlterationError::IndexOutOfBounds {
            hole: Box::new(p4spec_rust::phrase! { node: AlterHole::Next, span: span("exp") }),
            index: 1,
            item_count: 1
        })
    );
}

#[test]
fn test_alter_realigns_outputs_around_noncontiguous_inputs() {
    let hint = alter_hint(AlterHintKind::Brack(
        atom("L"),
        Box::new(alter_hint(AlterHintKind::Fuse(
            Box::new(alter_hint(AlterHintKind::Hole(AlterHole::Num(3)))),
            Box::new(alter_hint(AlterHintKind::Seq(vec![
                alter_hint(AlterHintKind::Hole(AlterHole::Num(1))),
                alter_hint(AlterHintKind::Hole(AlterHole::Num(3))),
            ]))),
        ))),
        atom("R"),
    ));

    assert_eq!(
        alter_impl::realign(
            &hint,
            &InputHint::new(vec![
                p4spec_rust::phrase!(node: 0, span: span("exp")),
                p4spec_rust::phrase!(node: 2, span: span("exp"))
            ])
        ),
        alter_hint(AlterHintKind::Brack(
            atom("L"),
            Box::new(alter_hint(AlterHintKind::Fuse(
                Box::new(alter_hint(AlterHintKind::Hole(AlterHole::Num(1)))),
                Box::new(alter_hint(AlterHintKind::Seq(vec![
                    alter_hint(AlterHintKind::Hole(AlterHole::Num(0))),
                    alter_hint(AlterHintKind::Hole(AlterHole::Num(1))),
                ]))),
            ))),
            atom("R"),
        ))
    );
}

#[test]
fn test_alter_alternates_with_omission_defaults_fuse_brackets_and_other() {
    let hint = alter_hint(AlterHintKind::Seq(vec![
        alter_hint(AlterHintKind::Text("omit".into())),
        alter_hint(AlterHintKind::Brack(
            atom("L"),
            Box::new(alter_hint(AlterHintKind::Hole(AlterHole::Next))),
            atom("R"),
        )),
        alter_hint(AlterHintKind::Fuse(
            Box::new(alter_hint(AlterHintKind::Hole(AlterHole::Num(1)))),
            Box::new(alter_hint(AlterHintKind::Other(exp(ExpKind::Text("other".into()))))),
        )),
    ]));
    let result = alter_impl::alternate(
        &hint,
        &["zero", "one"],
        &StringRenderer { empty: "_", separator: " ", fuse: "#" },
    )
    .unwrap();
    assert_eq!(result, "_ L zero R one#\"other\"");
    assert_eq!(
        alter_impl::alternate(
            &alter_hint(AlterHintKind::Hole(AlterHole::Num(2))),
            &["zero"],
            &StringRenderer { empty: "", separator: "", fuse: "" }
        )
        .unwrap_err(),
        AlterationError::IndexOutOfBounds {
            hole: Box::new(p4spec_rust::phrase! { node: AlterHole::Num(2), span: span("exp") }),
            index: 2,
            item_count: 1
        }
    );
}
#[test]
fn test_alter_edge_cases_cover_init_omission_duplicates_and_next_cursor() {
    assert!(matches!(
        alter_impl::init(&exp(ExpKind::Atom(atom("A")))).node,
        AlterHintKind::Atom(_)
    ));
    assert_eq!(
        alter_impl::init(&exp(ExpKind::Seq(Vec::new()))),
        alter_hint(AlterHintKind::Seq(Vec::new()))
    );
    let nested = exp(ExpKind::Seq(vec![exp(ExpKind::Brack(
        atom("L"),
        Box::new(exp(ExpKind::Hole(Hole::Rest))),
        atom("R"),
    ))]));
    assert_eq!(
        alter_impl::init(&nested),
        alter_hint(AlterHintKind::Seq(vec![alter_hint(AlterHintKind::Brack(
            atom("L"),
            Box::new(alter_hint(AlterHintKind::Other(exp(ExpKind::Hole(Hole::Rest))))),
            atom("R"),
        ))]))
    );
    let omitted = alter_hint(AlterHintKind::Brack(
        atom("L"),
        Box::new(alter_hint(AlterHintKind::Text("omit".into()))),
        atom("R"),
    ));
    let rendered = alter_impl::alternate(
        &omitted,
        &[] as &[&str],
        &StringRenderer { empty: "EMPTY", separator: "|", fuse: "" },
    )
    .unwrap();
    assert_eq!(rendered, "L|R");
    let nexts = alter_hint(AlterHintKind::Seq(vec![
        alter_hint(AlterHintKind::Hole(AlterHole::Next)),
        alter_hint(AlterHintKind::Hole(AlterHole::Next)),
    ]));

    assert_eq!(
        alter_impl::alternate(
            &nexts,
            &["a"],
            &StringRenderer { empty: "", separator: "", fuse: "" }
        )
        .unwrap_err(),
        AlterationError::IndexOutOfBounds {
            hole: Box::new(p4spec_rust::phrase! { node: AlterHole::Next, span: span("exp") }),
            index: 1,
            item_count: 1
        }
    );
}

#[test]
fn test_placeholder_locations_survive_initialization_and_realignment() {
    let span_hole = Span::new(Position::new("hint", 3, 7), Position::new("hint", 3, 9));
    let exp_hint =
        p4spec_rust::phrase! { node: ExpKind::Hole(Hole::Num(3)), span: span_hole.clone() };
    let hint = alter_impl::init(&exp_hint);
    let error = alter_impl::validate(&hint, 2).unwrap_err();
    let AlterationError::IndexOutOfBounds { hole, index, item_count } = error;
    assert_eq!(hole.span, span_hole);
    assert_eq!((index, item_count), (3, 2));
    let hint = alter_impl::realign(
        &hint,
        &InputHint::new(vec![p4spec_rust::phrase! { node: 0, span: span("exp") }]),
    );
    let AlterHintKind::Hole(hole) = &hint.node else { panic!("expected placeholder") };
    assert_eq!(hint.span, span_hole);
    assert_eq!(*hole, AlterHole::Num(0));
    let error = alter_impl::alternate(
        &hint,
        &[] as &[&str],
        &StringRenderer { empty: "", separator: "", fuse: "" },
    )
    .unwrap_err();
    let AlterationError::IndexOutOfBounds { hole, .. } = error;
    assert_eq!(hole.span, span_hole);
}

#[test]
fn test_template_span_survives_realignment() {
    let exp_hint = p4spec_rust::phrase! {
        node: ExpKind::Seq(vec![exp(ExpKind::Text("result".into())), exp(ExpKind::Hole(Hole::Num(2)))]),
        span: span("template"),
    };
    let hint = alter_impl::init(&exp_hint);
    let hint_realigned = alter_impl::realign(&hint, &InputHint::new(vec![]));
    assert_eq!(hint.span, span("template"));
    assert_eq!(hint_realigned.span, span("template"));
}
