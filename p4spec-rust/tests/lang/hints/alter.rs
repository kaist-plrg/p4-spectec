use super::*;

#[test]
fn test_alter_validates_sequential_and_numbered_holes() {
    let hint = AlterationHint::Seq(vec![
        AlterationHint::Hole(
            p4spec_rust::phrase! { node: AlterHole::Next, span: Default::default() },
        ),
        AlterationHint::Brack(
            atom("L"),
            Box::new(AlterationHint::Fuse(
                Box::new(AlterationHint::Hole(
                    p4spec_rust::phrase! { node: AlterHole::Num(2), span: Default::default() },
                )),
                Box::new(AlterationHint::Hole(
                    p4spec_rust::phrase! { node: AlterHole::Next, span: Default::default() },
                )),
            )),
            atom("R"),
        ),
    ]);

    assert_eq!(alter_impl::validate(&hint, 3), Ok(()));
    assert_eq!(
        alter_impl::validate(&hint, 1),
        Err(AlterationError::IndexOutOfBounds {
            hole: Box::new(
                p4spec_rust::phrase! { node: AlterHole::Num(2), span: Default::default() }
            ),
            index: 2,
            item_count: 1
        })
    );
    assert_eq!(
        alter_impl::validate(
            &AlterationHint::Seq(vec![
                AlterationHint::Hole(
                    p4spec_rust::phrase! { node: AlterHole::Next, span: Default::default() }
                ),
                AlterationHint::Hole(
                    p4spec_rust::phrase! { node: AlterHole::Next, span: Default::default() }
                ),
            ]),
            1,
        ),
        Err(AlterationError::IndexOutOfBounds {
            hole: Box::new(
                p4spec_rust::phrase! { node: AlterHole::Next, span: Default::default() }
            ),
            index: 1,
            item_count: 1
        })
    );
}

#[test]
fn test_alter_realigns_outputs_around_noncontiguous_inputs() {
    let hint = AlterationHint::Brack(
        atom("L"),
        Box::new(AlterationHint::Fuse(
            Box::new(AlterationHint::Hole(
                p4spec_rust::phrase! { node: AlterHole::Num(3), span: Default::default() },
            )),
            Box::new(AlterationHint::Seq(vec![
                AlterationHint::Hole(
                    p4spec_rust::phrase! { node: AlterHole::Num(1), span: Default::default() },
                ),
                AlterationHint::Hole(
                    p4spec_rust::phrase! { node: AlterHole::Num(3), span: Default::default() },
                ),
            ])),
        )),
        atom("R"),
    );

    assert_eq!(
        alter_impl::realign(
            &hint,
            &InputHint::new(vec![
                p4spec_rust::phrase!(node: 0, span: Default::default()),
                p4spec_rust::phrase!(node: 2, span: Default::default())
            ])
        ),
        AlterationHint::Brack(
            atom("L"),
            Box::new(AlterationHint::Fuse(
                Box::new(AlterationHint::Hole(
                    p4spec_rust::phrase! { node: AlterHole::Num(1), span: Default::default() }
                )),
                Box::new(AlterationHint::Seq(vec![
                    AlterationHint::Hole(
                        p4spec_rust::phrase! { node: AlterHole::Num(0), span: Default::default() }
                    ),
                    AlterationHint::Hole(
                        p4spec_rust::phrase! { node: AlterHole::Num(1), span: Default::default() }
                    ),
                ])),
            )),
            atom("R"),
        )
    );
}

#[test]
fn test_alter_alternates_with_omission_defaults_fuse_brackets_and_other() {
    let hint = AlterationHint::Seq(vec![
        AlterationHint::Text("omit".into()),
        AlterationHint::Brack(
            atom("L"),
            Box::new(AlterationHint::Hole(
                p4spec_rust::phrase! { node: AlterHole::Next, span: Default::default() },
            )),
            atom("R"),
        ),
        AlterationHint::Fuse(
            Box::new(AlterationHint::Hole(
                p4spec_rust::phrase! { node: AlterHole::Num(1), span: Default::default() },
            )),
            Box::new(AlterationHint::Other(exp(ExpKind::Text("other".into())))),
        ),
    ]);
    let result = alter_impl::alternate(
        &hint,
        &["zero", "one"],
        &StringRenderer { empty: "_", separator: " ", fuse: "#" },
    )
    .unwrap();
    assert_eq!(result, "_ L zero R one#\"other\"");
    assert_eq!(
        alter_impl::alternate(
            &AlterationHint::Hole(
                p4spec_rust::phrase! { node: AlterHole::Num(2), span: Default::default() }
            ),
            &["zero"],
            &StringRenderer { empty: "", separator: "", fuse: "" }
        )
        .unwrap_err(),
        AlterationError::IndexOutOfBounds {
            hole: Box::new(
                p4spec_rust::phrase! { node: AlterHole::Num(2), span: Default::default() }
            ),
            index: 2,
            item_count: 1
        }
    );
}
#[test]
fn test_alter_edge_cases_cover_init_omission_duplicates_and_next_cursor() {
    assert!(matches!(alter_impl::init(&exp(ExpKind::Atom(atom("A")))), AlterationHint::Atom(_)));
    assert_eq!(alter_impl::init(&exp(ExpKind::Seq(Vec::new()))), AlterationHint::Seq(Vec::new()));
    let nested = exp(ExpKind::Seq(vec![exp(ExpKind::Brack(
        atom("L"),
        Box::new(exp(ExpKind::Hole(Hole::Rest))),
        atom("R"),
    ))]));
    assert_eq!(
        alter_impl::init(&nested),
        AlterationHint::Seq(vec![AlterationHint::Brack(
            atom("L"),
            Box::new(AlterationHint::Other(exp(ExpKind::Hole(Hole::Rest)))),
            atom("R"),
        )])
    );
    let omitted =
        AlterationHint::Brack(atom("L"), Box::new(AlterationHint::Text("omit".into())), atom("R"));
    let rendered = alter_impl::alternate(
        &omitted,
        &[] as &[&str],
        &StringRenderer { empty: "EMPTY", separator: "|", fuse: "" },
    )
    .unwrap();
    assert_eq!(rendered, "L|R");
    let nexts = AlterationHint::Seq(vec![
        AlterationHint::Hole(
            p4spec_rust::phrase! { node: AlterHole::Next, span: Default::default() },
        ),
        AlterationHint::Hole(
            p4spec_rust::phrase! { node: AlterHole::Next, span: Default::default() },
        ),
    ]);

    assert_eq!(
        alter_impl::alternate(
            &nexts,
            &["a"],
            &StringRenderer { empty: "", separator: "", fuse: "" }
        )
        .unwrap_err(),
        AlterationError::IndexOutOfBounds {
            hole: Box::new(
                p4spec_rust::phrase! { node: AlterHole::Next, span: Default::default() }
            ),
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
        &InputHint::new(vec![p4spec_rust::phrase! { node: 0, span: Default::default() }]),
    );
    let AlterationHint::Hole(hole) = &hint else { panic!("expected placeholder") };
    assert_eq!(hole.span, span_hole);
    assert_eq!(hole.node, AlterHole::Num(0));
    let error = alter_impl::alternate(
        &hint,
        &[] as &[&str],
        &StringRenderer { empty: "", separator: "", fuse: "" },
    )
    .unwrap_err();
    let AlterationError::IndexOutOfBounds { hole, .. } = error;
    assert_eq!(hole.span, span_hole);
}
