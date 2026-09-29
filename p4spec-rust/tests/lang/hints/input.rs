use super::*;

#[test]
fn test_input_hints_validate_and_preserve_split_order() {
    let sequence =
        exp(ExpKind::Seq(vec![exp(ExpKind::Hole(Hole::Num(2))), exp(ExpKind::Hole(Hole::Num(0)))]));
    assert_eq!(
        input_impl::init(&sequence),
        Some(InputHint::new(vec![
            p4spec_rust::phrase!(node: 2, span: Default::default()),
            p4spec_rust::phrase!(node: 0, span: Default::default())
        ]))
    );
    assert_eq!(input_impl::validate(&InputHint::new(vec![]), 3), Err(InputError::Empty));
    assert_eq!(
        input_impl::validate(
            &InputHint::new(vec![
                p4spec_rust::phrase!(node: 1, span: Default::default()),
                p4spec_rust::phrase!(node: 1, span: Default::default())
            ]),
            3
        ),
        Err(InputError::DuplicateIndex {
            idx: Box::new(p4spec_rust::phrase!(node: 1, span: Default::default())),
            idx_previous: Box::new(p4spec_rust::phrase!(node: 1, span: Default::default()))
        })
    );
    assert_eq!(
        input_impl::validate(
            &InputHint::new(vec![p4spec_rust::phrase!(node: 3, span: Default::default())]),
            3
        ),
        Err(InputError::IndexOutOfBounds {
            idx: Box::new(p4spec_rust::phrase!(node: 3, span: Default::default())),
            arity: 3
        })
    );
    let hint = InputHint::new(vec![
        p4spec_rust::phrase!(node: 2, span: Default::default()),
        p4spec_rust::phrase!(node: 0, span: Default::default()),
    ]);
    assert_eq!(input_impl::validate(&hint, 3), Ok(()));

    let items = ["zero", "one", "two", "three"];
    let (items_input, items_output) = input_impl::split(&hint, items.to_vec()).unwrap();
    assert_eq!(items_input, vec!["zero", "two"]);
    assert_eq!(items_output, vec!["one", "three"]);
    assert_eq!(input_impl::combine(&hint, items_input, items_output), Ok(items.to_vec()));
    assert_eq!(
        input_impl::combine(&hint, vec!["zero"], vec!["one", "three"]),
        Err(InputError::InputCountMismatch { expected: 2, actual: 1 })
    );
    assert_eq!(
        input_impl::split(
            &InputHint::new(vec![p4spec_rust::phrase!(node: 4, span: Default::default())]),
            items.to_vec()
        ),
        Err(InputError::IndexOutOfBounds {
            idx: Box::new(p4spec_rust::phrase!(node: 4, span: Default::default())),
            arity: 4
        })
    );
    assert_eq!(
        input_impl::is_conditional(
            &InputHint::new(vec![
                p4spec_rust::phrase!(node: 0, span: Default::default()),
                p4spec_rust::phrase!(node: 1, span: Default::default())
            ]),
            &["left", "right"]
        ),
        Ok(true)
    );
    assert_eq!(
        input_impl::is_conditional(
            &InputHint::new(vec![p4spec_rust::phrase!(node: 0, span: Default::default())]),
            &["left", "right"]
        ),
        Ok(false)
    );
}

#[test]
fn test_zero_arity_default_hint_supports_operations_but_not_source_validation() {
    let hint = InputHint::new(vec![]);
    assert_eq!(input_impl::validate(&hint, 0), Err(InputError::Empty));
    assert_eq!(input_impl::split::<()>(&hint, vec![]), Ok((vec![], vec![])));
    assert_eq!(input_impl::combine::<()>(&hint, vec![], vec![]), Ok(vec![]));
    assert_eq!(input_impl::is_conditional::<()>(&hint, &[]), Ok(true));
    assert_eq!(input_impl::split(&hint, vec![0]), Err(InputError::Empty));
}

#[test]
fn test_input_hint_duplicates_take_precedence_over_bounds() {
    let hint = InputHint::new(vec![
        p4spec_rust::phrase!(node: 9, span: Default::default()),
        p4spec_rust::phrase!(node: 0, span: Default::default()),
        p4spec_rust::phrase!(node: 0, span: Default::default()),
    ]);
    assert_eq!(
        input_impl::validate(&hint, 2),
        Err(InputError::DuplicateIndex {
            idx: Box::new(p4spec_rust::phrase!(node: 0, span: Default::default())),
            idx_previous: Box::new(p4spec_rust::phrase!(node: 0, span: Default::default()))
        })
    );
}

#[test]
fn test_input_hint_preserves_element_spans_without_changing_equivalence() {
    let mut exp_a = exp(ExpKind::Hole(Hole::Num(2)));
    let mut exp_b = exp(ExpKind::Hole(Hole::Num(0)));
    exp_a.span = Span::new(Position::new("hint", 1, 11), Position::new("hint", 1, 13));
    exp_b.span = Span::new(Position::new("hint", 1, 14), Position::new("hint", 1, 16));
    let exp_hint = exp(ExpKind::Seq(vec![exp_a.clone(), exp_b.clone()]));
    let hint = input_impl::init(&exp_hint).unwrap();
    assert_eq!(hint.indices()[0].span, exp_a.span);
    assert_eq!(hint.indices()[1].span, exp_b.span);
    assert_eq!(
        hint,
        InputHint::new(vec![
            p4spec_rust::phrase!(node: 2, span: Default::default()),
            p4spec_rust::phrase!(node: 0, span: Default::default())
        ])
    );
    assert_eq!(
        InputHint::new(vec![
            p4spec_rust::phrase!(node: 2, span: Default::default()),
            p4spec_rust::phrase!(node: 0, span: Default::default())
        ])
        .indices()[0]
            .span,
        Span::default()
    );
    assert_eq!(
        hint.into_indices()
            .iter()
            .map(|idx| idx.node)
            .collect::<Vec<_>>(),
        vec![2, 0]
    );

    let idx = p4spec_rust::phrase!(node: 2, span: exp_a.span.clone());
    let idx_repeated = p4spec_rust::phrase!(node: 2, span: exp_b.span.clone());
    let hint_repeated = InputHint::new(vec![idx.clone(), idx_repeated.clone()]);
    assert_eq!(hint_repeated.indices()[0].span, idx.span);
    assert_eq!(hint_repeated.indices()[1].span, idx_repeated.span);
    let error = input_impl::validate(&hint_repeated, 3).unwrap_err();
    assert_eq!(error.to_string(), "input hint contains duplicate index 2");
    let InputError::DuplicateIndex { idx: idx_error, idx_previous } = error else {
        panic!("expected a duplicate input index");
    };
    assert_eq!(idx_error.node, idx_repeated.node);
    assert_eq!(idx_error.span, idx_repeated.span);
    assert_eq!(idx_previous.node, idx.node);
    assert_eq!(idx_previous.span, idx.span);

    let hint = InputHint::new(vec![idx.clone()]);
    let error = input_impl::validate(&hint, 2).unwrap_err();
    assert_eq!(error.to_string(), "input hint index 2 is out of bounds for arity 2");
    let InputError::IndexOutOfBounds { idx: idx_error, arity } = error else {
        panic!("expected an out-of-bounds input index");
    };
    assert_eq!(idx_error.node, idx.node);
    assert_eq!(idx_error.span, idx.span);
    assert_eq!(arity, 2);
}
