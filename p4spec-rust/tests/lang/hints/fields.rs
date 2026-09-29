use super::*;

#[test]
fn test_fields_initialize_only_text_and_require_exact_arity() {
    let hint_exp = exp(ExpKind::Seq(vec![
        exp(ExpKind::Text("left".to_owned())),
        exp(ExpKind::Text("right".to_owned())),
    ]));
    let hint: FieldHint = p4spec_rust::phrase! { node: vec![
        p4spec_rust::phrase! { node: "left".to_owned(), span: span("exp") },
        p4spec_rust::phrase! { node: "right".to_owned(), span: span("exp") },
    ], span: span("exp") };

    assert_eq!(fields_impl::init(&hint_exp), Ok(hint.clone()));
    assert_eq!(
        fields_impl::init(&exp(ExpKind::Text("field".to_owned()))),
        Ok(p4spec_rust::phrase! { node: vec![
            p4spec_rust::phrase! { node: "field".to_owned(), span: span("exp") }
        ], span: span("exp") })
    );
    assert!(fields_impl::init(&exp(ExpKind::Seq(vec![exp(ExpKind::Hole(Hole::Next))]))).is_err());
    assert_eq!(fields_impl::validate(&hint, 2), Ok(()));
    assert_eq!(
        fields_impl::validate(&hint, 1),
        Err(FieldError::ArityMismatch { expected: 1, actual: 2 })
    );
}

#[test]
fn test_empty_field_hint_retains_its_expression_span() {
    let exp_hint = p4spec_rust::phrase! { node: ExpKind::Seq(vec![]), span: span("empty-fields") };
    let hint = fields_impl::init(&exp_hint).unwrap();
    assert_eq!(hint.span, span("empty-fields"));
    assert_eq!(
        fields_impl::validate(&hint, 1),
        Err(FieldError::ArityMismatch { expected: 1, actual: 0 })
    );
}
