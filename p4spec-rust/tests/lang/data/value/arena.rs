//! Arena interning, annotation, and comparison tests

use super::{hash, span};
use num_bigint::BigInt;
use p4spec_rust::lang::traits::{cmp::SyntaxCmp, eq::SyntaxEq};
use p4spec_rust::lang::{
    common::prim::num::{Natural, Number},
    common::source::{Position, Span},
    data::{
        typ,
        value::{Value, ValueArena, ValueError, ValueTag, get, make},
    },
};
use p4spec_rust::util::json::json;
use std::rc::Rc;

#[test]
fn test_type_allocation_identity_preserves_annotations_and_value_syntax() {
    let mut arena = ValueArena::new();
    let typ_l = Rc::new(
        typ::make::var(
            p4spec_rust::phrase!(node: "T".to_owned(), span: span("type.spec", 3)),
            vec![typ::make::bool()],
        )
        .node,
    );
    let typ_r = Rc::new(typ_l.as_ref().clone());
    let child = make::bool(&mut arena, true, Span::default()).unwrap();
    let value_l = make::list(&mut arena, typ_l.clone(), vec![child], Span::default()).unwrap();
    let value_r = make::list(&mut arena, typ_r.clone(), vec![child], Span::default()).unwrap();
    assert_ne!(value_l.note, value_r.note);

    assert!(Rc::ptr_eq(arena.typ(&value_l), &typ_l));
    assert!(Rc::ptr_eq(arena.typ(&value_r), &typ_r));
    assert_eq!(arena.typ(&value_l), arena.typ(&value_r));

    let parent_l = make::list(&mut arena, typ_l.clone(), vec![value_l], Span::default()).unwrap();
    let parent_r = make::list(&mut arena, typ_l, vec![value_r], Span::default()).unwrap();
    assert_ne!(parent_l.node, parent_r.node);
    assert!(arena.view(parent_l).syntax_eq(&arena.view(parent_r)));
    assert_eq!(arena.canon_id(&parent_l), arena.canon_id(&parent_r));
}

#[test]
fn test_interned_bodies_preserve_outer_and_child_annotations() {
    let mut arena = ValueArena::new();
    let child = make::bool(&mut arena, true, span("child.p4", 11)).unwrap();
    let child_relocated = Value {
        span: make::bool(&mut arena, false, span("child.p4", 13))
            .unwrap()
            .span,
        ..child
    };
    let child_annotated = Value {
        note: make::new(
            &mut arena,
            p4spec_rust::lang::data::value::ValueKind::Bool(false),
            typ::make::text().node.clone().into(),
            Span::default(),
        )
        .unwrap()
        .note,
        ..child
    };
    assert_eq!(child.node, child_relocated.node);
    assert_eq!(child.node, child_annotated.node);
    assert_ne!(child.span, child_relocated.span);
    assert_ne!(child.note, child_annotated.note);

    let typ = typ::make::tuple(vec![typ::make::bool()]).node;
    let tuple = make::tuple(&mut arena, typ.clone().into(), vec![child], Span::default()).unwrap();
    let tuple_relocated =
        make::tuple(&mut arena, typ.clone().into(), vec![child_relocated], Span::default())
            .unwrap();
    let tuple_annotated =
        make::tuple(&mut arena, typ.clone().into(), vec![child_annotated], Span::default())
            .unwrap();
    assert_ne!(tuple.node, tuple_relocated.node);
    assert_ne!(tuple.node, tuple_annotated.node);
    assert!(arena.view(tuple).syntax_eq(&arena.view(tuple_relocated)));
    assert!(arena.view(tuple).syntax_eq(&arena.view(tuple_annotated)));
    assert!(
        arena
            .view(tuple)
            .syntax_cmp(&arena.view(tuple_relocated))
            .is_eq()
    );
    assert!(
        arena
            .view(tuple)
            .syntax_cmp(&arena.view(tuple_annotated))
            .is_eq()
    );
    let children = get::tuple(&arena, &tuple).unwrap();
    assert!(get::bool(&arena, &children[0]).unwrap());
    assert_eq!(arena.span(&children[0]), &span("child.p4", 11));
    assert_eq!(arena.typ(&children[0]).as_ref(), &typ::TypKind::Bool);
    assert_eq!(
        get::bool(&arena, &tuple),
        Err(ValueError::KindMismatch { expected: ValueTag::Bool, actual: ValueTag::Tuple })
    );
}

#[test]
fn test_interned_case_preserves_atom_locations_and_children() {
    use p4spec_rust::lang::common::notation::{atom::Atom, mixfix::Mixfix};
    let mut arena = ValueArena::new();
    let child = make::bool(&mut arena, true, span("child.p4", 11)).unwrap();
    let make_case = |line_relocated: Option<usize>| {
        let span_atom = |line| {
            span(if Some(line) == line_relocated { "other.spec" } else { "label.spec" }, line)
        };
        Mixfix::Brack(
            p4spec_rust::phrase!(node: Atom::LParen, span: span_atom(1)),
            Box::new(Mixfix::Infix(
                Box::new(Mixfix::Arg(child)),
                p4spec_rust::phrase!(node: Atom::Arrow, span: span_atom(2)),
                Box::new(Mixfix::Seq(vec![
                    Mixfix::Atom(
                        p4spec_rust::phrase!(node: Atom::Keyword("tail".to_owned()), span: span_atom(3)),
                    ),
                    Mixfix::Arg(child),
                ])),
            )),
            p4spec_rust::phrase!(node: Atom::RParen, span: span_atom(4)),
        )
    };
    let case = make_case(None);
    let value =
        make::case(&mut arena, typ::TypKind::Bool.into(), case.clone(), Span::default()).unwrap();
    let value_same =
        make::case(&mut arena, typ::TypKind::Bool.into(), make_case(None), Span::default())
            .unwrap();
    assert_eq!(value.node, value_same.node);
    for line in 1..=4 {
        let case_relocated = make_case(Some(line));
        let value_relocated =
            make::case(&mut arena, typ::TypKind::Bool.into(), case_relocated, Span::default())
                .unwrap();
        assert_ne!(value.node, value_relocated.node);
        assert_ne!(hash(arena.kind(&value)), hash(arena.kind(&value_relocated)));
        assert_eq!(arena.canon_id(&value), arena.canon_id(&value_relocated));
        assert!(arena.view(value).syntax_eq(&arena.view(value_relocated)));
    }
    let case_stored = get::case(&arena, &value).unwrap();

    assert_eq!(case_stored.args(), vec![&child, &child]);
}

#[test]
fn test_label_bodies_are_shared_across_outer_locations() {
    use p4spec_rust::lang::common::notation::atom::Atom;
    let mut arena = ValueArena::new();
    let child = make::bool(&mut arena, true, Span::default()).unwrap();
    let atom =
        p4spec_rust::phrase!(node: Atom::Keyword("field".to_owned()), span: span("field.p4", 3));
    let value = make::structure(
        &mut arena,
        typ::TypKind::Bool.into(),
        vec![(atom.clone(), child)],
        span("field.p4", 7),
    )
    .unwrap();
    let value_relocated = make::structure(
        &mut arena,
        typ::TypKind::Bool.into(),
        vec![(atom.clone(), child)],
        span("field.p4", 9),
    )
    .unwrap();
    assert_eq!(value.node, value_relocated.node);
    assert_ne!(value.span, value_relocated.span);
    assert_eq!(get::structure(&arena, &value).unwrap()[0].0, atom);
    let func = make::func(
        &mut arena,
        p4spec_rust::phrase!(node: "f".to_owned(), span: Span::default()),
        vec![],
        vec![],
        typ::make::bool(),
        span("name.p4", 13),
    )
    .unwrap();
    let func_relocated = make::func(
        &mut arena,
        p4spec_rust::phrase!(node: "f".to_owned(), span: Span::default()),
        vec![],
        vec![],
        typ::make::bool(),
        span("name.p4", 17),
    )
    .unwrap();
    assert_eq!(func.node, func_relocated.node);
    assert_ne!(func.span, func_relocated.span);
    assert_eq!(
        (match arena.kind(&func) {
            p4spec_rust::lang::data::value::ValueKind::Func(id) => id,
            _ => panic!("expected function"),
        })
        .node,
        "f"
    );
}

#[test]
fn test_arena_growth_preserves_children_and_value_order() {
    let mut arena = ValueArena::new();
    let value_high = make::nat(&mut arena, Natural::from(99_u64), Span::default()).unwrap();
    let value_low = make::nat(&mut arena, Natural::from(1_u64), Span::default()).unwrap();
    let typ = typ::make::tuple(vec![typ::make::nat()]).node;
    let tuple_high =
        make::tuple(&mut arena, typ.clone().into(), vec![value_high], Span::default()).unwrap();
    let tuple_low =
        make::tuple(&mut arena, typ.clone().into(), vec![value_low], Span::default()).unwrap();
    for line in 0..512 {
        make::text(&mut arena, format!("text-{line}"), span("growth.p4", line)).unwrap();
    }
    assert_eq!(arena.view(tuple_low).syntax_cmp(&arena.view(tuple_high)), std::cmp::Ordering::Less);
    assert_eq!(get::tuple(&arena, &tuple_high).unwrap(), &[value_high]);
    assert_eq!(get::num(&arena, &value_high).unwrap(), &Number::Nat(Natural::from(99_u64)));
}

#[test]
fn test_value_order_uses_variant_and_payload_order() {
    let mut arena = ValueArena::new();
    let bool_value = make::bool(&mut arena, false, Span::default()).unwrap();
    let nat_value = make::nat(&mut arena, Natural::from(0_u64), Span::default()).unwrap();
    let int_value = make::int(&mut arena, BigInt::from(0), Span::default()).unwrap();
    let text_value = make::text(&mut arena, String::new(), Span::default()).unwrap();

    assert!(
        arena
            .view(bool_value)
            .syntax_cmp(&arena.view(nat_value))
            .is_lt()
    );
    assert!(
        arena
            .view(nat_value)
            .syntax_cmp(&arena.view(int_value))
            .is_lt()
    );
    assert!(
        arena
            .view(int_value)
            .syntax_cmp(&arena.view(text_value))
            .is_lt()
    );
}

#[test]
fn test_function_values_are_ordered_by_name() {
    let mut arena = ValueArena::new();
    let id_a = p4spec_rust::phrase!(node: "a".to_owned(), span: Span::default());
    let id_b = p4spec_rust::phrase!(node: "b".to_owned(), span: Span::default());
    let func_a =
        make::func(&mut arena, id_a, Vec::new(), Vec::new(), typ::make::bool(), Span::default())
            .unwrap();
    let func_b =
        make::func(&mut arena, id_b, Vec::new(), Vec::new(), typ::make::bool(), Span::default())
            .unwrap();

    assert!(arena.view(func_a).syntax_cmp(&arena.view(func_b)).is_lt());
    assert_ne!(func_a, func_b);
}

#[test]
fn test_external_float_order_normalizes_signed_zero() {
    let mut arena = ValueArena::new();
    let typ = Rc::new(typ::make::text().node);
    let negative_zero =
        make::external(&mut arena, typ.clone(), serde_json::json!(-0.0).into(), Span::default())
            .unwrap();
    let positive_zero =
        make::external(&mut arena, typ.clone(), serde_json::json!(0.0).into(), Span::default())
            .unwrap();

    assert_eq!(negative_zero, positive_zero);
    assert_eq!(hash(&negative_zero), hash(&positive_zero));
    assert!(
        arena
            .view(negative_zero)
            .syntax_cmp(&arena.view(positive_zero))
            .is_eq()
    );
}

#[test]
fn test_function_value_syntax_equality_ignores_outer_spans() {
    let mut arena = ValueArena::new();
    let span_l = span("left.spec", 1);
    let span_r = span("right.spec", 1);
    let value_l = make::func(
        &mut arena,
        p4spec_rust::phrase!(node: "f".to_owned(), span: Span::default()),
        Vec::new(),
        Vec::new(),
        typ::make::bool(),
        span_l,
    )
    .unwrap();
    let value_r = make::func(
        &mut arena,
        p4spec_rust::phrase!(node: "f".to_owned(), span: Span::default()),
        Vec::new(),
        Vec::new(),
        typ::make::bool(),
        span_r,
    )
    .unwrap();

    assert!(arena.view(value_l).syntax_eq(&arena.view(value_r)));
}

#[test]
fn test_syntax_equality_distinguishes_nested_payloads_and_variants() {
    let mut arena = ValueArena::new();
    let value_true = make::bool(&mut arena, true, Span::default()).unwrap();
    let value_false = make::bool(&mut arena, false, Span::default()).unwrap();
    let atom = p4spec_rust::phrase!(node: p4spec_rust::lang::common::notation::atom::Atom::Keyword("field".to_owned()), span: Span::default());
    let value_l = make::structure(
        &mut arena,
        typ::TypKind::Bool.into(),
        vec![(atom.clone(), value_true)],
        Span::default(),
    )
    .unwrap();
    let value_r = make::structure(
        &mut arena,
        typ::TypKind::Bool.into(),
        vec![(atom, value_false)],
        Span::default(),
    )
    .unwrap();
    let value_l =
        make::list(&mut arena, typ::TypKind::Bool.into(), vec![value_l], Span::default()).unwrap();
    let value_r =
        make::list(&mut arena, typ::TypKind::Bool.into(), vec![value_r], Span::default()).unwrap();
    let value_text = make::text(&mut arena, "true".to_owned(), Span::default()).unwrap();

    assert!(!arena.view(value_l).syntax_eq(&arena.view(value_r)));
    assert!(!arena.view(value_true).syntax_eq(&arena.view(value_text)));
}

#[test]
fn test_canonical_identity_preserves_nested_locations_through_growth() {
    let mut arena = ValueArena::new();
    let mut value_l = make::bool(&mut arena, true, span("left.p4", 1)).unwrap();
    let mut value_r = make::bool(&mut arena, true, span("right.p4", 2)).unwrap();
    for line in 0..128 {
        value_l =
            make::list(&mut arena, typ::TypKind::Bool.into(), vec![value_l], span("left.p4", line))
                .unwrap();
        value_r = make::list(
            &mut arena,
            typ::TypKind::Text.into(),
            vec![value_r],
            span("right.p4", line),
        )
        .unwrap();
        assert_ne!(value_l.node, value_r.node);
        assert_eq!(arena.canon_id(&value_l), arena.canon_id(&value_r));
    }
    assert!(arena.view(value_l).syntax_eq(&arena.view(value_r)));
    let child_l = get::list(&arena, &value_l).unwrap()[0];
    let child_r = get::list(&arena, &value_r).unwrap()[0];
    assert_eq!(arena.span(&child_l), &span("left.p4", 126));
    assert_eq!(arena.span(&child_r), &span("right.p4", 126));
    let value_false = make::bool(&mut arena, false, Span::default()).unwrap();
    let value_false =
        make::list(&mut arena, typ::TypKind::Bool.into(), vec![value_false], Span::default())
            .unwrap();
    assert_ne!(arena.canon_id(&value_l), arena.canon_id(&value_false));
}

#[test]
fn test_canonical_identities_ignore_all_locations_but_distinguish_contents() {
    use p4spec_rust::{
        lang::common::notation::{atom::Atom, mixfix::Mixfix},
        lang::data::value::{Value, ValueKind},
    };

    fn values(arena: &mut ValueArena, line: usize) -> Vec<Value> {
        let span = span("values.spec", line);
        let value_true = make::bool(arena, true, span.clone()).unwrap();
        let value_false = make::bool(arena, false, span.clone()).unwrap();
        let atom =
            p4spec_rust::phrase!(node: Atom::Keyword("field".to_owned()), span: span.clone());
        let id = p4spec_rust::phrase!(node: "function".to_owned(), span: span.clone());
        let case = Mixfix::Infix(
            Box::new(Mixfix::Arg(value_true)),
            atom.clone(),
            Box::new(Mixfix::Arg(value_false)),
        );
        let value_case = make::case(arena, typ::TypKind::Bool.into(), case, span.clone()).unwrap();
        let Mixfix::Infix(_, atom_case, _) = get::case(arena, &value_case).unwrap() else {
            panic!("expected infix")
        };
        assert_eq!(&atom_case.span, arena.span(&value_true));
        let mut values = vec![value_case];
        for kind in [
            ValueKind::Bool(true),
            ValueKind::Bool(false),
            ValueKind::Num(Number::Nat(Natural::from(1_u64))),
            ValueKind::Num(Number::Int(BigInt::from(1))),
            ValueKind::Text("field".to_owned()),
            ValueKind::Struct(vec![(atom, value_true)]),
            ValueKind::Tuple(vec![value_true, value_false]),
            ValueKind::Opt(None),
            ValueKind::Opt(Some(value_true)),
            ValueKind::List(vec![value_true, value_false]),
            ValueKind::List(vec![value_false, value_true]),
            ValueKind::List(vec![value_true]),
            ValueKind::Func(id),
            ValueKind::Extern(json::Null.into()),
        ] {
            values.push(make::new(arena, kind, typ::TypKind::Bool.into(), span.clone()).unwrap());
        }
        values
    }
    let mut arena = ValueArena::new();
    let values_l = values(&mut arena, 1);
    let values_r = values(&mut arena, 2);
    for (index_l, value_l) in values_l.iter().enumerate() {
        for (index_r, value_r) in values_r.iter().enumerate() {
            let equal = index_l == index_r;
            assert_eq!(arena.canon_id(value_l) == arena.canon_id(value_r), equal);
            assert_eq!(
                arena
                    .view(*value_l)
                    .syntax_cmp(&arena.view(*value_r))
                    .is_eq(),
                equal
            );
        }
    }
    for index in [0, 6, 13] {
        assert_ne!(values_l[index].node, values_r[index].node);
    }
    let fields_l = get::structure(&arena, &values_l[6]).unwrap();
    let fields_r = get::structure(&arena, &values_r[6]).unwrap();
    assert_eq!(fields_l[0].0.span, span("values.spec", 1));
    assert_eq!(fields_r[0].0.span, span("values.spec", 2));
    assert_eq!(
        (match arena.kind(&values_l[13]) {
            p4spec_rust::lang::data::value::ValueKind::Func(id) => id,
            _ => panic!("expected function"),
        })
        .span,
        span("values.spec", 1)
    );
    assert_eq!(
        (match arena.kind(&values_r[13]) {
            p4spec_rust::lang::data::value::ValueKind::Func(id) => id,
            _ => panic!("expected function"),
        })
        .span,
        span("values.spec", 2)
    );
}

#[test]
fn test_default_span_interning_preserves_nondefault_positions() {
    let mut arena = ValueArena::new();
    let value = make::bool(&mut arena, true, Span::default()).unwrap();
    let value_empty = Value {
        span: make::bool(
            &mut arena,
            false,
            Span::new(Position::new("", 0, 0), Position::new("", 0, 0)),
        )
        .unwrap()
        .span,
        ..value
    };
    assert_eq!(value.span, value_empty.span);
    for span in [
        Span::new(Position::new("", 1, 0), Position::new("", 1, 0)),
        Span::new(Position::new("", 0, 0), Position::new("", 0, 1)),
        Span::new(Position::new("program.p4", 0, 0), Position::new("program.p4", 0, 0)),
    ] {
        let value_located =
            Value { span: make::bool(&mut arena, false, span.clone()).unwrap().span, ..value };
        assert_ne!(value_located.span, value.span);
        assert_eq!(arena.span(&value_located), &span);
    }
}

#[test]
fn test_primitive_constructors_reuse_type_allocations() {
    use p4spec_rust::lang::data::value::Value;

    fn primitives(arena: &mut ValueArena) -> [Value; 4] {
        [
            make::bool(arena, true, Span::default()).unwrap(),
            make::nat(arena, Natural::from(1_u64), Span::default()).unwrap(),
            make::int(arena, BigInt::from(-1), Span::default()).unwrap(),
            make::text(arena, "text".to_owned(), Span::default()).unwrap(),
        ]
    }

    let mut arena = ValueArena::new();
    let values = primitives(&mut arena);
    for _ in 0..16 {
        for (value, value_again) in values.iter().zip(primitives(&mut arena)) {
            assert_eq!(value.note, value_again.note);
            assert!(Rc::ptr_eq(arena.typ(value), arena.typ(&value_again)));
        }
    }
}

#[test]
fn test_external_object_key_order_shares_canonical_identity() {
    let mut arena = ValueArena::new();
    let typ = std::rc::Rc::new(typ::TypKind::Bool);
    let fields =
        vec![("a".to_owned(), serde_json::json!(1)), ("b".to_owned(), serde_json::json!(2))];
    let value_a = make::external(
        &mut arena,
        typ.clone(),
        json::Object(fields.clone().into_iter().collect()).into(),
        Span::default(),
    )
    .unwrap();
    let value_b = make::external(
        &mut arena,
        typ,
        json::Object(fields.into_iter().rev().collect()).into(),
        Span::default(),
    )
    .unwrap();
    assert_eq!(arena.canon_id(&value_a), arena.canon_id(&value_b));
    assert_eq!(hash(arena.kind(&value_a)), hash(arena.kind(&value_b)));
    assert!(arena.view(value_a).syntax_cmp(&arena.view(value_b)).is_eq());
}

#[test]
fn test_external_canonical_equality_stops_at_json() {
    use p4spec_rust::lang::data::value::external::{Encoding, encode_with};
    let mut arena = ValueArena::new();
    let value = make::bool(&mut arena, true, span("value.p4", 1)).unwrap();
    let value_relocated = Value {
        span: make::bool(&mut arena, false, span("value.p4", 2))
            .unwrap()
            .span,
        ..value
    };
    let value_retyped = Value {
        note: make::new(
            &mut arena,
            p4spec_rust::lang::data::value::ValueKind::Bool(false),
            typ::TypKind::Text.into(),
            Span::default(),
        )
        .unwrap()
        .note,
        ..value
    };
    for encoding in [Encoding::ArenaRelative, Encoding::ArenaIndependent] {
        let mut values_external = Vec::new();
        for value in [value, value_relocated, value_retyped, value] {
            let payload = Rc::new(encode_with(&arena, encoding, &vec![value]).unwrap());
            values_external.push(
                make::external(&mut arena, typ::TypKind::Bool.into(), payload, Span::default())
                    .unwrap(),
            );
        }
        for value_external in &values_external[1..3] {
            assert_ne!(
                get::external(&arena, &values_external[0]).unwrap(),
                get::external(&arena, value_external).unwrap()
            );
            assert_ne!(arena.canon_id(&values_external[0]), arena.canon_id(value_external));
            assert!(
                !arena
                    .view(values_external[0])
                    .syntax_eq(&arena.view(*value_external))
            );
        }
        assert_eq!(values_external[0].node, values_external[3].node);
        assert_eq!(arena.canon_id(&values_external[0]), arena.canon_id(&values_external[3]));
        assert_eq!(hash(arena.kind(&values_external[0])), hash(arena.kind(&values_external[3])));
    }
}
