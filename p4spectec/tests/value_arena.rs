//! Exact type sharing and annotation preservation in the value arena

use p4spectec::lang::{
    common::source::{Position, Span},
    data::{arena::Arena, typ::make as make_typ, value::flat::make},
};
use std::rc::Rc;

#[test]
fn equal_fresh_types_share_handles_and_parent_bodies() {
    let mut arena = Arena::new();
    let span = Span::default();
    let typ = Rc::new(make_typ::list(make_typ::bool()).node);
    let value = make::list(&mut arena, typ.clone(), vec![], span).unwrap();
    let value_outer =
        make::tuple(&mut arena, Rc::new(make_typ::tuple(vec![]).node), vec![value], span).unwrap();
    for _ in 0..1000 {
        let typ_fresh = Rc::new(make_typ::list(make_typ::bool()).node);
        assert!(!Rc::ptr_eq(&typ, &typ_fresh));
        let value_fresh = make::list(&mut arena, typ_fresh, vec![], span).unwrap();
        assert_eq!(value.note, value_fresh.note);
        assert!(Rc::ptr_eq(&typ, arena.value.typ(value_fresh.note)));
        let value_outer_fresh =
            make::tuple(&mut arena, Rc::new(make_typ::tuple(vec![]).node), vec![value_fresh], span)
                .unwrap();
        assert_eq!(value_outer, value_outer_fresh);
    }
}

#[test]
fn distinct_types_and_nested_spans_are_preserved() {
    let mut arena = Arena::new();
    let span = Span::default();
    let typ_a = make_typ::list(make_typ::bool());
    let mut typ_inner = make_typ::bool();
    typ_inner.span = Span::new(Position { line: 7, ..Position::default() }, Position::default());
    let typ_b = make_typ::list(typ_inner);
    let typ_c = make_typ::list(make_typ::nat());
    let value_a = make::list(&mut arena, Rc::new(typ_a.node.clone()), vec![], span).unwrap();
    let value_b = make::list(&mut arena, Rc::new(typ_b.node.clone()), vec![], span).unwrap();
    let value_c = make::list(&mut arena, Rc::new(typ_c.node.clone()), vec![], span).unwrap();
    assert_ne!(value_a.note, value_b.note);
    assert_ne!(value_a.note, value_c.note);
    assert_eq!(arena.value.typ(value_b.note).as_ref(), &typ_b.node);
    assert_eq!(arena.value.typ(value_c.note).as_ref(), &typ_c.node);
}
