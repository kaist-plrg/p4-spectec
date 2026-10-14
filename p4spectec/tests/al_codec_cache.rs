//! AL codec reuse, recursive propagation, and arena reset contracts

use p4spectec::{
    interface::al::{Cache, decode::Decode, encode::Encode},
    lang::{
        common::source::{Position, Span},
        data::{
            arena::Arena,
            notation::{flat::Mixop as FlatMixop, tree::Mixop},
            typ::make as make_typ,
            value::flat::{Value, get, make},
        },
        il::ast::Typ,
    },
};

fn list_bool(arena: &mut Arena, value_bool: bool, span: Span) -> Value {
    let value = make::bool(arena, value_bool, span).unwrap();
    make::list(arena, make_typ::list(make_typ::bool()).node.into(), vec![value], span).unwrap()
}

#[test]
fn encode_reuses_canonical_values_across_annotations() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(Cache::DEFAULT_LIMIT);
    let span = Span::new(Position { line: 2, ..Position::default() }, Position::default());
    let value_a = list_bool(&mut arena, true, Span::default());
    let value_b = list_bool(&mut arena, true, span);
    assert_ne!(value_a, value_b);
    let value_meta_a = value_a.encode_cached(&mut arena, &mut cache).unwrap();
    let value_meta_b = value_b.encode_cached(&mut arena, &mut cache).unwrap();
    assert_eq!(value_meta_a, value_meta_b);
    let value = Value::decode(&mut arena, value_meta_a).unwrap();
    assert!(get::bool(&arena, &get::list(&arena, &value).unwrap()[0]).unwrap());
}

#[test]
fn recursive_conversions_reuse_child_results() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(Cache::DEFAULT_LIMIT);
    let value_inner = list_bool(&mut arena, true, Span::default());
    let value_outer = make::tuple(
        &mut arena,
        make_typ::tuple(vec![make_typ::list(make_typ::bool())])
            .node
            .into(),
        vec![value_inner],
        Span::default(),
    )
    .unwrap();
    let value_meta = value_outer.encode_cached(&mut arena, &mut cache).unwrap();
    let values_meta = get::case(&arena, &value_meta).unwrap().args()[0];
    let value_inner_meta = get::list(&arena, &values_meta).unwrap()[0];
    assert_eq!(value_inner.encode_cached(&mut arena, &mut cache).unwrap(), value_inner_meta);
    let value_outer = Value::decode_cached(&mut arena, &mut cache, value_meta).unwrap();
    let value_inner = get::tuple(&arena, &value_outer).unwrap()[0];
    assert_eq!(
        Value::decode_cached(&mut arena, &mut cache, value_inner_meta).unwrap(),
        value_inner
    );
}

#[test]
fn disabled_and_uncached_conversions_preserve_each_inputs_annotations() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(0);
    for line in [0, 2] {
        let span = Span::new(Position { line, ..Position::default() }, Position::default());
        let value = list_bool(&mut arena, true, span);
        let value_meta = value.encode_cached(&mut arena, &mut cache).unwrap();
        assert_eq!(*arena.value.span(value_meta.span), span);
        let value_meta = value.encode(&mut arena).unwrap();
        assert_eq!(*arena.value.span(value_meta.span), span);
        let value = Value::decode_cached(&mut arena, &mut cache, value_meta).unwrap();
        assert_eq!(*arena.value.span(value.span), span);
        let value = Value::decode(&mut arena, value_meta).unwrap();
        assert_eq!(*arena.value.span(value.span), span);
    }
}

#[test]
fn encoding_does_not_seed_reverse_decoding() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(Cache::DEFAULT_LIMIT);
    let value_source = list_bool(&mut arena, true, Span::default());
    let value_meta = value_source.encode_cached(&mut arena, &mut cache).unwrap();
    let value_cached = Value::decode_cached(&mut arena, &mut cache, value_meta).unwrap();
    let value_uncached = Value::decode(&mut arena, value_meta).unwrap();
    assert_eq!(arena.value.typ(value_cached.note), arena.value.typ(value_uncached.note));
    assert_ne!(arena.value.typ(value_cached.note), arena.value.typ(value_source.note));
}

#[test]
fn clear_discards_handles_before_arena_reset() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(Cache::DEFAULT_LIMIT);
    let value = list_bool(&mut arena, true, Span::default());
    let value_meta = value.encode_cached(&mut arena, &mut cache).unwrap();
    Value::decode_cached(&mut arena, &mut cache, value_meta).unwrap();
    cache.clear();
    arena.reset_values();
    let value = list_bool(&mut arena, false, Span::default());
    let value_meta = value.encode_cached(&mut arena, &mut cache).unwrap();
    let value = Value::decode_cached(&mut arena, &mut cache, value_meta).unwrap();
    assert!(!get::bool(&arena, &get::list(&arena, &value).unwrap()[0]).unwrap());
}

#[test]
fn type_cache_uses_contents_and_clear_discards_results() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(Cache::DEFAULT_LIMIT);
    let typ_a = make_typ::list(make_typ::bool());
    let mut typ_b = typ_a.clone();
    typ_b.span.left.line = 7;
    let value_a = typ_a.encode(&mut arena).unwrap();
    let value_b = typ_b.encode(&mut arena).unwrap();
    assert_eq!(Typ::decode_cached(&mut arena, &mut cache, value_a).unwrap(), typ_a);
    assert_eq!(Typ::decode_cached(&mut arena, &mut cache, value_b).unwrap(), typ_a);
    cache.clear();
    assert_eq!(Typ::decode_cached(&mut arena, &mut cache, value_b).unwrap(), typ_b);
}

#[test]
fn malformed_values_still_fail_with_cache_enabled() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(Cache::DEFAULT_LIMIT);
    let value_meta = make::bool(&mut arena, true, Span::default()).unwrap();
    assert!(Value::decode_cached(&mut arena, &mut cache, value_meta).is_err());
    assert!(Value::decode_cached(&mut arena, &mut cache, value_meta).is_err());
}

#[test]
fn mixop_caches_are_cleared_before_value_handles_are_reused() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(Cache::DEFAULT_LIMIT);
    let mixop = arena.mixop.intern(&Mixop::Seq(vec![])).unwrap();
    let value_meta = mixop.encode_cached(&mut arena, &mut cache).unwrap();
    assert_eq!(mixop.encode_cached(&mut arena, &mut cache).unwrap(), value_meta);
    assert_eq!(FlatMixop::decode_cached(&mut arena, &mut cache, value_meta).unwrap(), mixop);
    cache.clear();
    arena.reset_values();
    // The same notation survives reset, but its old encoded value does not.
    let value_meta = mixop.encode_cached(&mut arena, &mut cache).unwrap();
    assert_eq!(FlatMixop::decode(&mut arena, value_meta).unwrap(), mixop);
    let mixop = arena.mixop.intern(&Mixop::Arg).unwrap();
    let value_meta = mixop.encode_cached(&mut arena, &mut cache).unwrap();
    assert_eq!(FlatMixop::decode_cached(&mut arena, &mut cache, value_meta).unwrap(), mixop);
}

#[test]
fn decode_reuses_canonical_values_across_annotations() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(Cache::DEFAULT_LIMIT);
    let span_a = Span::default();
    let span_b = Span::new(Position { line: 3, ..Position::default() }, Position::default());
    let value_a = list_bool(&mut arena, true, span_a);
    let value_b = list_bool(&mut arena, true, span_b);
    let value_meta_a = value_a.encode(&mut arena).unwrap();
    let value_meta_b = value_b.encode(&mut arena).unwrap();
    assert_ne!(value_meta_a, value_meta_b);
    let value_a = Value::decode_cached(&mut arena, &mut cache, value_meta_a).unwrap();
    let value_b = Value::decode_cached(&mut arena, &mut cache, value_meta_b).unwrap();
    assert_eq!(value_a, value_b);
    assert_eq!(*arena.value.span(value_b.span), span_a);
}

#[test]
fn full_tables_keep_hits_but_do_not_retain_new_results() {
    let mut arena = Arena::new();
    let mut cache = Cache::new(2);
    let span = Span::new(Position { line: 2, ..Position::default() }, Position::default());
    let value_a = list_bool(&mut arena, true, Span::default());
    let value_b = list_bool(&mut arena, false, Span::default());
    let value_a_later = list_bool(&mut arena, true, span);
    let value_b_later = list_bool(&mut arena, false, span);
    let value_meta_a = value_a.encode_cached(&mut arena, &mut cache).unwrap();
    let value_meta_b = value_b.encode_cached(&mut arena, &mut cache).unwrap();
    assert_eq!(value_a_later.encode_cached(&mut arena, &mut cache).unwrap(), value_meta_a);
    let value_meta_b_later = value_b_later.encode_cached(&mut arena, &mut cache).unwrap();
    assert_ne!(value_meta_b_later, value_meta_b);
    let value_a = Value::decode_cached(&mut arena, &mut cache, value_meta_a).unwrap();
    let value_meta_a_later = value_a_later.encode(&mut arena).unwrap();
    assert_eq!(Value::decode_cached(&mut arena, &mut cache, value_meta_a_later).unwrap(), value_a);
    let value_b = Value::decode_cached(&mut arena, &mut cache, value_meta_b).unwrap();
    let value_b_later = Value::decode_cached(&mut arena, &mut cache, value_meta_b_later).unwrap();
    assert_ne!(value_b, value_b_later);
    cache.clear();
    let value_b = Value::decode_cached(&mut arena, &mut cache, value_meta_b).unwrap();
    assert_eq!(Value::decode_cached(&mut arena, &mut cache, value_meta_b_later).unwrap(), value_b);
}
