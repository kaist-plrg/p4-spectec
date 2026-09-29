use crate::common::*;

#[test]
fn test_id_set_uses_identifier_text_as_its_key() {
    let id_first = id("x", "first");
    let id_second = id("x", "second");
    let mut ids = IdSet::new();

    assert!(ids.insert(id_first.clone()));
    assert!(!ids.insert(id_second.clone()));
    assert!(ids.contains(&id_second));
    assert_eq!(ids.iter().collect::<Vec<_>>(), vec![&id_first]);
}
#[test]
fn test_id_map_uses_identifier_text_as_its_key() {
    let id_first = id("x", "first");
    let id_second = id("x", "second");
    let mut ids = IdMap::new();

    assert_eq!(ids.insert(id_first.clone(), 1), None);
    assert_eq!(ids.insert(id_second.clone(), 2), Some(1));
    assert_eq!(ids.get(&id_first), Some(&2));

    assert_eq!(ids.keys().collect::<Vec<_>>(), vec![&id_second]);
}

#[test]
fn test_id_map_rejects_mismatched_lists() {
    let keys = [id("x", "first")];
    let values = [1, 2];

    let error = IdMap::from_lists(&keys, &values).expect_err("mismatched list lengths");

    assert_eq!(error, ArityMismatch::new(1, 2));
}
#[test]
fn test_free_identifier_sets_preserve_source_spans() {
    let id_stored = id("x", "stored");
    let id_lookup = id("x", "lookup");
    let exp: il::ast::Exp = p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id_stored.clone()), note: il::ast::TypKind::Bool, span: Span::default());

    let ids: IdSet = exp.free_ids();
    assert!(ids.contains(&id_lookup));
    assert_eq!(ids.iter().next().unwrap().span, id_stored.span);
}

#[test]
fn test_free_into_extends_one_ordered_set_without_duplicates() {
    let id_exp = |name| -> il::ast::Exp {
        p4spec_rust::note_phrase!(node: p4spec_rust::lang::il::ast::ExpKind::Id(id(name, name)), note: il::ast::TypKind::Bool, span: Span::default())
    };
    let exp: il::ast::Exp = p4spec_rust::note_phrase! {
        node: il::ast::ExpKind::Tuple(vec![id_exp("x"), id_exp("x"), id_exp("y")]),
        note: il::ast::TypKind::Bool,
        span: Span::default(),
    };
    let mut ids = IdSet::from([id("seed", "seed")]);

    exp.free_ids_into(&mut ids);

    assert_eq!(ids, IdSet::from([id("seed", "seed"), id("x", "x"), id("y", "y")]));
}

#[test]
fn test_id_map_clone_isolates_branch_updates() {
    let id_x = id("x", "original");
    let id_y = id("y", "branch");
    let mut map = [(id_x.clone(), 1)].into_iter().collect::<IdMap<_>>();
    let mut map_branch = map.clone();

    map_branch.insert(id_x.clone(), 2);
    map_branch.insert(id_y.clone(), 3);

    assert_eq!(map.get(&id_x), Some(&1));
    assert_eq!(map.get(&id_y), None);
    assert_eq!(map_branch.get(&id_x), Some(&2));
    assert_eq!(map_branch.get(&id_y), Some(&3));

    map.insert(id_y.clone(), 4);
    assert_eq!(map.get(&id_y), Some(&4));
    assert_eq!(map_branch.get(&id_y), Some(&3));
}

#[test]
fn test_id_set_difference_is_relative_complement() {
    let ids_l = IdSet::from([id("x", "left"), id("y", "left")]);
    let ids_r = IdSet::from([id("y", "right"), id("z", "right")]);

    assert_eq!(ids_l.difference(&ids_r), IdSet::from([id("x", "left")]));
}
