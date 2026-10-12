//! Identifier sharing, lookup, and value encoding compatibility

use p4spectec::{
    lang::{
        common::{
            Id,
            ds::map::IdMap,
            source::{FileId, Position, Span},
        },
        data::{
            arena::Arena,
            encoding::Encoding,
            value::flat::{ValueKind, external},
        },
    },
    phrase,
};
use serde_json::json;

fn id_at(name: &str, line: usize) -> Id {
    let pos = Position::new(FileId::intern("identifiers.watsup"), line, 0);
    phrase!(node: name.into(), span: Span::new(pos, pos))
}

#[test]
fn identifier_clones_share_names() {
    let id = id_at("name", 1);
    let id_copy = id.clone();
    assert_eq!(id, id_copy);
    assert_eq!(id.node.as_ptr(), id_copy.node.as_ptr());
}

#[test]
fn stripping_suffixes_preserves_names_and_spans() {
    // Suffix removal keeps underscore-only tails and stops at the first marker
    for (name, name_expect) in [
        ("name", "name"),
        ("name__", "name__"),
        ("name_1", "name"),
        ("name'", "name"),
        ("name_1'", "name"),
    ] {
        let id = id_at(name, 3);
        let id_base = id.strip_suffix();
        assert_eq!(&*id_base.node, name_expect);
        assert_eq!(id_base.span, id.span);

        // Unchanged names retain their allocation
        if name == name_expect {
            assert_eq!(id.node.as_ptr(), id_base.node.as_ptr());
        }
    }
}

#[test]
fn identifier_maps_compare_names_without_spans() {
    let id = id_at("name", 1);
    let id_other = id_at("name", 2);
    let mut map = IdMap::new();
    map.insert(id.clone(), 7);

    // Lookups keep the original key's source location
    assert_eq!(map.get(&id_other), Some(&7));
    assert_eq!(map.get_key_value(&id_other), Some((&id, &7)));
    *map.get_mut(&id_other).unwrap() = 8;
    assert!(map.contains_key(&id_other));
    assert_eq!(map.remove_entry(&id_other), Some((id, 8)));
    assert!(map.is_empty());
}

#[test]
fn function_bodies_encode_identifier_contents_in_both_modes() {
    let id = id_at("f", 2);
    let value_kind = ValueKind::Func(id);
    let json_expect = json!({
        "Func": {
            "node": "f",
            "note": null,
            "span": {
                "left": { "file": "identifiers.watsup", "line": 2, "column": 0 },
                "right": { "file": "identifiers.watsup", "line": 2, "column": 0 }
            }
        }
    });

    // Function bodies carry their identifiers even in relative encoding
    for encoding in [Encoding::ArenaRelative, Encoding::ArenaIndependent] {
        let mut arena = Arena::new();
        let json = external::encode_with(&arena, encoding, &value_kind).unwrap();
        assert_eq!(json, json_expect);
        let value_kind_decoded: ValueKind =
            external::decode_with(&mut arena, encoding, &json).unwrap();
        assert_eq!(value_kind_decoded, value_kind);
    }
}
