//! Map builtins over the specification's ordered pair list
//!
//! A map value is decoded to its pair list, updated at the first matching key,
//! and encoded again.
//! Thus updating `{a: 1}` with `a: 2` preserves its list position
//! and produces `{a: 2}`.

use std::rc::Rc;

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        notation::tree::{self as notation, Mixop, parse},
        typ,
        value::flat::{self as value, Value},
    },
    traits::eq::SyntaxEq,
};

use crate::lang::il::ast::Typ;

use super::{BuiltinError, extract};

// == Value map

/// A map as its list of `k : v` pair values, in insertion order.
type ValueMap = Vec<Value>;

/// The `k : v` shape of a pair value.
fn pair_mixop() -> Rc<Mixop> {
    parse::mixop("k ':' v")
}

/// The `{ ... }` shape of a map value.
fn map_mixop() -> Rc<Mixop> {
    parse::mixop("`{ k `}")
}

/// The value under `key`, from the first matching pair.
fn map_find_opt(arena: &Arena, key: &Value, map: &[Value]) -> Option<Value> {
    let mixop_pair = pair_mixop();
    for pair in map {
        // Skip anything that is not a pair case
        let Ok(value_case) = value::get::case(arena, pair) else {
            continue;
        };
        let mixop = value_case.mixop().into_tree(&arena.mixop);
        if !mixop.syntax_eq(mixop_pair.as_ref()) {
            continue;
        }
        if let [value_key, value_value] = value_case.args()
            && value_key.view(arena).syntax_eq(&key.view(arena))
        {
            return Some(*value_value);
        }
    }
    None
}

/// Builds a `pair<K, V>` value from a key and a value.
fn make_pair(
    arena: &mut Arena,
    typ_key: &Typ,
    typ_value: &Typ,
    value_key: Value,
    value_value: Value,
) -> Result<Value, BuiltinError> {
    let pair_id = crate::phrase!(node: "pair".to_owned(), span: Span::default());
    let typ = typ::make::var(pair_id, vec![typ_key.clone(), typ_value.clone()]);
    let value_case = notation::Mixfix::new(pair_mixop(), vec![value_key, value_value])
        .expect("the pair mixop has exactly two arguments");
    Ok(value::make::case(arena, typ.node.into(), value_case, Span::default())?)
}

/// Replaces the value of the first pair with `key`, or appends a new pair.
fn map_update(
    arena: &mut Arena,
    typ_key: &Typ,
    typ_value: &Typ,
    key: &Value,
    value: &Value,
    map: &[Value],
) -> Result<ValueMap, BuiltinError> {
    let mut found = false;
    let mut updated = Vec::with_capacity(map.len() + 1);
    let mixop_pair = pair_mixop();
    for pair in map {
        let matching = value::get::case(arena, pair).ok().is_some_and(|value_case| {
            let mixop = value_case.mixop().into_tree(&arena.mixop);
            if !mixop.syntax_eq(mixop_pair.as_ref()) {
                return false;
            }
            matches!(value_case.args(), [value_key, _] if value_key.view(arena).syntax_eq(&key.view(arena)))
        });
        // Replace in place once; later duplicates are kept as they are
        if !found && matching {
            updated.push(make_pair(arena, typ_key, typ_value, *key, *value)?);
            found = true;
        } else {
            updated.push(*pair);
        }
    }
    // A new key goes at the end
    if !found {
        updated.push(make_pair(arena, typ_key, typ_value, *key, *value)?);
    }
    Ok(updated)
}

// == Conversion between meta-maps and runtime lists

/// Decodes a `map<K, V>` value into its pair list.
fn map_of_value(arena: &Arena, value: &Value) -> Result<ValueMap, BuiltinError> {
    let value_case = value::get::case(arena, value)
        .map_err(|_| BuiltinError::argument_invalid("expected a map"))?;
    let mixop_map = map_mixop();
    // The value must be a map case wrapping one list
    let mixop = value_case.mixop().into_tree(&arena.mixop);
    if !mixop.syntax_eq(mixop_map.as_ref()) {
        return Err(BuiltinError::argument_invalid("expected a map"));
    }
    let value_pairs = extract::one(value_case.args())?;
    value::get::list(arena, value_pairs)
        .map(<[Value]>::to_vec)
        .map_err(|_| BuiltinError::argument_invalid("expected a map"))
}

/// Encodes a pair list as a `map<K, V>` value.
fn value_of_map(
    arena: &mut Arena,
    typ_key: &Typ,
    typ_value: &Typ,
    map: ValueMap,
) -> Result<Value, BuiltinError> {
    // The pair list is typed `pair<K, V>*`, the case `map<K, V>`
    let pair_id = crate::phrase!(node: "pair".to_owned(), span: Span::default());
    let typ_pair = typ::make::var(pair_id, vec![typ_key.clone(), typ_value.clone()]);
    let typ_pairs = typ::make::list(typ_pair);
    let value_pairs = value::make::list(arena, typ_pairs.node.into(), map, Span::default())?;
    let map_id = crate::phrase!(node: "map".to_owned(), span: Span::default());
    let typ = typ::make::var(map_id, vec![typ_key.clone(), typ_value.clone()]);
    let value_case = notation::Mixfix::new(map_mixop(), vec![value_pairs])
        .expect("the map mixop has exactly one argument");
    let value = value::make::case(arena, typ.node.into(), value_case, Span::default())?;
    Ok(value)
}

// == Built-in implementations

/// `dec $find_map<K, V>(map<K, V>, K) : V?`,
/// the value under the key, if present.
pub fn find_map(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let (_typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_key) = extract::two(values)?;
    let map = map_of_value(arena, value_map)?;
    let typ_opt = typ::make::opt(typ_value.clone());
    let value_opt = map_find_opt(arena, value_key, &map);
    let value = value::make::opt(arena, typ_opt.node.into(), value_opt, Span::default())?;
    Ok(value)
}

/// `dec $find_maps<K, V>(map<K, V>*, K) : V?`,
/// the value under the key in the first map that has it.
pub fn find_maps(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    let (_typ_key, typ_value) = extract::two(targs)?;
    let (value_maps, value_key) = extract::two(values)?;
    let values = value::get::list(arena, value_maps).map_err(BuiltinError::from)?;
    let mut value_opt = None;
    for value_map in values {
        let map = map_of_value(arena, value_map)?;
        // Earlier maps shadow later ones; all are still decoded
        if value_opt.is_none() {
            value_opt = map_find_opt(arena, value_key, &map);
        }
    }
    let typ_opt = typ::make::opt(typ_value.clone());
    let value = value::make::opt(arena, typ_opt.node.into(), value_opt, Span::default())?;
    Ok(value)
}

/// `dec $add_map<K, V>(map<K, V>, K, V) : map<K, V>`,
/// the map with the key bound, replacing or appending.
pub fn add_map(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let (typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_key, value_value) = extract::three(values)?;
    let map = map_of_value(arena, value_map)?;
    let map = map_update(arena, typ_key, typ_value, value_key, value_value, &map)?;
    value_of_map(arena, typ_key, typ_value, map)
}

/// `dec $adds_map<K, V>(map<K, V>, K*, V*) : map<K, V>`,
/// the map with each key bound to its value in turn.
pub fn adds_map(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let (typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_keys, value_values) = extract::three(values)?;
    let mut map = map_of_value(arena, value_map)?;
    let values_key = value::get::list(arena, value_keys)
        .map_err(BuiltinError::from)?
        .to_vec();
    let values_value = value::get::list(arena, value_values)
        .map_err(BuiltinError::from)?
        .to_vec();
    // Keys and values pair up positionally
    if values_key.len() != values_value.len() {
        return Err(BuiltinError::argument_invalid(
            "map key and value lists must have the same length",
        ));
    }
    for (value_key, value_value) in values_key.iter().zip(&values_value) {
        map = map_update(arena, typ_key, typ_value, value_key, value_value, &map)?;
    }
    value_of_map(arena, typ_key, typ_value, map)
}

/// `dec $update_map<K, V>(map<K, V>, K, V) : map<K, V>`, the same as `add_map`.
pub fn update_map(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    let (typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_key, value_value) = extract::three(values)?;
    let map = map_of_value(arena, value_map)?;
    let map = map_update(arena, typ_key, typ_value, value_key, value_value, &map)?;
    value_of_map(arena, typ_key, typ_value, map)
}
