//! Map builtins over the specification's ordered pair list
//!
//! A map value is decoded to its pair list, updated at the first matching key,
//! and encoded again.
//! Thus updating `{a: 1}` with `a: 2` preserves its list position
//! and produces `{a: 2}`.

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        notation::flat::{self as notation, Mixop, parse::ToMixop},
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
fn pair_mixop(arena: &mut Arena) -> Result<Mixop, BuiltinError> {
    "k ':' v"
        .to_mixop(&mut arena.mixop)
        .map_err(BuiltinError::from)
}

/// The `{ ... }` shape of a map value.
fn map_mixop(arena: &mut Arena) -> Result<Mixop, BuiltinError> {
    "`{ k `}"
        .to_mixop(&mut arena.mixop)
        .map_err(BuiltinError::from)
}

/// The value under `key`, from the first matching pair.
fn map_find_opt(arena: &Arena, mixop_pair: Mixop, key: &Value, map: &[Value]) -> Option<Value> {
    for pair in map {
        // Skip anything that is not a pair case
        let Ok(value_case) = value::get::case(arena, pair) else {
            continue;
        };
        if arena.mixop.canon_id(*value_case.mixop()) != arena.mixop.canon_id(mixop_pair) {
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
    let pair_id = crate::phrase!(node: "pair".into(), span: Span::default());
    let typ = typ::make::var(pair_id, vec![typ_key.clone(), typ_value.clone()]);
    let mixop_pair = pair_mixop(arena)?;
    let value_case = notation::Mixfix::new(&arena.mixop, mixop_pair, vec![value_key, value_value])
        .expect("the pair mixop has exactly two arguments");
    Ok(value::make::case_flat(arena, typ.node.into(), value_case, Span::default())?)
}

/// Replaces the value of the first pair with `key`, or appends a new pair.
fn map_update(
    arena: &mut Arena,
    typ_key: &Typ,
    typ_value: &Typ,
    key: &Value,
    value: &Value,
    mut map: ValueMap,
) -> Result<ValueMap, BuiltinError> {
    let mixop_pair = pair_mixop(arena)?;
    // Only the first matching key is replaced; later duplicates stay intact
    let idx = map.iter().position(|pair| {
        value::get::case(arena, pair).ok().is_some_and(|value_case| {
            if arena.mixop.canon_id(*value_case.mixop()) != arena.mixop.canon_id(mixop_pair) {
                return false;
            }
            matches!(value_case.args(), [value_key, _] if value_key.view(arena).syntax_eq(&key.view(arena)))
        })
    });
    let pair = make_pair(arena, typ_key, typ_value, *key, *value)?;
    // Reuse the owned buffer across a sequence of updates
    if let Some(idx) = idx {
        map[idx] = pair;
    } else {
        map.push(pair);
    }
    Ok(map)
}

// == Conversion between meta-maps and runtime lists

/// Decodes a `map<K, V>` value into its pair list.
fn map_of_value<'arena>(
    arena: &'arena Arena,
    mixop_map: Mixop,
    value: &Value,
) -> Result<&'arena [Value], BuiltinError> {
    let value_case = value::get::case(arena, value)
        .map_err(|_| BuiltinError::argument_invalid("expected a map"))?;
    // The value must be a map case wrapping one list
    if arena.mixop.canon_id(*value_case.mixop()) != arena.mixop.canon_id(mixop_map) {
        return Err(BuiltinError::argument_invalid("expected a map"));
    }
    let value_pairs = extract::one(value_case.args())?;
    value::get::list(arena, value_pairs)
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
    let pair_id = crate::phrase!(node: "pair".into(), span: Span::default());
    let typ_pair = typ::make::var(pair_id, vec![typ_key.clone(), typ_value.clone()]);
    let typ_pairs = typ::make::list(typ_pair);
    let value_pairs = value::make::list(arena, typ_pairs.node.into(), map, Span::default())?;
    let map_id = crate::phrase!(node: "map".into(), span: Span::default());
    let typ = typ::make::var(map_id, vec![typ_key.clone(), typ_value.clone()]);
    let mixop_map = map_mixop(arena)?;
    let value_case = notation::Mixfix::new(&arena.mixop, mixop_map, vec![value_pairs])
        .expect("the map mixop has exactly one argument");
    let value = value::make::case_flat(arena, typ.node.into(), value_case, Span::default())?;
    Ok(value)
}

// == Built-in implementations

/// `dec $find_map<K, V>(map<K, V>, K) : V?`,
/// the value under the key, if present.
pub fn find_map(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let mixop_map = map_mixop(arena)?;
    let mixop_pair = pair_mixop(arena)?;
    let (_typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_key) = extract::two(values)?;
    let map = map_of_value(arena, mixop_map, value_map)?;
    let typ_opt = typ::make::opt(typ_value.clone());
    let value_opt = map_find_opt(arena, mixop_pair, value_key, map);
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
    let mixop_map = map_mixop(arena)?;
    let mixop_pair = pair_mixop(arena)?;
    let (_typ_key, typ_value) = extract::two(targs)?;
    let (value_maps, value_key) = extract::two(values)?;
    let values = value::get::list(arena, value_maps).map_err(BuiltinError::from)?;
    let mut value_opt = None;
    for value_map in values {
        let map = map_of_value(arena, mixop_map, value_map)?;
        // Earlier maps shadow later ones; all are still decoded
        if value_opt.is_none() {
            value_opt = map_find_opt(arena, mixop_pair, value_key, map);
        }
    }
    let typ_opt = typ::make::opt(typ_value.clone());
    let value = value::make::opt(arena, typ_opt.node.into(), value_opt, Span::default())?;
    Ok(value)
}

/// `dec $add_map<K, V>(map<K, V>, K, V) : map<K, V>`,
/// the map with the key bound, replacing or appending.
pub fn add_map(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let mixop_map = map_mixop(arena)?;
    let (typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_key, value_value) = extract::three(values)?;
    let map = map_of_value(arena, mixop_map, value_map)?.to_vec();
    let map = map_update(arena, typ_key, typ_value, value_key, value_value, map)?;
    value_of_map(arena, typ_key, typ_value, map)
}

/// `dec $adds_map<K, V>(map<K, V>, K*, V*) : map<K, V>`,
/// the map with each key bound to its value in turn.
pub fn adds_map(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let mixop_map = map_mixop(arena)?;
    let (typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_keys, value_values) = extract::three(values)?;
    let mut map = map_of_value(arena, mixop_map, value_map)?.to_vec();
    let num_keys = value::get::list(arena, value_keys)
        .map_err(BuiltinError::from)?
        .len();
    let num_values = value::get::list(arena, value_values)
        .map_err(BuiltinError::from)?
        .len();
    // Keys and values pair up positionally
    if num_keys != num_values {
        return Err(BuiltinError::argument_invalid(
            "map key and value lists must have the same length",
        ));
    }
    // Arena lists stay immutable while updates append new values
    for idx in 0..num_keys {
        let value_key = value::get::list(arena, value_keys).expect("validated key list")[idx];
        let value_value = value::get::list(arena, value_values).expect("validated value list")[idx];
        map = map_update(arena, typ_key, typ_value, &value_key, &value_value, map)?;
    }
    value_of_map(arena, typ_key, typ_value, map)
}

/// `dec $update_map<K, V>(map<K, V>, K, V) : map<K, V>`, the same as `add_map`.
pub fn update_map(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    let mixop_map = map_mixop(arena)?;
    let (typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_key, value_value) = extract::three(values)?;
    let map = map_of_value(arena, mixop_map, value_map)?.to_vec();
    let map = map_update(arena, typ_key, typ_value, value_key, value_value, map)?;
    value_of_map(arena, typ_key, typ_value, map)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn updates_preserve_order_duplicates_and_the_input() {
        // The input deliberately contains two pairs with the same key
        let mut arena = Arena::new();
        let typ = typ::make::nat();
        let mut nat = |n: u64| value::make::nat(&mut arena, n.into(), Span::default()).unwrap();
        let [key_a, key_b, key_c, value_a, value_b, value_c, value_d, value_e, value_f] =
            [1, 2, 3, 10, 20, 30, 40, 50, 60].map(&mut nat);
        let pairs = [(key_a, value_a), (key_b, value_b), (key_a, value_c)]
            .into_iter()
            .map(|(key, value)| make_pair(&mut arena, &typ, &typ, key, value).unwrap())
            .collect();
        let value_map = value_of_map(&mut arena, &typ, &typ, pairs).unwrap();
        let typ_list = typ::make::list(typ.clone());
        let value_keys = value::make::list(
            &mut arena,
            typ_list.node.clone().into(),
            vec![key_a, key_c, key_a],
            Span::default(),
        )
        .unwrap();
        let value_values = value::make::list(
            &mut arena,
            typ_list.node.into(),
            vec![value_d, value_e, value_f],
            Span::default(),
        )
        .unwrap();
        let targs = [typ.clone(), typ];
        // Repeated updates replace the first pair without moving it
        let value_updated =
            adds_map(&mut arena, &targs, &[value_map, value_keys, value_values]).unwrap();
        let mixop_map = map_mixop(&mut arena).unwrap();
        let map = map_of_value(&arena, mixop_map, &value_updated).unwrap();
        let pairs = map
            .iter()
            .map(|pair| {
                let [key, value] = value::get::case(&arena, pair).unwrap().args() else {
                    panic!("pair")
                };
                (*key, *value)
            })
            .collect::<Vec<_>>();
        assert_eq!(pairs, [(key_a, value_f), (key_b, value_b), (key_a, value_c), (key_c, value_e)]);
        // Updating a copy must leave the original map intact
        let value_found = find_map(&mut arena, &targs, &[value_map, key_a]).unwrap();
        assert_eq!(value::get::opt(&arena, &value_found).unwrap(), Some(value_a));
    }
}
