//! Map builtins over the specification's ordered pair list
//!
//! A map value is decoded to its pair list, updated at the first matching key,
//! and encoded again.
//! Thus updating `{a: 1}` with `a: 2` preserves its list position
//! and produces `{a: 2}`.

use std::borrow::Cow;

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
struct ValueMap<'a> {
    pairs: Cow<'a, [Value]>,
}

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

impl<'a> ValueMap<'a> {
    // == Operations

    /// The value under `key`, from the first matching pair.
    fn find(&self, arena: &Arena, mixop_pair: Mixop, key: &Value) -> Option<Value> {
        for pair in self.pairs.iter() {
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

    /// Replaces the value of the first pair with `key`, or appends a new pair.
    fn update(
        &mut self,
        arena: &mut Arena,
        typ_key: &Typ,
        typ_value: &Typ,
        key: &Value,
        value: &Value,
    ) -> Result<(), BuiltinError> {
        let mixop_pair = pair_mixop(arena)?;
        // Only the first matching key is replaced; later duplicates stay intact
        let idx = self.pairs.iter().position(|pair| {
            value::get::case(arena, pair)
                .ok()
                .is_some_and(|value_case| {
                    if arena.mixop.canon_id(*value_case.mixop()) != arena.mixop.canon_id(mixop_pair)
                    {
                        return false;
                    }
                    matches!(
                        value_case.args(),
                        [value_key, _] if value_key.view(arena).syntax_eq(&key.view(arena))
                    )
                })
        });
        let pair = make_pair(arena, typ_key, typ_value, *key, *value)?;
        // Reuse the owned buffer across a sequence of updates
        let pairs = self.pairs.to_mut();
        if let Some(idx) = idx {
            pairs[idx] = pair;
        } else {
            pairs.push(pair);
        }
        Ok(())
    }

    // == Conversion between meta-maps and runtime lists

    /// Decodes a `map<K, V>` value into its pair list.
    fn from_value(arena: &'a Arena, mixop_map: Mixop, value: &Value) -> Result<Self, BuiltinError> {
        let value_case = value::get::case(arena, value)
            .map_err(|_| BuiltinError::argument_invalid("expected a map"))?;
        // The value must be a map case wrapping one list
        if arena.mixop.canon_id(*value_case.mixop()) != arena.mixop.canon_id(mixop_map) {
            return Err(BuiltinError::argument_invalid("expected a map"));
        }
        let value_pairs = extract::one(value_case.args())?;
        let pairs = value::get::list(arena, value_pairs)
            .map_err(|_| BuiltinError::argument_invalid("expected a map"))?;
        Ok(Self { pairs: Cow::Borrowed(pairs) })
    }

    /// Copies borrowed pairs so updates can allocate in their arena.
    fn into_owned(self) -> ValueMap<'static> {
        ValueMap { pairs: Cow::Owned(self.pairs.into_owned()) }
    }
}

impl ValueMap<'static> {
    /// Encodes a pair list as a `map<K, V>` value.
    fn into_value(
        self,
        arena: &mut Arena,
        typ_key: &Typ,
        typ_value: &Typ,
    ) -> Result<Value, BuiltinError> {
        // The pair list is typed `pair<K, V>*`, the case `map<K, V>`
        let pair_id = crate::phrase!(node: "pair".into(), span: Span::default());
        let typ_pair = typ::make::var(pair_id, vec![typ_key.clone(), typ_value.clone()]);
        let typ_pairs = typ::make::list(typ_pair);
        let value_pairs = value::make::list(
            arena,
            typ_pairs.node.into(),
            self.pairs.into_owned(),
            Span::default(),
        )?;
        let map_id = crate::phrase!(node: "map".into(), span: Span::default());
        let typ = typ::make::var(map_id, vec![typ_key.clone(), typ_value.clone()]);
        let mixop_map = map_mixop(arena)?;
        let value_case = notation::Mixfix::new(&arena.mixop, mixop_map, vec![value_pairs])
            .expect("the map mixop has exactly one argument");
        let value = value::make::case_flat(arena, typ.node.into(), value_case, Span::default())?;
        Ok(value)
    }
}

// == Built-in implementations

/// `dec $find_map<K, V>(map<K, V>, K) : V?`,
/// the value under the key, if present.
pub fn find_map(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let mixop_map = map_mixop(arena)?;
    let mixop_pair = pair_mixop(arena)?;
    let (_typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_key) = extract::two(values)?;
    let map = ValueMap::from_value(arena, mixop_map, value_map)?;
    let typ_opt = typ::make::opt(typ_value.clone());
    let value_opt = map.find(arena, mixop_pair, value_key);
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
        let map = ValueMap::from_value(arena, mixop_map, value_map)?;
        // Earlier maps shadow later ones; all are still decoded
        if value_opt.is_none() {
            value_opt = map.find(arena, mixop_pair, value_key);
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
    let mut map = ValueMap::from_value(arena, mixop_map, value_map)?.into_owned();
    map.update(arena, typ_key, typ_value, value_key, value_value)?;
    map.into_value(arena, typ_key, typ_value)
}

/// `dec $adds_map<K, V>(map<K, V>, K*, V*) : map<K, V>`,
/// the map with each key bound to its value in turn.
pub fn adds_map(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let mixop_map = map_mixop(arena)?;
    let (typ_key, typ_value) = extract::two(targs)?;
    let (value_map, value_keys, value_values) = extract::three(values)?;
    let mut map = ValueMap::from_value(arena, mixop_map, value_map)?.into_owned();
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
        map.update(arena, typ_key, typ_value, &value_key, &value_value)?;
    }
    map.into_value(arena, typ_key, typ_value)
}

/// `dec $update_map<K, V>(map<K, V>, K, V) : map<K, V>`, the same as `add_map`.
pub fn update_map(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    add_map(arena, targs, values)
}
