//! Set builtins backed by a collection with actual set semantics
//!
//! Runtime syntax is decoded into a sorted, deduplicated list,
//! operated on, and encoded back in syntax order.
//! For example, the union of `{a}` and `{a, b}` is emitted once as `{a, b}`.

use std::borrow::Cow;

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        notation::flat::{self as notation, Mixop, parse::ToMixop},
        typ,
        value::flat::{self as value, Value},
    },
    traits::{cmp::SyntaxCmp, eq::SyntaxEq},
};

use crate::lang::il::ast::Typ;

use super::{BuiltinError, extract};

// == Value set

/// A set as a sorted list without duplicates.
struct ValueSet<'a> {
    values: Cow<'a, [Value]>,
}

/// The `{ ... }` shape of a set value.
fn set_mixop(arena: &mut Arena) -> Result<Mixop, BuiltinError> {
    "`{ k `}"
        .to_mixop(&mut arena.mixop)
        .map_err(BuiltinError::from)
}

impl<'a> ValueSet<'a> {
    // == Operations

    /// Sorts by syntax and drops syntactic duplicates.
    fn sort(&mut self, arena: &Arena) {
        let values = self.values.to_mut();
        values.sort_by(|value_a, value_b| value_a.view(arena).syntax_cmp(&value_b.view(arena)));
        values.dedup_by(|value_a, value_b| value_a.view(arena).syntax_eq(&value_b.view(arena)));
    }

    /// Finds an element by binary search in the sorted set.
    fn contains(&self, arena: &Arena, value: &Value) -> bool {
        self.values
            .binary_search_by(|value_elem| value_elem.view(arena).syntax_cmp(&value.view(arena)))
            .is_ok()
    }

    /// Keeps the elements present in both sets.
    fn intersection(self, arena: &Arena, set: &ValueSet<'_>) -> ValueSet<'static> {
        let mut intersection = self.into_owned();
        intersection
            .values
            .to_mut()
            .retain(|value| set.contains(arena, value));
        intersection
    }

    /// Combines both sets in syntax order, keeping the first equal representative.
    fn union(self, arena: &Arena, set: &ValueSet<'_>) -> ValueSet<'static> {
        let mut union = self.into_owned();
        union.values.to_mut().extend(set.values.iter().copied());
        union.sort(arena);
        union
    }

    /// Combines runtime sets, sorting the accumulated elements once.
    fn unions(
        arena: &Arena,
        mixop_set: Mixop,
        values: &[Value],
    ) -> Result<ValueSet<'static>, BuiltinError> {
        let mut union = ValueSet { values: Cow::Owned(Vec::new()) };
        // Validate and normalize every input before accumulating its elements
        for value in values {
            let set = ValueSet::from_value(arena, mixop_set, value)?;
            union.values.to_mut().extend(set.values.iter().copied());
        }
        // Keep the first representative across sets in syntax order
        union.sort(arena);
        Ok(union)
    }

    /// Keeps the elements absent from the other set.
    fn difference(self, arena: &Arena, set: &ValueSet<'_>) -> ValueSet<'static> {
        let mut difference = self.into_owned();
        difference
            .values
            .to_mut()
            .retain(|value| !set.contains(arena, value));
        difference
    }

    /// Checks whether every element occurs in the other set.
    fn is_subset(&self, arena: &Arena, set: &ValueSet<'_>) -> bool {
        self.values.iter().all(|value| set.contains(arena, value))
    }

    /// Compares normalized sets by their elements' syntax.
    fn equals(&self, arena: &Arena, set: &ValueSet<'_>) -> bool {
        self.values.len() == set.values.len()
            && self
                .values
                .iter()
                .zip(set.values.iter())
                .all(|(value_a, value_b)| value_a.view(arena).syntax_eq(&value_b.view(arena)))
    }

    // == Conversion between meta-sets and runtime lists

    /// Borrows normalized elements or sorts and deduplicates an owned copy.
    fn from_value(arena: &'a Arena, mixop_set: Mixop, value: &Value) -> Result<Self, BuiltinError> {
        let value_case = value::get::case(arena, value)
            .map_err(|_| BuiltinError::argument_invalid("expected a set"))?;
        // The value must be a set case wrapping one list
        if arena.mixop.canon_id(*value_case.mixop()) != arena.mixop.canon_id(mixop_set) {
            return Err(BuiltinError::argument_invalid("expected a set"));
        }
        let value_set = extract::one(value_case.args())?;
        let values = value::get::list(arena, value_set)
            .map_err(|_| BuiltinError::argument_invalid("expected a set"))?;

        // Strict syntax order implies both sorting and absence of duplicates
        let sorted = values.windows(2).all(|values| {
            values[0]
                .view(arena)
                .syntax_cmp(&values[1].view(arena))
                .is_lt()
        });
        let mut set = Self { values: Cow::Borrowed(values) };
        if !sorted {
            // Preserve the arena's original order and duplicate elements
            set.sort(arena);
        }
        Ok(set)
    }

    /// Copies borrowed elements so the result can be allocated in their arena.
    fn into_owned(self) -> ValueSet<'static> {
        ValueSet { values: Cow::Owned(self.values.into_owned()) }
    }
}

impl ValueSet<'static> {
    /// Encodes a set as a `set<K>` value.
    fn into_value(self, arena: &mut Arena, typ_key: &Typ) -> Result<Value, BuiltinError> {
        // The element list is typed `K*`, the case `set<K>`
        let values_elem = self.values.into_owned();
        let typ_list = typ::make::list(typ_key.clone());
        let value_set =
            value::make::list(arena, typ_list.node.into(), values_elem, Span::default())?;
        let set_id = crate::phrase!(node: "set".into(), span: Span::default());
        let typ = typ::make::var(set_id, vec![typ_key.clone()]);
        let mixop_set = set_mixop(arena)?;
        let value_case = notation::Mixfix::new(&arena.mixop, mixop_set, vec![value_set])
            .expect("the set mixop has exactly one argument");
        let value = value::make::case_flat(arena, typ.node.into(), value_case, Span::default())?;
        Ok(value)
    }
}

// == Built-in implementations

/// `dec $intersect_set<K>(set<K>, set<K>) : set<K>`, the elements in both.
pub fn intersect_set(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    let mixop_set = set_mixop(arena)?;
    let typ_key = extract::one(targs)?;
    let (value_set_l, value_set_r) = extract::two(values)?;
    let set_l = ValueSet::from_value(arena, mixop_set, value_set_l)?;
    let set_r = ValueSet::from_value(arena, mixop_set, value_set_r)?;
    let intersection = set_l.intersection(arena, &set_r);
    intersection.into_value(arena, typ_key)
}

/// `dec $union_set<K>(set<K>, set<K>) : set<K>`, the elements in either.
pub fn union_set(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    let mixop_set = set_mixop(arena)?;
    let typ_key = extract::one(targs)?;
    let (value_set_l, value_set_r) = extract::two(values)?;
    let set_l = ValueSet::from_value(arena, mixop_set, value_set_l)?;
    let set_r = ValueSet::from_value(arena, mixop_set, value_set_r)?;
    let union = set_l.union(arena, &set_r);
    union.into_value(arena, typ_key)
}

/// `dec $unions_set<K>(set<K>*) : set<K>`, the elements in any of the sets.
pub fn unions_set(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    let mixop_set = set_mixop(arena)?;
    let typ_key = extract::one(targs)?;
    let value_sets = extract::one(values)?;
    let values = value::get::list(arena, value_sets).map_err(BuiltinError::from)?;
    let union = ValueSet::unions(arena, mixop_set, values)?;
    union.into_value(arena, typ_key)
}

/// `dec $diff_set<K>(set<K>, set<K>) : set<K>`,
/// the elements of the first not in the second.
pub fn diff_set(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let mixop_set = set_mixop(arena)?;
    let typ_key = extract::one(targs)?;
    let (value_set_l, value_set_r) = extract::two(values)?;
    let set_l = ValueSet::from_value(arena, mixop_set, value_set_l)?;
    let set_r = ValueSet::from_value(arena, mixop_set, value_set_r)?;
    let difference = set_l.difference(arena, &set_r);
    difference.into_value(arena, typ_key)
}

/// `dec $sub_set<K>(set<K>, set<K>) : bool`,
/// whether the first is a subset of the second.
pub fn sub_set(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let mixop_set = set_mixop(arena)?;
    let _typ_key = extract::one(targs)?;
    let (value_set_l, value_set_r) = extract::two(values)?;
    let set_l = ValueSet::from_value(arena, mixop_set, value_set_l)?;
    let set_r = ValueSet::from_value(arena, mixop_set, value_set_r)?;
    let is_subset = set_l.is_subset(arena, &set_r);
    let value = value::make::bool(arena, is_subset, Span::default())?;
    Ok(value)
}

/// `dec $eq_set<K>(set<K>, set<K>) : bool`,
/// whether both have the same elements.
pub fn eq_set(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    let mixop_set = set_mixop(arena)?;
    let _typ_key = extract::one(targs)?;
    let (value_set_l, value_set_r) = extract::two(values)?;
    let set_l = ValueSet::from_value(arena, mixop_set, value_set_l)?;
    let set_r = ValueSet::from_value(arena, mixop_set, value_set_r)?;
    let equal = set_l.equals(arena, &set_r);
    let value = value::make::bool(arena, equal, Span::default())?;
    Ok(value)
}
