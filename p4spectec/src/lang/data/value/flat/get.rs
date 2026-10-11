//! Projections that read a value of a given kind
//!
//! Typed getters report a kind mismatch; case matching borrows arguments.
//! Fixed-arity getters project argument slices without allocating.

use std::rc::Rc;

use crate::util::json::json;

use crate::lang::{common::prim::num::Number, data::arena::Arena};

use super::super::{ValueError, ValueTag};
use super::{Value, ValueCase, ValueField, ValueKind};

// - Errors

/// Reports the actual kind of a body when a projection expects another kind.
fn kind_mismatch(kind: &ValueKind, expected: ValueTag) -> ValueError {
    ValueError::KindMismatch { expected, actual: kind.tag() }
}

// - Primitives

/// The boolean in a value.
pub fn bool(arena: &Arena, value: &Value) -> Result<bool, ValueError> {
    match arena.value.kind(value.node) {
        ValueKind::Bool(value) => Ok(*value),
        kind => Err(kind_mismatch(kind, ValueTag::Bool)),
    }
}

/// The number in a value.
pub fn num<'a>(arena: &'a Arena, value: &Value) -> Result<&'a Number, ValueError> {
    match arena.value.kind(value.node) {
        ValueKind::Num(value) => Ok(value),
        kind => Err(kind_mismatch(kind, ValueTag::Num)),
    }
}

/// The text in a value.
pub fn text<'a>(arena: &'a Arena, value: &Value) -> Result<&'a str, ValueError> {
    match arena.value.kind(value.node) {
        ValueKind::Text(value) => Ok(value),
        kind => Err(kind_mismatch(kind, ValueTag::Text)),
    }
}

// - Structures

/// The fields of a struct value.
pub fn structure<'a>(arena: &'a Arena, value: &Value) -> Result<&'a [ValueField], ValueError> {
    match arena.value.kind(value.node) {
        ValueKind::Struct(value_fields) => Ok(value_fields),
        kind => Err(kind_mismatch(kind, ValueTag::Struct)),
    }
}

// - Cases

/// The shape and arguments of a case value.
pub fn case<'a>(arena: &'a Arena, value: &Value) -> Result<&'a ValueCase, ValueError> {
    match arena.value.kind(value.node) {
        ValueKind::Case(value_case) => Ok(value_case),
        kind => Err(kind_mismatch(kind, ValueTag::Case)),
    }
}

// - Sequences

/// The components of a tuple value.
pub fn tuple<'a>(arena: &'a Arena, value: &Value) -> Result<&'a [Value], ValueError> {
    match arena.value.kind(value.node) {
        ValueKind::Tuple(values) => Ok(values),
        kind => Err(kind_mismatch(kind, ValueTag::Tuple)),
    }
}

/// The content of an option value.
pub fn opt(arena: &Arena, value: &Value) -> Result<Option<Value>, ValueError> {
    match arena.value.kind(value.node) {
        ValueKind::Opt(value) => Ok(*value),
        kind => Err(kind_mismatch(kind, ValueTag::Opt)),
    }
}

/// The elements of a list value.
pub fn list<'a>(arena: &'a Arena, value: &Value) -> Result<&'a [Value], ValueError> {
    match arena.value.kind(value.node) {
        ValueKind::List(values) => Ok(values),
        kind => Err(kind_mismatch(kind, ValueTag::List)),
    }
}

// - Externals

/// The JSON of a host-owned value.
pub fn external<'a>(arena: &'a Arena, value: &Value) -> Result<&'a Rc<json>, ValueError> {
    match arena.value.kind(value.node) {
        ValueKind::Extern(json) => Ok(json),
        kind => Err(kind_mismatch(kind, ValueTag::Extern)),
    }
}

// - Indexing

/// The element at `index`, or an out-of-bounds error.
pub fn nth(values: &[Value], index: usize) -> Result<&Value, ValueError> {
    values
        .get(index)
        .ok_or(ValueError::IndexOutOfBounds { index, len: values.len() })
}

// - Arity

/// Exactly one value.
pub fn one(values: &[Value]) -> Result<&Value, ValueError> {
    match values {
        [value] => Ok(value),
        _ => Err(ValueError::CountMismatch { expected: 1, actual: values.len() }),
    }
}

/// Exactly two values.
pub fn two(values: &[Value]) -> Result<(&Value, &Value), ValueError> {
    match values {
        [value_a, value_b] => Ok((value_a, value_b)),
        _ => Err(ValueError::CountMismatch { expected: 2, actual: values.len() }),
    }
}

/// Exactly three values.
#[allow(clippy::type_complexity)]
pub fn three(values: &[Value]) -> Result<(&Value, &Value, &Value), ValueError> {
    match values {
        [value_a, value_b, value_c] => Ok((value_a, value_b, value_c)),
        _ => Err(ValueError::CountMismatch { expected: 3, actual: values.len() }),
    }
}

/// Exactly four values.
#[allow(clippy::type_complexity)]
pub fn four(values: &[Value]) -> Result<(&Value, &Value, &Value, &Value), ValueError> {
    match values {
        [value_a, value_b, value_c, value_d] => Ok((value_a, value_b, value_c, value_d)),
        _ => Err(ValueError::CountMismatch { expected: 4, actual: values.len() }),
    }
}
