//! Integer aggregation builtins in specification order
//!
//! Sums, maxima, and minima over integer lists; an empty list yields `0`.

use num_bigint::BigInt;
use num_traits::Zero;

use crate::lang::{
    common::{prim::num, source::Span},
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
};

use crate::lang::il::ast::Typ;

use super::{BuiltinError, extract};

// == Conversion between meta-numerics and Rust numerics

/// The integer in a number value.
fn bigint_of_value<'a>(arena: &'a Arena, value: &Value) -> Result<&'a BigInt, BuiltinError> {
    let num = value::get::num(arena, value).map_err(BuiltinError::from)?;
    Ok(num::to_int(num))
}

/// An integer value.
fn value_of_bigint(arena: &mut Arena, value: BigInt) -> Result<Value, BuiltinError> {
    let value = value::make::int(arena, value, Span::default())?;
    Ok(value)
}

/// The elements of the single list argument.
fn input_values<'a>(arena: &'a Arena, values: &[Value]) -> Result<&'a [Value], BuiltinError> {
    let value = extract::one(values)?;
    value::get::list(arena, value).map_err(BuiltinError::from)
}

// == Built-in implementations

/// `dec $sum_int(nat*) : nat`, the sum of the list.
pub fn sum_int(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let mut sum = BigInt::zero();
    for value in input_values(arena, values)? {
        sum += bigint_of_value(arena, value)?;
    }
    value_of_bigint(arena, sum)
}

/// `dec $max_int(int*) : int`, the largest element, `0` for an empty list.
pub fn max_int(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let values = input_values(arena, values)?;
    // Start from the first element, or zero when there is none
    let mut maximum = match values.split_first() {
        Some((first, _)) => {
            let first = bigint_of_value(arena, first)?;
            first.clone()
        }
        None => BigInt::zero(),
    };
    for value in values.get(1..).unwrap_or_default() {
        let value = bigint_of_value(arena, value)?.clone();
        maximum = maximum.max(value);
    }
    value_of_bigint(arena, maximum)
}

/// `dec $min_int(int*) : int`, the smallest element, `0` for an empty list.
pub fn min_int(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let values = input_values(arena, values)?;
    // Start from the first element, or zero when there is none
    let mut minimum = match values.split_first() {
        Some((first, _)) => {
            let first = bigint_of_value(arena, first)?;
            first.clone()
        }
        None => BigInt::zero(),
    };
    for value in values.get(1..).unwrap_or_default() {
        let value = bigint_of_value(arena, value)?.clone();
        minimum = minimum.min(value);
    }
    value_of_bigint(arena, minimum)
}
