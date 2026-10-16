//! Natural-number aggregation builtins in specification order
//!
//! Sums, maxima, and minima over natural lists;
//! an empty list has no maximum or minimum and is an error.

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

/// A natural value; a negative integer is an error.
fn value_of_bigint(arena: &mut Arena, value: BigInt) -> Result<Value, BuiltinError> {
    let value = num::Natural::try_from(value).map_err(BuiltinError::from)?;
    let value = value::make::nat(arena, value, Span::default())?;
    Ok(value)
}

/// The elements of the single list argument.
fn input_values<'a>(arena: &'a Arena, values: &[Value]) -> Result<&'a [Value], BuiltinError> {
    let value = extract::one(values)?;
    value::get::list(arena, value).map_err(BuiltinError::from)
}

// == Built-in implementations

/// `dec $sum_nat(nat*) : nat`, the sum of the list.
pub fn sum_nat(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let mut sum = BigInt::zero();
    for value in input_values(arena, values)? {
        sum += bigint_of_value(arena, value)?;
    }
    value_of_bigint(arena, sum)
}

/// `dec $max_nat(nat*) : nat`, the largest element.
pub fn max_nat(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let values = input_values(arena, values)?;
    // An empty list has no maximum
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| BuiltinError::argument_invalid("max of empty list"))?;
    let mut maximum = bigint_of_value(arena, first)?.clone();
    for value in rest {
        let value = bigint_of_value(arena, value)?.clone();
        maximum = maximum.max(value);
    }
    value_of_bigint(arena, maximum)
}

/// `dec $min_nat(nat*) : nat`, the smallest element.
pub fn min_nat(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let values = input_values(arena, values)?;
    // An empty list has no minimum
    let (first, rest) = values
        .split_first()
        .ok_or_else(|| BuiltinError::argument_invalid("min of empty list"))?;
    let mut minimum = bigint_of_value(arena, first)?.clone();
    for value in rest {
        let value = bigint_of_value(arena, value)?.clone();
        minimum = minimum.min(value);
    }
    value_of_bigint(arena, minimum)
}
