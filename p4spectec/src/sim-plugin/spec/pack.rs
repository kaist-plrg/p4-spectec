//! Packs an IL value representing a P4 value from a Rust type
//!
//! Each constructor builds the specification's `value` case
//! for one P4 value form.

use num_bigint::BigInt;

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
};

use crate::runner::ExternError;

// == P4 values

/// `D int`, an arbitrary-precision integer.
pub fn p4_arbitrary_int(arena: &mut Arena, int: BigInt) -> Result<Value, ExternError> {
    let value_int = value::make::int(arena, int, Span::default())?;
    Ok(value::make::case_shaped! {
        arena: arena,
        shape: "D int",
        args: vec![value_int],
        typ: "value",
        span: Span::default(),
    }?)
}

/// `nat W int`, a fixed-width unsigned bit string; the width must be a natural.
pub fn p4_fixed_bit(arena: &mut Arena, width: BigInt, int: BigInt) -> Result<Value, ExternError> {
    // The width must be a natural number
    let nat = width
        .try_into()
        .map_err(|error: crate::lang::common::prim::num::NumericError| ExternError::from(error))?;
    let value_width = value::make::nat(arena, nat, Span::default())?;
    let value_int = value::make::int(arena, int, Span::default())?;
    Ok(value::make::case_shaped! {
        arena: arena,
        shape: "nat W int",
        args: vec![value_width, value_int],
        typ: "value",
        span: Span::default(),
    }?)
}

/// `tid . id`, an enum member.
pub fn p4_enum(arena: &mut Arena, id_enum: &str, id: &str) -> Result<Value, ExternError> {
    let value_enum = value::make::text(arena, id_enum.to_owned(), Span::default())?;
    let value_id = value::make::text(arena, id.to_owned(), Span::default())?;
    Ok(value::make::case_shaped! {
        arena: arena,
        shape: "tid '.' id",
        args: vec![value_enum, value_id],
        typ: "value",
        span: Span::default(),
    }?)
}
