//! Bit and integer builtins in specification order
//!
//! Calls decode runtime numerics, apply the bounded arithmetic operation,
//! and return one encoded result.
//! For example, unsigned bits `[true, false]` decode to the integer `2`.
//! Bit strings are integers;
//! bit arrays are lists of booleans, most significant first.
//! Widths are capped at `MAX_BIT_WIDTH`.

use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

use crate::lang::{
    common::{prim::num, source::Span},
    data::{
        arena::Arena,
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::lang::il::ast::Typ;

use super::{BuiltinError, extract};

// == Maximum bit width

/// Largest width a shift or bit string may have.
const MAX_BIT_WIDTH: usize = 2048;

// == Conversion between meta-bits and bit vectors

/// The booleans of a bit-array value.
fn bits_of_value(arena: &Arena, value: &Value) -> Result<Vec<bool>, BuiltinError> {
    let values = value::get::list(arena, value).map_err(BuiltinError::from)?;
    let mut bits = Vec::with_capacity(values.len());
    for value in values {
        let bit = value::get::bool(arena, value).map_err(BuiltinError::from)?;
        bits.push(bit);
    }
    Ok(bits)
}

/// A `bit` list value from booleans.
fn value_of_bits(arena: &mut Arena, bits: Vec<bool>) -> Result<Value, BuiltinError> {
    let bit_id = crate::phrase!(node: "bit".to_owned(), span: Span::default());
    let typ = typ::make::var(bit_id, Vec::new());
    let mut bit_values = Vec::with_capacity(bits.len());
    for bit in bits {
        let bit_value = value::make::bool(arena, bit, Span::default())?;
        bit_values.push(bit_value);
    }
    let value = value::make::list(arena, typ.node.into(), bit_values, Span::default())?;
    Ok(value)
}

// == Conversion between meta-numerics and runtime numerics

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

/// A width as `usize`: negative clamps to zero, over the cap is an error.
fn width_of_bigint(width: &BigInt, too_large: &'static str) -> Result<usize, BuiltinError> {
    if width > &BigInt::from(MAX_BIT_WIDTH) {
        return Err(BuiltinError::argument_invalid(too_large));
    }
    if width <= &BigInt::zero() {
        return Ok(0);
    }
    width
        .to_usize()
        .ok_or_else(|| BuiltinError::argument_invalid(too_large))
}

/// A bit-array width; unlike shifts, negative is an error.
fn array_width_of_bigint(width: &BigInt) -> Result<usize, BuiltinError> {
    if width < &BigInt::zero() {
        return Err(BuiltinError::argument_invalid("negative bit array width"));
    }
    width_of_bigint(width, "bitstr width too large")
}

/// `2^width`, with `1` for non-positive widths.
fn pow2_value(width: &BigInt) -> Result<BigInt, BuiltinError> {
    if width <= &BigInt::zero() {
        return Ok(BigInt::one());
    }
    let width = width
        .to_usize()
        .ok_or_else(|| BuiltinError::argument_invalid("shift amount too large"))?;
    let value = BigInt::one() << width;
    Ok(value)
}

// == Built-in implementations

/// `dec $shl(int, int) : int`, the base shifted left.
pub fn shl(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_base, value_offset) = extract::two(values)?;
    let base = bigint_of_value(arena, value_base)?;
    let offset = bigint_of_value(arena, value_offset)?;
    let offset = width_of_bigint(offset, "shift amount too large")?;
    let value = base << offset;
    value_of_bigint(arena, value)
}

/// `dec $shr(int, int) : int`, the base shifted right, rounding toward zero.
pub fn shr(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_base, value_offset) = extract::two(values)?;
    let base = bigint_of_value(arena, value_base)?;
    let offset = bigint_of_value(arena, value_offset)?;
    let offset = width_of_bigint(offset, "shift amount too large")?;
    let divisor = BigInt::one() << offset;
    let value = base / divisor;
    value_of_bigint(arena, value)
}

/// `dec $shr_arith(int, int, int) : int`,
/// an arithmetic right shift that adds `modulus` per step.
pub fn shr_arith(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_base, value_offset, value_modulus) = extract::three(values)?;
    let base = bigint_of_value(arena, value_base)?;
    let mut base = base.clone();
    let offset = bigint_of_value(arena, value_offset)?;
    let offset = width_of_bigint(offset, "shift amount too large")?;
    let modulus = bigint_of_value(arena, value_modulus)?;
    // One step halves and re-adds the modulus, keeping the sign extension
    for _ in 0..offset {
        base = base / 2 + modulus;
    }
    value_of_bigint(arena, base)
}

/// `dec $pow2(int) : int`, two to the power.
pub fn pow2(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let value_width = extract::one(values)?;
    let width = bigint_of_value(arena, value_width)?;
    let value = pow2_value(width)?;
    value_of_bigint(arena, value)
}

/// `dec $bitstr_to_int(int, bitstr) : int`,
/// the bit string read as a signed `width`-bit integer.
pub fn bitstr_to_int(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_width, value_bitstr) = extract::two(values)?;
    let width = bigint_of_value(arena, value_width)?;
    let width = width_of_bigint(width, "bitstr width too large")?;
    // Wrap into the signed range: shift by half, reduce, shift back
    if width == 0 {
        return value_of_bigint(arena, BigInt::zero());
    }
    let modulus = BigInt::one() << width;
    let half = &modulus / 2;
    let bitstr = bigint_of_value(arena, value_bitstr)?;
    let normalized = ((bitstr + &half) % &modulus + &modulus) % &modulus - half;
    value_of_bigint(arena, normalized)
}

/// `dec $int_to_bitstr(int, int) : bitstr`,
/// the integer wrapped into `width` unsigned bits.
pub fn int_to_bitstr(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_width, value_int) = extract::two(values)?;
    let width = bigint_of_value(arena, value_width)?;
    let width = width_of_bigint(width, "bitstr width too large")?;
    // Wrap into the unsigned range, mapping negatives up
    if width == 0 {
        return value_of_bigint(arena, BigInt::zero());
    }
    let modulus = BigInt::one() << width;
    let rawint = bigint_of_value(arena, value_int)?;
    let normalized = (rawint % &modulus + &modulus) % modulus;
    value_of_bigint(arena, normalized)
}

/// Reads big-endian bits as an unsigned integer.
fn bits_to_int_unsigned_value(bits: &[bool]) -> BigInt {
    bits.iter()
        .fold(BigInt::zero(), |value, bit| (value << 1) + usize::from(*bit))
}

/// `dec $bits_to_int_unsigned(bool*) : int`, the bits read as unsigned.
pub fn bits_to_int_unsigned(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let value_bits = extract::one(values)?;
    let bits = bits_of_value(arena, value_bits)?;
    let value = bits_to_int_unsigned_value(&bits);
    value_of_bigint(arena, value)
}

/// `dec $bits_to_int_signed(bool*) : int`, the bits read as two's complement.
pub fn bits_to_int_signed(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let value_bits = extract::one(values)?;
    let bits = bits_of_value(arena, value_bits)?;
    // The first bit is the sign; there must be one
    let Some(sign) = bits.first() else {
        return Err(BuiltinError::argument_invalid("empty bit array"));
    };
    let mut value = bits_to_int_unsigned_value(&bits);
    // A set sign bit means the value is `2^n` too high
    if *sign {
        value -= BigInt::one() << bits.len();
    }
    value_of_bigint(arena, value)
}

/// The low `width` bits of an integer, most significant first.
fn int_to_bits_unsigned_value(value: &BigInt, width: usize) -> Vec<bool> {
    (0..width)
        .rev()
        .map(|index| ((value >> index) & BigInt::one()) > BigInt::zero())
        .collect()
}

/// `dec $int_to_bits_unsigned(int) : bool*`, the low `width` bits.
pub fn int_to_bits_unsigned(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_width, value_int) = extract::two(values)?;
    let width = bigint_of_value(arena, value_width)?;
    let width = array_width_of_bigint(width)?;
    let value = bigint_of_value(arena, value_int)?;
    let bits = int_to_bits_unsigned_value(value, width);
    value_of_bits(arena, bits)
}

/// `dec $int_to_bits_signed(int) : bool*`,
/// the low `width` bits in two's complement.
pub fn int_to_bits_signed(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_width, value_int) = extract::two(values)?;
    let width = bigint_of_value(arena, value_width)?;
    let width = array_width_of_bigint(width)?;
    let value = bigint_of_value(arena, value_int)?;
    // Masking a negative integer yields its two's complement bits
    let mask = (BigInt::one() << width) - 1;
    let value = value & mask;
    let bits = int_to_bits_unsigned_value(&value, width);
    value_of_bits(arena, bits)
}

/// `dec $bneg(int) : int`, the bitwise complement.
pub fn bneg(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let value = extract::one(values)?;
    let rawint = bigint_of_value(arena, value)?;
    value_of_bigint(arena, !rawint)
}

/// `dec $band(int, int) : int`, the bitwise and.
pub fn band(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_l, value_r) = extract::two(values)?;
    let rawint_l = bigint_of_value(arena, value_l)?;
    let rawint_r = bigint_of_value(arena, value_r)?;
    value_of_bigint(arena, rawint_l & rawint_r)
}

/// `dec $bxor(int, int) : int`, the bitwise exclusive or.
pub fn bxor(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_l, value_r) = extract::two(values)?;
    let rawint_l = bigint_of_value(arena, value_l)?;
    let rawint_r = bigint_of_value(arena, value_r)?;
    value_of_bigint(arena, rawint_l ^ rawint_r)
}

/// `dec $bor(int, int) : int`, the bitwise or.
pub fn bor(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_l, value_r) = extract::two(values)?;
    let rawint_l = bigint_of_value(arena, value_l)?;
    let rawint_r = bigint_of_value(arena, value_r)?;
    value_of_bigint(arena, rawint_l | rawint_r)
}

/// `dec $bitacc(int, int, int) : int`,
/// bits `h` down to `l` of `b`, as an integer.
pub fn bitacc(arena: &mut Arena, targs: &[Typ], values: &[Value]) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_b, value_h, value_l) = extract::three(values)?;
    let rawint_b = bigint_of_value(arena, value_b)?;
    let rawint_h = bigint_of_value(arena, value_h)?;
    let rawint_l = bigint_of_value(arena, value_l)?;
    // Shift the low end to bit zero, then mask the slice width
    // The slice must start at a non-negative, representable bit
    if rawint_l < &BigInt::zero() {
        return Err(BuiltinError::argument_invalid("bitslice x[y:z] must have y > z > 0"));
    }
    let low = rawint_l
        .to_usize()
        .ok_or_else(|| BuiltinError::argument_invalid("bitslice index too large"))?;
    let slice_width = rawint_h + 1 - rawint_l;
    let mask = pow2_value(&slice_width)? - 1;
    let shifted = rawint_b >> low;
    let value = shifted & mask;
    value_of_bigint(arena, value)
}

/// `dec $bitacc_replace(int, int, int, int) : int`,
/// `b` with bits `h` down to `l` replaced by `rhs`.
pub fn bitacc_replace(
    arena: &mut Arena,
    targs: &[Typ],
    values: &[Value],
) -> Result<Value, BuiltinError> {
    extract::zero(targs)?;
    let (value_b, value_h, value_l, value_rhs) = extract::four(values)?;
    let rawint_b = bigint_of_value(arena, value_b)?;
    let rawint_h = bigint_of_value(arena, value_h)?;
    let rawint_l = bigint_of_value(arena, value_l)?;
    let rawint_rhs = bigint_of_value(arena, value_rhs)?;
    // The slice must start at a non-negative, representable bit
    if rawint_l < &BigInt::zero() {
        return Err(BuiltinError::argument_invalid("bitslice x[y:z] must have y > z > 0"));
    }
    let low = rawint_l
        .to_usize()
        .ok_or_else(|| BuiltinError::argument_invalid("bitslice index too large"))?;
    // Clear the slice with a mask of ones outside `h..l`, then or in `rhs`
    let rhs = rawint_rhs << low;
    let mask_hi_width = rawint_h + 1;
    let mask_hi: BigInt = pow2_value(&mask_hi_width)? - BigInt::one();
    let mask_lo: BigInt = pow2_value(rawint_l)? - BigInt::one();
    let mask: BigInt = !(mask_hi ^ mask_lo);
    let value = (rawint_b & mask) ^ rhs;
    value_of_bigint(arena, value)
}
