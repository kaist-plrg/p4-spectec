//! Integer arithmetic and bit operations

use num_bigint::BigInt;
use num_traits::{One, Signed, ToPrimitive, Zero};

use crate::runner::ExternError;

// == Arithmetic

/// Return a nonnegative remainder; requires a positive modulus.
pub(crate) fn remainder(int: &BigInt, int_modulus: &BigInt) -> BigInt {
    let int = int % int_modulus;
    if int.is_negative() { int + int_modulus } else { int }
}

// == Bit operations

/// The width as a `usize`, or an error when it does not fit.
pub(crate) fn width_bit(width: &BigInt) -> Result<usize, ExternError> {
    width
        .to_usize()
        .ok_or_else(|| ExternError::message(format!("invalid hash bit width: {width}")))
}

/// Flip the low `width` bits, keeping higher bits unchanged.
pub fn bitwise_neg(int: &BigInt, width: &BigInt) -> Result<BigInt, ExternError> {
    if width <= &BigInt::zero() {
        return Ok(int.clone());
    }
    let width = width_bit(width)?;
    Ok(int ^ ((BigInt::one() << width) - BigInt::one()))
}
