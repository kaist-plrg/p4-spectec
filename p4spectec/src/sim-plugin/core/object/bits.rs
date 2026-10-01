//! Bit vectors as the packet representation
//!
//! Packets are `Vec<bool>`, most significant bit first;
//! hex text converts four bits per digit.

use num_bigint::BigInt;
use num_traits::Zero;

use crate::runner::ExternError;

use crate::sim_plugin::error;

// Bit manipulation

/// Decodes hex text into bits, four per digit.
pub fn string_to_bits(text: &str) -> Result<Vec<bool>, ExternError> {
    let mut bits = Vec::with_capacity(text.len().saturating_mul(4));
    for char in text.chars() {
        let int = char.to_digit(16).ok_or_else(|| {
            error::packet_digit_invalid(format!("invalid hexadecimal packet digit: {char}"))
        })?;
        // Most significant bit of the nibble first
        for idx in (0..4).rev() {
            bits.push(int & (1 << idx) != 0);
        }
    }
    Ok(bits)
}

/// Encodes bits as uppercase hex, padding a trailing partial nibble with zeros.
pub fn bits_to_string(bits: &[bool]) -> String {
    bits.chunks(4)
        .map(|bits| {
            // A short last chunk is left-aligned
            let int = bits
                .iter()
                .fold(0_u8, |int, bit| (int << 1) | u8::from(*bit))
                << (4 - bits.len());
            char::from(b"0123456789ABCDEF"[usize::from(int)])
        })
        .collect()
}

/// Reads bits as an unsigned integer.
pub fn bits_to_int_unsigned(bits: &[bool]) -> BigInt {
    bits.iter()
        .fold(BigInt::zero(), |int, bit| (int << 1) + u8::from(*bit))
}
