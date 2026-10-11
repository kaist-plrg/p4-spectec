//! Packet field packing, reflected CRCs, and one's-complement checksums
//!
//! The hash externs pack their fields into one wide integer (`package`)
//! and hash it with the named algorithm (`compute_hash`).
//! CRCs are the reflected table-less form over bytes;
//! checksums fold 16-bit words with end-around carry and complement the result.

use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

use crate::util::bigint::{remainder, width_bit};

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::runner::ExternError;

use crate::sim_plugin::error;

use super::spec::unpack;

// == Bit operations

/// A width as `usize`, which must be a multiple of `alignment`.
fn width_bit_aligned(width: &BigInt, alignment: usize) -> Result<usize, ExternError> {
    let width = width_bit(width)?;
    if !width.is_multiple_of(alignment) {
        return Err(error::hash_width_invalid(format!(
            "hash width {width} must be a multiple of {alignment} bits"
        ))
        .into());
    }
    Ok(width)
}

// == Hash algorithms

/// A reflected CRC over the low `width` bits of `int`, high byte first.
fn crc(
    width: &BigInt,
    int: &BigInt,
    polynomial: u32,
    int_init: u32,
) -> Result<BigInt, ExternError> {
    let width = width_bit_aligned(width, 8)?;
    let mut int_crc = int_init;
    // Feed bytes from most to least significant
    for idx in (0..width).step_by(8).rev() {
        let byte = ((int >> idx) & BigInt::from(255_u16))
            .to_u32()
            .expect("masked CRC byte fits u32");
        // Eight reflected shift-and-xor steps per byte
        let mut entry = (int_crc ^ byte) & 255;
        for _ in 0..8 {
            entry = if entry & 1 == 0 { entry >> 1 } else { (entry >> 1) ^ polynomial };
        }
        int_crc = (int_crc >> 8) ^ entry;
    }
    // The final xor equals the initial value for both supported CRCs
    Ok(BigInt::from(int_crc ^ int_init))
}

/// The one's-complement sum of 16-bit words, complemented;
/// `subtract` folds inverted words.
fn checksum(
    width: &BigInt,
    int: &BigInt,
    int_init: &BigInt,
    subtract: bool,
) -> Result<BigInt, ExternError> {
    let width = width_bit_aligned(width, 16)?;
    let int_threshold = BigInt::one() << 16;
    let mask = &int_threshold - BigInt::one();
    let mut int_hash = int_init.clone();
    // Add each word with end-around carry
    for idx in (0..width).step_by(16).rev() {
        let int_word = (int >> idx) & &mask;
        let int_word = if subtract { int_word ^ &mask } else { int_word };
        let int_sum = int_hash + int_word;
        let carry = u8::from(int_sum >= int_threshold);
        int_hash = remainder(&int_sum, &int_threshold) + carry;
    }
    // Complement only the low 16 bits, retaining any higher seed bits
    Ok(int_hash ^ mask)
}

// == Hash computation

/// Hashes a packed field with the named algorithm.
pub fn compute_hash(
    algo: &str,
    int_init: Option<&BigInt>,
    (width, int): &(BigInt, BigInt),
) -> Result<BigInt, ExternError> {
    match algo {
        // CRC16-ARC
        "crc16" => crc(width, int, 0xA001, 0),
        // CRC32 as in IEEE 802.3
        "crc32" => crc(width, int, 0xEDB88320, u32::MAX),
        // Internet checksum, optionally subtracting the fields from the seed
        "csum16" | "csum16_sub" => {
            checksum(width, int, int_init.unwrap_or(&BigInt::zero()), algo == "csum16_sub")
        }
        // Identity passes the packed value through
        "identity" => Ok(int.clone()),
        // Other algorithms are not implemented
        _ => Err(error::hash_algorithm_unsupported(format!("(TODO: compute_hash) {algo}")).into()),
    }
}

/// Concatenates fixed-width fields into one integer, padded to 16 bits.
pub fn package(arena: &Arena, values: &[Value]) -> Result<(BigInt, BigInt), ExternError> {
    let mut width_pack = BigInt::zero();
    let mut int_pack = BigInt::zero();
    for value in values {
        // Each field adds its width and its bits, reduced modulo the width
        let (width, int) = unpack::p4_precision_number(arena, value)?;
        let width_bits = width_bit(&width)?;
        let int_modulus = BigInt::one() << width_bits;
        width_pack += width;
        int_pack = (int_pack << width_bits) + remainder(&int, &int_modulus);
    }
    let rem = &width_pack % 16_u8;
    if !rem.is_zero() {
        // Source padding widens the capacity without shifting the packed bits
        width_pack += BigInt::from(16_u8) - rem;
    }
    Ok((width_pack, int_pack))
}

// == Entry points

/// Packs the fields and hashes them.
pub fn compute_checksum(
    arena: &Arena,
    algo: &str,
    int_init: Option<&BigInt>,
    values: &[Value],
) -> Result<BigInt, ExternError> {
    let bits = package(arena, values)?;
    compute_hash(algo, int_init, &bits)
}
