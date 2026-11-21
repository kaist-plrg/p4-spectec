//! Packet field packing, reflected CRCs, and one's-complement checksums
//!
//! The hash externs pack their fields into one wide integer (`pack`)
//! and hash it with an `Algorithm` (`hash_bits`), both through `hash_values`;
//! each architecture's P4 `HashAlgorithm` enum converts into one.
//! CRCs are the reflected table-less form over bytes;
//! checksums fold 16-bit words with end-around carry and complement the result.

use num_bigint::BigInt;
use num_traits::{One, ToPrimitive, Zero};

use p4spectec::util::bigint::{remainder, width_bit};

use p4spectec::runner::ExternError;

use crate::p4::P4Value;

use crate::error;

// == Algorithms

/// The hash and checksum algorithms the simulator computes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Algorithm {
    /// The packed value itself.
    Identity,
    /// CRC16-ARC.
    Crc16,
    /// CRC32 as in IEEE 802.3.
    Crc32,
    /// The Internet checksum, adding the fields to the seed.
    Csum16,
    /// The Internet checksum, subtracting the fields from the seed.
    Csum16Sub,
    /// An algorithm a P4 enum names but the simulator does not compute.
    Unsupported(&'static str),
}

// == Hash algorithms

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
/// `Csum16Sub` folds inverted words.
fn checksum(
    width: &BigInt,
    int: &BigInt,
    int_init: &BigInt,
    algo: Algorithm,
) -> Result<BigInt, ExternError> {
    let width = width_bit_aligned(width, 16)?;
    let int_threshold = BigInt::one() << 16;
    let int_mask = &int_threshold - BigInt::one();
    let mut int_hash = int_init.clone();
    // Add each word with end-around carry
    for idx in (0..width).step_by(16).rev() {
        let int_word = (int >> idx) & &int_mask;
        let int_word = if algo == Algorithm::Csum16Sub { int_word ^ &int_mask } else { int_word };
        let int_sum = int_hash + int_word;
        let carry = u8::from(int_sum >= int_threshold);
        int_hash = remainder(&int_sum, &int_threshold) + carry;
    }
    // Complement only the low 16 bits, retaining any higher seed bits
    Ok(int_hash ^ int_mask)
}

// == Hashing

/// Hashes a packed field with the named algorithm.
fn hash_bits(
    algo: Algorithm,
    int_init: Option<&BigInt>,
    (width, int): &(BigInt, BigInt),
) -> Result<BigInt, ExternError> {
    let int_seed = int_init.unwrap_or(&BigInt::zero()).clone();
    match algo {
        Algorithm::Identity => Ok(int.clone()),
        Algorithm::Crc16 => crc(width, int, 0xA001, 0),
        Algorithm::Crc32 => crc(width, int, 0xEDB88320, u32::MAX),
        Algorithm::Csum16 | Algorithm::Csum16Sub => checksum(width, int, &int_seed, algo),
        Algorithm::Unsupported(name) => {
            Err(error::hash_algorithm_unsupported(format!("(TODO: compute_hash) {name}")).into())
        }
    }
}

/// Concatenates fixed-width fields into one integer, padded to 16 bits.
fn pack(values: &[P4Value]) -> Result<(BigInt, BigInt), ExternError> {
    let mut width_pack = BigInt::zero();
    let mut int_pack = BigInt::zero();
    for value in values {
        // Each field adds its width and its bits, reduced modulo the width;
        // a varbit adds its actual width, not its maximum
        let (width, int) = match value {
            P4Value::Bit { width, int }
            | P4Value::Int { width, int }
            | P4Value::Varbit { width, int, .. } => (*width, int),
            _ => return Err(value.mismatch("a bit<W>, int<W>, or varbit<W>")),
        };
        let int_modulus = BigInt::one() << width;
        width_pack += width;
        int_pack = (int_pack << width) + remainder(int, &int_modulus);
    }
    let int_rem = &width_pack % 16_u8;
    if !int_rem.is_zero() {
        // Source padding widens the capacity without shifting the packed bits
        width_pack += BigInt::from(16_u8) - int_rem;
    }
    Ok((width_pack, int_pack))
}

/// Packs the fields and hashes them.
pub fn hash_values(
    algo: Algorithm,
    int_init: Option<&BigInt>,
    values: &[P4Value],
) -> Result<BigInt, ExternError> {
    let bits = pack(values)?;
    hash_bits(algo, int_init, &bits)
}
