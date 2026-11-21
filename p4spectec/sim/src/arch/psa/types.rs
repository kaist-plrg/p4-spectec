//! P4 enums declared in psa.p4
//!
//! Each Rust enum lists its P4 enum's members in declaration order.
//! `TryFrom<&P4Value>` reads an argument of the enum's type,
//! failing on a member of another enum or on a value of another form:
//! `let kind = CounterType::try_from(&env.get("type")?)?;`
//! `MeterColor` is only returned, so it converts into a `P4Value` instead.

use p4spectec::runner::ExternError;

use crate::{arch::hash, error};

use crate::p4::P4Value;

// == Hashing

/// The algorithm of a `Hash`.
///
/// ```text
/// enum PSA_HashAlgorithm_t {
///   IDENTITY, CRC32, CRC32_CUSTOM, CRC16, CRC16_CUSTOM, ONES_COMPLEMENT16, TARGET_DEFAULT
/// }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HashAlgorithm {
    Identity,
    Crc32,
    Crc32Custom,
    Crc16,
    Crc16Custom,
    OnesComplement16,
    TargetDefault,
}

impl TryFrom<&P4Value> for HashAlgorithm {
    type Error = ExternError;

    fn try_from(algo: &P4Value) -> Result<Self, Self::Error> {
        let P4Value::Enum { id_typ, id_member } = algo else {
            return Err(algo.mismatch("a PSA_HashAlgorithm_t"));
        };
        match (id_typ.as_str(), id_member.as_str()) {
            ("PSA_HashAlgorithm_t", "IDENTITY") => Ok(Self::Identity),
            ("PSA_HashAlgorithm_t", "CRC32") => Ok(Self::Crc32),
            ("PSA_HashAlgorithm_t", "CRC32_CUSTOM") => Ok(Self::Crc32Custom),
            ("PSA_HashAlgorithm_t", "CRC16") => Ok(Self::Crc16),
            ("PSA_HashAlgorithm_t", "CRC16_CUSTOM") => Ok(Self::Crc16Custom),
            ("PSA_HashAlgorithm_t", "ONES_COMPLEMENT16") => Ok(Self::OnesComplement16),
            ("PSA_HashAlgorithm_t", "TARGET_DEFAULT") => Ok(Self::TargetDefault),
            // A member of another enum
            _ => Err(error::hash_algorithm_invalid(format!(
                "invalid PSA_HashAlgorithm_t enum value: {id_typ}.{id_member}"
            ))
            .into()),
        }
    }
}

// == Counters

/// What a `Counter` counts.
///
/// `enum PSA_CounterType_t { PACKETS, BYTES, PACKETS_AND_BYTES }`
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CounterType {
    Packets,
    Bytes,
    PacketsAndBytes,
}

impl From<HashAlgorithm> for hash::Algorithm {
    fn from(algo: HashAlgorithm) -> Self {
        match algo {
            HashAlgorithm::Identity => Self::Identity,
            HashAlgorithm::Crc32 => Self::Crc32,
            HashAlgorithm::Crc16 => Self::Crc16,
            HashAlgorithm::OnesComplement16 => Self::Csum16,
            // Not computed; the name is the P4 member's
            HashAlgorithm::Crc32Custom => Self::Unsupported("CRC32_CUSTOM"),
            HashAlgorithm::Crc16Custom => Self::Unsupported("CRC16_CUSTOM"),
            HashAlgorithm::TargetDefault => Self::Unsupported("TARGET_DEFAULT"),
        }
    }
}

impl TryFrom<&P4Value> for CounterType {
    type Error = ExternError;

    fn try_from(kind: &P4Value) -> Result<Self, Self::Error> {
        let P4Value::Enum { id_typ, id_member } = kind else {
            return Err(kind.mismatch("a PSA_CounterType_t"));
        };
        match (id_typ.as_str(), id_member.as_str()) {
            ("PSA_CounterType_t", "PACKETS") => Ok(Self::Packets),
            ("PSA_CounterType_t", "BYTES") => Ok(Self::Bytes),
            ("PSA_CounterType_t", "PACKETS_AND_BYTES") => Ok(Self::PacketsAndBytes),
            // A member of another enum
            _ => Err(error::counter_type_invalid(format!(
                "invalid PSA_CounterType_t enum value: {id_typ}.{id_member}"
            ))
            .into()),
        }
    }
}

// == Meters

/// What a `Meter` meters.
///
/// `enum PSA_MeterType_t { PACKETS, BYTES }`
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MeterType {
    Packets,
    Bytes,
}

impl TryFrom<&P4Value> for MeterType {
    type Error = ExternError;

    fn try_from(kind: &P4Value) -> Result<Self, Self::Error> {
        let P4Value::Enum { id_typ, id_member } = kind else {
            return Err(kind.mismatch("a PSA_MeterType_t"));
        };
        match (id_typ.as_str(), id_member.as_str()) {
            ("PSA_MeterType_t", "PACKETS") => Ok(Self::Packets),
            ("PSA_MeterType_t", "BYTES") => Ok(Self::Bytes),
            // A member of another enum
            _ => Err(error::meter_type_invalid(format!(
                "invalid PSA_MeterType_t enum value: {id_typ}.{id_member}"
            ))
            .into()),
        }
    }
}

/// A meter color (RFC 2698), as a `Meter` returns it.
///
/// `enum PSA_MeterColor_t { RED, GREEN, YELLOW }`
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MeterColor {
    /// Over the peak rate.
    Red,
    /// Within the committed rate.
    Green,
    /// Between the committed and peak rates.
    Yellow,
}

impl From<MeterColor> for P4Value {
    fn from(color: MeterColor) -> Self {
        let id_member = match color {
            MeterColor::Red => "RED",
            MeterColor::Green => "GREEN",
            MeterColor::Yellow => "YELLOW",
        };
        P4Value::Enum { id_typ: "PSA_MeterColor_t".into(), id_member: id_member.into() }
    }
}
