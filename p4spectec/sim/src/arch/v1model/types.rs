//! P4 enums declared in v1model.p4
//!
//! Each Rust enum lists its P4 enum's members in declaration order.
//! `TryFrom<&P4Value>` reads an argument of the enum's type,
//! failing on a member of another enum or on a value of another form:
//! `let kind = CounterType::try_from(&env.get("type")?)?;`

use p4spectec::runner::ExternError;

use crate::{arch::hash, error};

use crate::p4::P4Value;

// == Counters and meters

/// What a `counter` or `direct_counter` counts.
///
/// `enum CounterType { packets, bytes, packets_and_bytes }`
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CounterType {
    Packets,
    Bytes,
    PacketsAndBytes,
}

impl TryFrom<&P4Value> for CounterType {
    type Error = ExternError;

    fn try_from(kind: &P4Value) -> Result<Self, Self::Error> {
        let P4Value::Enum { id_typ, id_member } = kind else {
            return Err(kind.mismatch("a CounterType"));
        };
        match (id_typ.as_str(), id_member.as_str()) {
            ("CounterType", "packets") => Ok(Self::Packets),
            ("CounterType", "bytes") => Ok(Self::Bytes),
            ("CounterType", "packets_and_bytes") => Ok(Self::PacketsAndBytes),
            // A member of another enum
            _ => Err(error::counter_type_invalid(format!(
                "invalid CounterType enum value: {id_typ}.{id_member}"
            ))
            .into()),
        }
    }
}

/// What a `meter` or `direct_meter` meters.
///
/// `enum MeterType { packets, bytes }`
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MeterType {
    Packets,
    Bytes,
}

impl TryFrom<&P4Value> for MeterType {
    type Error = ExternError;

    fn try_from(kind: &P4Value) -> Result<Self, Self::Error> {
        let P4Value::Enum { id_typ, id_member } = kind else {
            return Err(kind.mismatch("a MeterType"));
        };
        match (id_typ.as_str(), id_member.as_str()) {
            ("MeterType", "packets") => Ok(Self::Packets),
            ("MeterType", "bytes") => Ok(Self::Bytes),
            // A member of another enum
            _ => Err(error::meter_type_invalid(format!(
                "invalid MeterType enum value: {id_typ}.{id_member}"
            ))
            .into()),
        }
    }
}

// == Hashing

/// The algorithm of `hash`, `verify_checksum`, and `update_checksum`.
///
/// ```text
/// enum HashAlgorithm {
///     crc32, crc32_custom, crc16, crc16_custom, random, identity, csum16, xor16
/// }
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HashAlgorithm {
    Crc32,
    Crc32Custom,
    Crc16,
    Crc16Custom,
    Random,
    Identity,
    Csum16,
    Xor16,
}

impl TryFrom<&P4Value> for HashAlgorithm {
    type Error = ExternError;

    fn try_from(algo: &P4Value) -> Result<Self, Self::Error> {
        let P4Value::Enum { id_typ, id_member } = algo else {
            return Err(algo.mismatch("a HashAlgorithm"));
        };
        match (id_typ.as_str(), id_member.as_str()) {
            ("HashAlgorithm", "crc32") => Ok(Self::Crc32),
            ("HashAlgorithm", "crc32_custom") => Ok(Self::Crc32Custom),
            ("HashAlgorithm", "crc16") => Ok(Self::Crc16),
            ("HashAlgorithm", "crc16_custom") => Ok(Self::Crc16Custom),
            ("HashAlgorithm", "random") => Ok(Self::Random),
            ("HashAlgorithm", "identity") => Ok(Self::Identity),
            ("HashAlgorithm", "csum16") => Ok(Self::Csum16),
            ("HashAlgorithm", "xor16") => Ok(Self::Xor16),
            // A member of another enum
            _ => Err(error::hash_algorithm_invalid(format!(
                "invalid HashAlgorithm enum value: {id_typ}.{id_member}"
            ))
            .into()),
        }
    }
}

impl From<HashAlgorithm> for hash::Algorithm {
    fn from(algo: HashAlgorithm) -> Self {
        match algo {
            HashAlgorithm::Crc32 => Self::Crc32,
            HashAlgorithm::Crc16 => Self::Crc16,
            HashAlgorithm::Identity => Self::Identity,
            HashAlgorithm::Csum16 => Self::Csum16,
            // Not computed; the name is the P4 member's
            HashAlgorithm::Crc32Custom => Self::Unsupported("crc32_custom"),
            HashAlgorithm::Crc16Custom => Self::Unsupported("crc16_custom"),
            HashAlgorithm::Random => Self::Unsupported("random"),
            HashAlgorithm::Xor16 => Self::Unsupported("xor16"),
        }
    }
}

// == Clones

/// Where a clone is taken: after ingress (I2E) or after egress (E2E).
///
/// `enum CloneType { I2E, E2E }`
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CloneType {
    /// Ingress-to-egress clone: parsed again before egress.
    I2E,
    /// Egress-to-egress clone: enters egress directly.
    E2E,
}

impl TryFrom<&P4Value> for CloneType {
    type Error = ExternError;

    fn try_from(kind: &P4Value) -> Result<Self, Self::Error> {
        let P4Value::Enum { id_typ, id_member } = kind else {
            return Err(kind.mismatch("a CloneType"));
        };
        match (id_typ.as_str(), id_member.as_str()) {
            ("CloneType", "I2E") => Ok(Self::I2E),
            ("CloneType", "E2E") => Ok(Self::E2E),
            // A member of another enum
            _ => Err(error::clone_direction_invalid(format!(
                "Invalid enum value \"{id_typ}.{id_member}\". Expected I2E or E2E"
            ))
            .into()),
        }
    }
}
