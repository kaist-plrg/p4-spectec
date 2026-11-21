//! Packet clone requests, pending actions and processing contexts
//!
//! For example, an I2E clone records a mirror session and a field-list index

use p4spectec::runner::ExternError;

use crate::p4::{P4Value, SpecValue};

use crate::arch::{core::object::PacketIn, v1model::types::CloneType};

// == Packet clones

/// Packet clone direction, mirror session and preserved field-list index.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CloneInfo(pub CloneType, pub usize, pub usize);

impl CloneInfo {
    /// Reads the clone type, session, and field-list index from the arguments.
    pub fn new(kind: &P4Value, session: P4Value, idx: P4Value) -> Result<Self, ExternError> {
        let clone_type = CloneType::try_from(kind)?;
        let session: usize = session.try_into()?;
        let idx: usize = idx.try_into()?;
        Ok(Self(clone_type, session, idx))
    }
}

// == Actions on a packet

/// Actions requested by extern calls for the current packet.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Action {
    /// Clone requested by the last `clone_preserving_field_list` call.
    pub clone_opt: Option<CloneInfo>,
    /// Field-list index of a requested resubmit.
    pub resubmit_opt: Option<usize>,
    /// Field-list index of a requested recirculate.
    pub recirculate_opt: Option<usize>,
}

// == Processing context per packet

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Block a queued packet resumes at.
pub enum Entrypoint {
    /// Ingress control; parser and verify have already run.
    Ingress,
    /// Egress control, followed by checksum and deparser.
    Egress,
}

/// Processing context per packet.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Packet {
    /// Evaluation context.
    pub value_ctx: SpecValue,
    /// Packet input.
    pub packet_in: PacketIn,
    /// Block to resume after parser and verify have already run.
    pub entrypoint: Entrypoint,
}
