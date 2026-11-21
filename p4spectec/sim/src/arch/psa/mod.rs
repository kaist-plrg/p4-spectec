//! PSA architecture: the externs of `psa.p4` and the switch behind them
//!
//! `externs` is what `psa.p4` declares, its stateful objects (`object`),
//! and how a specification call reaches them.
//! `target` is the switch behind it: the architecture state (`state`),
//! the packet queue, mirror and multicast tables, the control plane (`runtime`),
//! and the scheduler (`pipe`): ingress and egress each parse, control, and deparse,
//! and the PRE and BQE schedule clones, multicast, resubmit, and recirculate.
//! The enums of `psa.p4` are in `types`.

use crate::p4::HostTypDef;

use super::core::object::{PacketIn, PacketOut};

use self::{
    externs::object::{Counter, HashExtern, InternetChecksum, Meter, Register},
    target::state::ArchState,
};

mod externs;
pub(super) mod target;
pub(crate) mod types;

// == Configuration

/// The PSA architecture, with its definitions of the extern types.
pub struct Psa {
    /// The definition of `archState` as the architectural state.
    typdef_state: HostTypDef<ArchState>,
    /// The definition of `objectState`; a stateless object carries `None`.
    typdef_object: HostTypDef<Option<ObjectState>>,
}

// Every instance makes definitions of its own
impl Default for Psa {
    fn default() -> Self {
        Self {
            typdef_state: HostTypDef::new("archState"),
            typdef_object: HostTypDef::new("objectState"),
        }
    }
}

// == Extern objects

/// Core and PSA-specific extern objects.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ObjectState {
    /// A `packet_in` of the current pipeline.
    PacketIn(PacketIn),
    /// A `packet_out` being emitted.
    PacketOut(PacketOut),
    /// A `Counter` array.
    Counter(Counter),
    /// A `Register` array.
    Register(Register),
    /// A `Hash`.
    Hash(HashExtern),
    /// An `InternetChecksum`.
    InternetChecksum(InternetChecksum),
    /// A `Meter`.
    Meter(Meter),
}
