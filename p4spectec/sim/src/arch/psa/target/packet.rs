//! Queued packet of the PSA scheduler
//!
//! A saved context, input packet, and the pipeline it resumes at.

use crate::p4::SpecValue;

use crate::arch::core::object::PacketIn;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Which pipeline a queued packet resumes at.
pub enum Entrypoint {
    /// Ingress parser, control, and deparser.
    Ingress,
    /// Egress parser, control, and deparser.
    Egress,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Processing context per packet.
pub struct Packet {
    /// Evaluation context.
    pub value_ctx: SpecValue,
    /// Packet input.
    pub packet_in: PacketIn,
    /// Which pipeline the packet should begin processing.
    pub entrypoint: Entrypoint,
}
