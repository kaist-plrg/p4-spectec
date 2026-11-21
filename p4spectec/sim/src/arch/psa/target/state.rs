//! Architectural state of the PSA pipeline
//!
//! The packet queue, mirror sessions, and multicast groups,
//! stored in the specification's architecture state as a host payload.

use std::collections::VecDeque;

use super::{mirror, multicast, packet::Packet};

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Architectural state with an empty-state default constructor.
pub struct ArchState {
    /// Packets waiting for ingress or egress processing.
    pub queue: VecDeque<Packet>,
    /// Clone session id to multicast group id.
    pub mirrortable: mirror::Table,
    /// Multicast groups and their nodes.
    pub multicast: multicast::State,
}
