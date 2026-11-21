//! Architectural state of the v1model pipeline
//!
//! The packet queue, mirror sessions, multicast groups,
//! and the current packet's requested actions,
//! stored in the specification's architecture state as a host payload.

use std::collections::VecDeque;

use super::{
    mirror, multicast,
    packet::{Action, Packet},
};

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Architectural state with an empty-state default constructor.
pub struct ArchState {
    /// Packets waiting for ingress or egress processing.
    pub queue: VecDeque<Packet>,
    /// Mirror session id to output port.
    pub mirrortable: mirror::Table,
    /// Multicast groups and their nodes.
    pub multicast: multicast::State,
    /// Clone, resubmit, and recirculate requests of the current packet.
    pub action: Action,
}

impl ArchState {
    /// Reset only the current packet's requested actions.
    pub fn reset(&mut self) {
        self.action = Action::default();
    }
}
