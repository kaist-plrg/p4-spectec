//! Architectural state of the v1model pipeline
//!
//! The packet queue, mirror sessions, multicast groups,
//! and the current packet's requested actions,
//! stored in the specification's architecture state as a host payload.

use std::collections::VecDeque;

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::{
            flat::{self as value, Value},
            host::HostTypDef,
        },
    },
};

use crate::runner::ExternError;

use super::{
    mirror, multicast,
    packet::{Action, Packet},
};

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Architectural state with an empty-state default constructor.
pub struct Arch {
    /// Packets waiting for ingress or egress processing.
    pub queue: VecDeque<Packet>,
    /// Mirror session id to output port.
    pub mirrortable: mirror::Table,
    /// Multicast groups and their nodes.
    pub multicast: multicast::State,
    /// Clone, resubmit, and recirculate requests of the current packet.
    pub action: Action,
}

impl Arch {
    /// Reset only the current packet's requested actions.
    pub fn reset(&mut self) {
        self.action = Action::default();
    }

    /// Wraps the state as an `archState` value of `typdef`.
    pub fn to_value(
        &self,
        arena: &mut Arena,
        typdef: HostTypDef<Self>,
    ) -> Result<Value, ExternError> {
        Ok(value::make::external(arena, typdef.payload(self.clone()), Span::default())?)
    }

    /// Reads the state from an `archState` value of `typdef`.
    pub fn from_value(
        arena: &Arena,
        value: &Value,
        typdef: HostTypDef<Self>,
    ) -> Result<Self, ExternError> {
        Ok(value::get::external(arena, typdef, value)?.clone())
    }
}
