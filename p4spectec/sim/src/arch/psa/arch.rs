//! Architectural state of the PSA pipeline
//!
//! The packet queue, mirror sessions, and multicast groups,
//! stored in the specification's architecture state as a host payload.

use std::collections::VecDeque;

use p4spectec::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::{
            flat::{self as value, Value},
            host::HostTypDef,
        },
    },
};

use p4spectec::runner::ExternError;

use super::{mirror, multicast, packet::Packet};

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Architectural state with an empty-state default constructor.
pub struct Arch {
    /// Packets waiting for ingress or egress processing.
    pub queue: VecDeque<Packet>,
    /// Clone session id to multicast group id.
    pub mirrortable: mirror::Table,
    /// Multicast groups and their nodes.
    pub multicast: multicast::State,
}

impl Arch {
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
