//! Architectural state of the v1model pipeline
//!
//! The packet queue, mirror sessions, multicast groups,
//! and the current packet's requested actions,
//! stored in the specification's architecture state as an extern value.

use std::collections::VecDeque;

use serde_derive_state::{DeserializeState, SerializeState};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::{
            extern_value::TypDef,
            flat::{
                Value,
                external::{DecodeContext, EncodeContext},
            },
        },
    },
};

use crate::runner::ExternError;

use super::{
    mirror, multicast,
    packet::{Action, Packet},
};

#[derive(
    Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, SerializeState, DeserializeState,
)]
#[serde(deny_unknown_fields, serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'de>")]
/// Architectural state with an empty-state default constructor.
pub struct Arch {
    #[serde(state)]
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

    /// Wraps the state as the specification's `archState` value.
    pub fn to_value(self, arena: &mut Arena, typdef: TypDef<Self>) -> Result<Value, ExternError> {
        Ok(typdef.make(arena, self, Span::default())?)
    }

    /// Reads the state from an `archState` value.
    pub fn from_value(
        arena: &Arena,
        typdef: TypDef<Self>,
        value: &Value,
    ) -> Result<Self, ExternError> {
        Ok(typdef.get(arena, value)?.clone())
    }
}
