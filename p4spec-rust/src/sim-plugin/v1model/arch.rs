//! Architectural state of the v1model pipeline
//!
//! The packet queue, mirror sessions, multicast groups,
//! and the current packet's requested actions,
//! stored in the specification's architecture state as an encoded value.

use super::{
    mirror, multicast,
    packet::{Action, Packet},
};
use crate::lang::data::value::external::{
    DecodeContext, EncodeContext, Encoding, decode_with, encode_with,
};
use crate::{
    lang::{
        common::source::Span,
        data::{
            typ,
            value::{Value, ValueArena, get, make},
        },
    },
    runner::ExternError,
};
use serde_derive_state::{DeserializeState, SerializeState};
use std::collections::VecDeque;

#[derive(Clone, Debug, Default, PartialEq, Eq, SerializeState, DeserializeState)]
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

    /// Encodes the state as the specification's `archState` external value.
    pub fn to_value(
        &self,
        arena: &mut ValueArena,
        encoding: Encoding,
    ) -> Result<Value, ExternError> {
        let payload = encode_with(arena, encoding, self).map_err(ExternError::from)?;
        let typ = typ::make::var(
            crate::phrase!(node: "archState".to_owned(), span: Span::default()),
            Vec::new(),
        );
        Ok(make::external(arena, typ.node.into(), payload.into(), Span::default())?)
    }

    /// Decodes the state from an `archState` external value.
    pub fn from_value(
        arena: &mut ValueArena,
        encoding: Encoding,
        value: &Value,
    ) -> Result<Self, ExternError> {
        let json = get::external(arena, value)?.clone();
        decode_with(arena, encoding, json.as_ref()).map_err(ExternError::from)
    }
}
