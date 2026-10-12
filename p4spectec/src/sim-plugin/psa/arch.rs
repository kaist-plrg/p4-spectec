//! Architectural state of the PSA pipeline
//!
//! The packet queue, mirror sessions, and multicast groups,
//! stored in the specification's architecture state as an encoded value.

use std::collections::VecDeque;

use serde_derive_state::{DeserializeState, SerializeState};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        encoding::Encoding,
        typ,
        value::flat::{
            self as value, Value,
            external::{DecodeContext, EncodeContext, decode_with, encode_with},
        },
    },
};

use crate::runner::ExternError;

use super::{mirror, multicast, packet::Packet};

#[derive(Clone, Debug, Default, PartialEq, Eq, SerializeState, DeserializeState)]
#[serde(deny_unknown_fields, serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'de>")]
/// Architectural state with an empty-state default constructor.
pub struct Arch {
    #[serde(state)]
    /// Packets waiting for ingress or egress processing.
    pub queue: VecDeque<Packet>,
    /// Clone session id to multicast group id.
    pub mirrortable: mirror::Table,
    /// Multicast groups and their nodes.
    pub multicast: multicast::State,
}

impl Arch {
    /// Encodes the state as the specification's `archState` external value.
    pub fn to_value(&self, arena: &mut Arena, encoding: Encoding) -> Result<Value, ExternError> {
        let payload = encode_with(arena, encoding, self).map_err(ExternError::from)?;
        let typ = typ::make::var(
            crate::phrase!(node: "archState".to_owned(), span: Span::default()),
            Vec::new(),
        );
        Ok(value::make::external(arena, typ.node.into(), payload.into(), Span::default())?)
    }

    /// Decodes the state from an `archState` external value.
    pub fn from_value(
        arena: &mut Arena,
        encoding: Encoding,
        value: &Value,
    ) -> Result<Self, ExternError> {
        let json = value::get::external(arena, value)?.clone();
        decode_with(arena, encoding, json.as_ref()).map_err(ExternError::from)
    }
}
