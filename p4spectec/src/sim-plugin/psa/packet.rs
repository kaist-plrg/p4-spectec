//! Queued packet of the PSA scheduler
//!
//! A saved context, input packet, and the pipeline it resumes at.

use serde::{Deserialize, Serialize};
use serde_derive_state::{DeserializeState, SerializeState};

use crate::lang::data::value::flat::{
    Value,
    external::{DecodeContext, EncodeContext},
};

use super::super::core::object::PacketIn;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// Which pipeline a queued packet resumes at.
pub enum Entrypoint {
    /// Ingress parser, control, and deparser.
    Ingress,
    /// Egress parser, control, and deparser.
    Egress,
}

#[derive(Clone, Debug, PartialEq, Eq, SerializeState, DeserializeState)]
#[serde(deny_unknown_fields, serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'de>")]
/// Processing context per packet.
pub struct Packet {
    /// Evaluation context.
    #[serde(state)]
    pub value_ctx: Value,
    /// Packet input.
    pub packet_in: PacketIn,
    /// Which pipeline the packet should begin processing.
    pub entrypoint: Entrypoint,
}
