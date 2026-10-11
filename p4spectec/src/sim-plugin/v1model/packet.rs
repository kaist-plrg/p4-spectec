//! Packet clone requests, pending actions and processing contexts
//!
//! For example, an I2E clone records a mirror session and a field-list index

use serde::{Deserialize, Serialize};
use serde_derive_state::{DeserializeState, SerializeState};

use crate::lang::data::{
    arena::Arena,
    value::flat::{
        Value,
        external::{DecodeContext, EncodeContext},
    },
};

use crate::runner::ExternError;

use crate::sim_plugin::error;

use super::super::{core::object::PacketIn, spec::unpack};

// == Packet clones

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// Where a clone is taken: after ingress (I2E) or after egress (E2E).
pub enum CloneType {
    /// Ingress-to-egress clone: parsed again before egress.
    I2E,
    /// Egress-to-egress clone: enters egress directly.
    E2E,
}

/// Packet clone direction, mirror session and preserved field-list index.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CloneInfo(pub CloneType, pub usize, pub usize);

impl CloneInfo {
    /// Reads the clone type, session, and field-list index from the arguments.
    pub fn new(
        arena: &Arena,
        value_clone_type: &Value,
        value_session: &Value,
        value_idx: &Value,
    ) -> Result<Self, ExternError> {
        let (_, name) = unpack::p4_enum(arena, value_clone_type)?;
        let clone_type = match name.as_str() {
            "I2E" => CloneType::I2E,
            "E2E" => CloneType::E2E,
            // Any other tag is not a `CloneType`
            name => {
                return Err(error::clone_direction_invalid(format!(
                    "Invalid enum value \"{name}\". Expected I2E or E2E"
                ))
                .into());
            }
        };
        let session = usize::try_from(&unpack::p4_fixed_bit(arena, value_session)?.1)?;
        let idx = usize::try_from(&unpack::p4_fixed_bit(arena, value_idx)?.1)?;
        Ok(Self(clone_type, session, idx))
    }
}

// == Actions on a packet

/// Actions requested by extern calls for the current packet.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Action {
    /// Clone requested by the last `clone_preserving_field_list` call.
    pub clone_opt: Option<CloneInfo>,
    /// Field-list index of a requested resubmit.
    pub resubmit_opt: Option<usize>,
    /// Field-list index of a requested recirculate.
    pub recirculate_opt: Option<usize>,
}

// == Processing context per packet

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
/// Block a queued packet resumes at.
pub enum Entrypoint {
    /// Ingress control; parser and verify have already run.
    Ingress,
    /// Egress control, followed by checksum and deparser.
    Egress,
}

/// Processing context per packet.
#[derive(Clone, Debug, PartialEq, Eq, SerializeState, DeserializeState)]
#[serde(deny_unknown_fields, serialize_state = "EncodeContext<'arena>", ser_parameters = "'arena")]
#[serde(deserialize_state = "DecodeContext<'de>")]
pub struct Packet {
    /// Evaluation context.
    #[serde(state)]
    pub value_ctx: Value,
    /// Packet input.
    pub packet_in: PacketIn,
    /// Block to resume after parser and verify have already run.
    pub entrypoint: Entrypoint,
}
