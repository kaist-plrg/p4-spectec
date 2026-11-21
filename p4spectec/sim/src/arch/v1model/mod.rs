//! v1model architecture: the externs of `v1model.p4` and the switch behind them
//!
//! This module defines the architecture's type and the extern objects it keeps
//! (`ObjectState`), the drop port and the port width,
//! and `Sim::get_packet_in`, which both the externs and the scheduler read.
//! `externs` holds what `v1model.p4` declares, its extern functions and objects,
//! and `impl Architecture`, which is how a specification call reaches them.
//! `target` is the switch behind them: the state, the packet queue,
//! mirror and multicast tables, the control plane, and the scheduler (`pipe`).
//! `types` has the enums of `v1model.p4`.

use p4spectec::runner::ExternError;

use crate::{
    error,
    p4::{HostTypDef, Sim},
};

use super::core::object::{PacketIn, PacketOut};

use self::{
    externs::object::{Counter, DirectCounter, DirectMeter, Register},
    target::state::ArchState,
};

mod externs;
pub(super) mod target;
pub(crate) mod types;

// == Configuration

/// The v1model architecture, with its definitions of the extern types.
pub struct V1Model {
    /// The definition of `archState` as the architectural state.
    typdef_state: HostTypDef<ArchState>,
    /// The definition of `objectState`; a stateless object carries `None`.
    typdef_object: HostTypDef<Option<ObjectState>>,
}

// Every instance makes definitions of its own
impl Default for V1Model {
    fn default() -> Self {
        Self {
            typdef_state: HostTypDef::new("archState"),
            typdef_object: HostTypDef::new("objectState"),
        }
    }
}

// == Extern objects

/// Core and v1model-specific extern objects.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ObjectState {
    /// The `packet_in` of the current packet.
    PacketIn(PacketIn),
    /// The `packet_out` being emitted.
    PacketOut(PacketOut),
    /// A `counter` array.
    Counter(Counter),
    /// A `register` array.
    Register(Register),
    /// A `direct_counter`.
    DirectCounter(DirectCounter),
    /// A `direct_meter`.
    DirectMeter(DirectMeter),
}

// == Constants

/// The port that drops a packet, bmv2's `SimpleSwitch::default_drop_port`.
const DEFAULT_DROP_PORT: usize = 511;

/// The width of a port in `standard_metadata`, `PortId_t` in `v1model.p4`.
const PORT_WIDTH: usize = 9;

// == Objects of the current packet

impl Sim<'_, V1Model> {
    /// The `packet_in` object of the current packet.
    fn get_packet_in(&mut self) -> Result<PacketIn, ExternError> {
        match self.get_object("packet_in")? {
            ObjectState::PacketIn(packet_in) => Ok(packet_in),
            _ => {
                Err(error::extern_object_undefined("packet_in extern not found".to_owned()).into())
            }
        }
    }
}
