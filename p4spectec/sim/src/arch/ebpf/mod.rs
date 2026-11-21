//! eBPF architecture: the externs of `ebpf_model.p4` and the switch behind them
//!
//! `externs` is what `ebpf_model.p4` declares, its one stateful object,
//! a counter array (`object`), and how a specification call reaches it.
//! `target` is the switch behind it: the control plane (`runtime`)
//! and the pipeline (`pipe`), which parses, filters, and forwards accepted packets.

use crate::p4::HostTypDef;

use super::core::object::PacketIn;

use self::externs::object::CounterArray;

mod externs;
pub(super) mod target;

// == Configuration

/// The eBPF architecture, with its definitions of the extern types.
pub struct Ebpf {
    /// The definition of `archState`, which eBPF leaves empty.
    typdef_state: HostTypDef<()>,
    /// The definition of `objectState`; a stateless object carries `None`.
    typdef_object: HostTypDef<Option<ObjectState>>,
}

// Every instance makes definitions of its own
impl Default for Ebpf {
    fn default() -> Self {
        Self {
            typdef_state: HostTypDef::new("archState"),
            typdef_object: HostTypDef::new("objectState"),
        }
    }
}

// == Extern objects

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
/// Core and eBPF-specific extern objects.
pub enum ObjectState {
    /// The `packet_in` being parsed.
    PacketIn(PacketIn),
    /// A `CounterArray`.
    CounterArray(CounterArray),
}
