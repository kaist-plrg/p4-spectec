//! Control-plane operations, named after bmv2's runtime interface
//!
//! An STF test configures the switch between packets:
//! table entries, mirroring sessions, multicast groups and nodes,
//! and registers.
//! Tables work the same in every architecture,
//! so their operations are `Sim` methods instead
//! (`add_entry`, `set_default_action`, `get_keys`, in `spec::table`).
//! Each architecture implements the operations it supports;
//! the rest fail with "not implemented".
//! The names follow `bm::RuntimeInterface` in bmv2.

use num_bigint::BigInt;

use p4spectec::runner::ExternError;

use crate::error;

use crate::p4::P4Value;

use super::{arch::Architecture, sim::Sim};

// == Configuration

/// Where a mirroring session sends its clones, as in bmv2's `MirroringSessionConfig`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MirroringSessionConfig {
    /// Each clone leaves through this port, as `mirroring_add` sets.
    EgressPort(usize),
    /// Each clone is replicated to this multicast group, as `mirroring_add_mc` sets.
    Mgid(usize),
}

// == Table entries

/// A table entry, as bmv2's `mt_add_entry` takes it and `Sim::add_entry` adds it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TableEntry {
    /// The table's control-plane name, qualified or bare.
    pub table: String,
    /// The entry's priority, for tables that use one.
    pub priority: Option<i64>,
    /// The key values, by key name.
    pub keys: Vec<MatchKey>,
    /// The action to run on a match.
    pub action: ActionCall,
}

/// One key of a table entry.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchKey {
    /// The key's name, such as `hdr.ipv4.dstAddr`.
    pub name: String,
    /// The value to match.
    pub param: MatchKeyParam,
}

/// A key value as the control plane writes it, interpreted by the key's match kind.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MatchKeyParam {
    /// A number in `radix`, whose digits may include `*` wildcards.
    Number { radix: Radix, digits: String },
    /// A value and a prefix length, `value/len`.
    Prefix { value: String, len: usize },
}

/// The radix a key number is written in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Radix {
    /// `0b...`.
    Bin,
    /// Plain digits.
    Dec,
    /// `0x...`.
    Hex,
}

/// An action and its arguments by parameter name.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ActionCall {
    /// The action's control-plane name.
    pub name: String,
    /// The arguments, by parameter name.
    pub args: Vec<(String, BigInt)>,
}

/// Whether a table took an entry.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EntryStatus {
    /// The entry was added.
    Added,
    /// The table rejected the entry, for example because a key name did not match.
    Rejected,
}

/// One key of a table: its name and match kind, such as `exact` or `lpm`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TableKey {
    /// The key's name.
    pub name: String,
    /// The key's match kind.
    pub match_kind: String,
}

// == Operations

/// The control-plane operations an STF test can apply to an architecture.
pub trait RuntimeInterface: Architecture {
    /// `mirroring_add` and `mirroring_add_mc`: configures mirroring session `id`.
    fn mirroring_add_session(
        _sim: &mut Sim<Self>,
        _id: usize,
        _config: MirroringSessionConfig,
    ) -> Result<(), ExternError> {
        runtime_unsupported::<Self, _>("mirroring_add_session")
    }

    /// `mc_mgrp_create`: creates multicast group `mgrp`.
    fn mc_mgrp_create(_sim: &mut Sim<Self>, _mgrp: usize) -> Result<(), ExternError> {
        runtime_unsupported::<Self, _>("mc_mgrp_create")
    }

    /// `mc_node_create`: creates a replication node with id `rid` over `ports`.
    fn mc_node_create(
        _sim: &mut Sim<Self>,
        _rid: usize,
        _ports: &[usize],
    ) -> Result<(), ExternError> {
        runtime_unsupported::<Self, _>("mc_node_create")
    }

    /// `mc_node_associate`: adds node `node` to multicast group `mgrp`.
    fn mc_node_associate(
        _sim: &mut Sim<Self>,
        _mgrp: usize,
        _node: usize,
    ) -> Result<(), ExternError> {
        runtime_unsupported::<Self, _>("mc_node_associate")
    }

    /// `register_read`: the element `idx` of register `name`.
    fn register_read(
        _sim: &mut Sim<Self>,
        _name: &str,
        _idx: usize,
    ) -> Result<P4Value, ExternError> {
        runtime_unsupported::<Self, _>("register_read")
    }

    /// `register_write`: writes `value` to the element `idx` of register `name`.
    fn register_write(
        _sim: &mut Sim<Self>,
        _name: &str,
        _idx: usize,
        _value: P4Value,
    ) -> Result<(), ExternError> {
        runtime_unsupported::<Self, _>("register_write")
    }

    /// `register_reset`: resets every element of register `name`.
    fn register_reset(_sim: &mut Sim<Self>, _name: &str) -> Result<(), ExternError> {
        runtime_unsupported::<Self, _>("register_reset")
    }
}

/// Fails because `Arch` does not implement the operation `op`.
pub fn runtime_unsupported<Arch: RuntimeInterface, T>(op: &str) -> Result<T, ExternError> {
    Err(error::control_operation_unsupported(format!(
        "{op} is not implemented for the {} simulator",
        Arch::NAME
    ))
    .into())
}
