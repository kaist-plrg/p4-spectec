//! The PSA control plane
//!
//! A clone session replicates clones to a multicast group,
//! and multicast groups replicate a packet to their nodes;
//! both live in the architecture state.
//! Registers are read, written, and reset as `Register` objects.

use p4spectec::runner::ExternError;

use crate::p4::{self, MirroringSessionConfig, P4Ops, P4Value, RuntimeInterface, Sim};

use crate::error;

use super::super::{ObjectState, Psa, externs::object::Register};

// == Control plane

impl Sim<'_, Psa> {
    /// The `Register` object at the dotted path `name`.
    fn get_register(&mut self, name: &str) -> Result<Register, ExternError> {
        match self.get_object(name)? {
            ObjectState::Register(reg) => Ok(reg),
            _ => {
                Err(error::extern_object_undefined(format!("Register extern {name} not found"))
                    .into())
            }
        }
    }
}

// Multicast mirroring, multicast, and registers; no port mirroring
impl RuntimeInterface for Psa {
    /// Configures clone session `id` to replicate clones to a multicast group.
    fn mirroring_add_session(
        sim: &mut Sim<Self>,
        id: usize,
        config: MirroringSessionConfig,
    ) -> Result<(), ExternError> {
        // Port mirror sessions are not supported
        let MirroringSessionConfig::Mgid(mgid) = config else {
            return p4::runtime_unsupported::<Self, _>("mirroring_add_session with a port");
        };
        let mut arch = sim.get_arch_state()?;
        arch.mirrortable.insert(id, mgid);
        sim.set_arch_state(&arch)
    }

    /// Creates multicast group `mgrp`.
    fn mc_mgrp_create(sim: &mut Sim<Self>, mgrp: usize) -> Result<(), ExternError> {
        let mut arch = sim.get_arch_state()?;
        arch.multicast.group_create(mgrp);
        sim.set_arch_state(&arch)
    }

    /// Creates a multicast node with instance `rid` on `ports`.
    fn mc_node_create(sim: &mut Sim<Self>, rid: usize, ports: &[usize]) -> Result<(), ExternError> {
        let mut arch = sim.get_arch_state()?;
        arch.multicast.node_create(rid, ports);
        sim.set_arch_state(&arch)
    }

    /// Adds node `node` to multicast group `mgrp`.
    fn mc_node_associate(sim: &mut Sim<Self>, mgrp: usize, node: usize) -> Result<(), ExternError> {
        let mut arch = sim.get_arch_state()?;
        arch.multicast.node_associate(mgrp, node);
        sim.set_arch_state(&arch)
    }

    /// The element `idx` of register `name`, or its default when out of range.
    fn register_read(sim: &mut Sim<Self>, name: &str, idx: usize) -> Result<P4Value, ExternError> {
        let reg = sim.get_register(name)?;
        match reg.values.get(idx) {
            Some(value) => Ok(value.clone()),
            None => sim.default_of(&reg.typ),
        }
    }

    /// Writes `value`, cast to the element type, at `idx`; out of range is ignored.
    fn register_write(
        sim: &mut Sim<Self>,
        name: &str,
        idx: usize,
        value: P4Value,
    ) -> Result<(), ExternError> {
        let mut reg = sim.get_register(name)?;
        let value = sim.cast(&reg.typ, value)?;
        if let Some(value_elem) = reg.values.get_mut(idx) {
            *value_elem = value;
        }
        sim.set_object(name, ObjectState::Register(reg))
    }

    /// Resets every element of register `name` to the default.
    fn register_reset(sim: &mut Sim<Self>, name: &str) -> Result<(), ExternError> {
        let mut reg = sim.get_register(name)?;
        let value = sim.default_of(&reg.typ)?;
        reg.values.fill(value);
        sim.set_object(name, ObjectState::Register(reg))
    }
}
