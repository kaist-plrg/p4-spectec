//! The v1model control plane
//!
//! Mirror sessions send clones to a port,
//! and multicast groups replicate a packet to their nodes;
//! both live in the architecture state.
//! Registers are not configured from STF.

use p4spectec::runner::ExternError;

use crate::p4::{self, MirroringSessionConfig, RuntimeInterface, Sim};

use super::super::V1Model;

// == Control plane

// Port mirroring and multicast; registers are not configured from STF
impl RuntimeInterface for V1Model {
    /// Configures mirror session `id` to send clones to a port.
    fn mirroring_add_session(
        sim: &mut Sim<Self>,
        id: usize,
        config: MirroringSessionConfig,
    ) -> Result<(), ExternError> {
        // Multicast mirror sessions are not supported
        let MirroringSessionConfig::EgressPort(port) = config else {
            return p4::runtime_unsupported::<Self, _>("mirroring_add_session with a group");
        };
        let mut arch = sim.get_arch_state()?;
        arch.mirrortable.insert(id, port);
        sim.set_arch_state(&arch)
    }

    /// Creates multicast group `mgrp`.
    fn mc_mgrp_create(sim: &mut Sim<Self>, mgrp: usize) -> Result<(), ExternError> {
        let mut arch = sim.get_arch_state()?;
        arch.multicast.group_create(mgrp);
        sim.set_arch_state(&arch)
    }

    /// Creates a multicast node with replication id `rid` on `ports`.
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
}
