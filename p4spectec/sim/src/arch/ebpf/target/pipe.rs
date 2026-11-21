//! eBPF parses the packet, then filters it using the parsed headers
//!
//! ```text
//! Rx -> Parser -> Filter -> accept = true -> Tx
//!         |                  |
//!       reject          accept = false
//!         |                  |
//!         v                  v
//!        drop               drop
//! ```
//!
//! Accepted packets keep their original bytes and input port
//! There is no deparser

use p4spectec::runner::ExternError;

use crate::p4::{self, BlockResult, P4Vars, Sim, SpecValue};

use crate::spec::host::ArchHost;

use crate::arch::{core::object::PacketIn, io::Rx, state::SimState};

use super::super::{Ebpf, ObjectState};

// == Initialization

/// Instantiates the program and returns the initial simulator state.
pub fn init_pipe(
    host: &mut dyn ArchHost<Ebpf>,
    program: SpecValue,
) -> Result<SimState, ExternError> {
    p4::init_program(host, "EBPF_init", program)
}

// == Driver

/// Processes one received packet: parse, filter, and forward if accepted.
///
/// A parser reject or false `accept` drops the packet; no deparser runs.
pub fn drive_pipe(sim: &mut Sim<Ebpf>, rx: &Rx) -> Result<(), ExternError> {
    // Setup packet_in extern and global variables
    let packet_in = ObjectState::PacketIn(PacketIn::new(&rx.packet)?);
    sim.run_init_object("EBPF_init_packet_in", packet_in)?;
    sim.run_init("EBPF_init_globals", vec![])?;
    // A rejected packet is dropped
    if let BlockResult::Reject(_) = sim.run_block("EBPF_parse")? {
        return Ok(());
    }
    sim.run_block("EBPF_filter")?;
    // Forward only when the filter accepted the packet
    let accept: bool = sim.get_global("accept")?.try_into()?;
    if accept {
        sim.transmit(rx.port, rx.packet.clone());
    }
    Ok(())
}
