//! V1Model parses once, runs ingress and egress, then deparses for transmission
//!
//! ```text
//! Rx -> Parser -> Verify checksum -> Ingress -> Egress
//!                                                |
//!                                                v
//! Tx <- Deparser <- Update checksum <-------------+
//! ```
//!
//! The scheduler runs queued ingress and egress packets, including clones and
//! multicast copies; dropping a packet ends its path
//!
//! Resubmit returns the original packet to the parser; recirculate returns the
//! deparsed packet to the parser

use p4spectec::runner::ExternError;

use crate::{
    error,
    p4::{self, BlockResult, P4Value, P4Vars, RelArg, Sim, SpecValue},
};

use crate::spec::host::ArchHost;

use crate::arch::{
    core::object::{PacketIn, PacketOut, packet},
    io::Rx,
    state::SimState,
};

use super::super::{DEFAULT_DROP_PORT, ObjectState, PORT_WIDTH, V1Model, types::CloneType};

use super::{
    packet::{CloneInfo, Entrypoint, Packet},
    state::ArchState,
};

// == Constants

/// The kinds of packet in `standard_metadata.instance_type`,
/// bmv2's `PktInstanceType` in `simple_switch.h`.
#[derive(Clone, Copy, Debug)]
enum PktInstanceType {
    #[allow(dead_code, reason = "a received packet keeps the instance type it starts with")]
    Normal = 0,
    IngressClone = 1,
    EgressClone = 2,
    #[allow(dead_code, reason = "the simulator does not coalesce packets")]
    Coalesced = 3,
    Recirc = 4,
    Replication = 5,
    Resubmit = 6,
}

impl From<PktInstanceType> for P4Value {
    fn from(instance_type: PktInstanceType) -> Self {
        P4Value::Bit { width: 32, int: (instance_type as u32).into() }
    }
}

// == Packet state

/// The `packet_out` object of the current packet.
fn get_packet_out(sim: &mut Sim<V1Model>) -> Result<PacketOut, ExternError> {
    match sim.get_object("packet_out")? {
        ObjectState::PacketOut(packet_out) => Ok(packet_out),
        _ => Err(error::extern_object_undefined("packet_out extern not found".to_owned()).into()),
    }
}

/// Makes `packet_in` the input of the current packet.
fn set_packet_in(sim: &mut Sim<V1Model>, packet_in: PacketIn) -> Result<(), ExternError> {
    sim.set_object("packet_in", ObjectState::PacketIn(packet_in))
}

/// The deparsed packet: the emitted headers followed by the unparsed payload.
fn deparsed_packet(sim: &mut Sim<V1Model>) -> Result<String, ExternError> {
    let packet_in = sim.get_packet_in()?;
    let packet_out = get_packet_out(sim)?;
    packet::to_string(&packet_in, &packet_out)
}

/// Whether `egress_spec` holds the 9-bit drop port.
fn is_dropped(sim: &mut Sim<V1Model>) -> Result<bool, ExternError> {
    match sim.get_global("standard_metadata.egress_spec")? {
        P4Value::Bit { width, int: port } => {
            Ok(width == PORT_WIDTH && port == DEFAULT_DROP_PORT.into())
        }
        egress_spec => Err(egress_spec.mismatch("a bit<W>")),
    }
}

// == Initialization

/// Instantiates the program and returns the initial simulator state.
pub fn init_pipe(
    host: &mut dyn ArchHost<V1Model>,
    program: SpecValue,
) -> Result<SimState, ExternError> {
    p4::init_program(host, "V1Model_init", program)
}

/// Installs the received packet as `packet_in`, an empty `packet_out`,
/// and the standard metadata for `rx.port`.
fn setup_rx(sim: &mut Sim<V1Model>, rx: &Rx) -> Result<(), ExternError> {
    // Install packet_in and packet_out, then set the globals for the input port
    let packet_in = ObjectState::PacketIn(PacketIn::new(&rx.packet)?);
    sim.run_init_object("V1Model_init_packet_in", packet_in)?;
    let packet_out = ObjectState::PacketOut(PacketOut::default());
    sim.run_init_object("V1Model_init_packet_out", packet_out)?;
    sim.run_init("V1Model_init_globals", vec![RelArg::Int(rx.port)])
}

// == Contexts

/// Marks the context as a resubmit preserving field list `idx`.
fn prepare_resubmit_ctx(sim: &mut Sim<V1Model>, idx: usize) -> Result<(), ExternError> {
    // Preserve the `@field_list(idx)` metadata fields
    let arg_idx = RelArg::Value(P4Value::Bit { width: 8, int: idx.into() });
    sim.run_init("V1Model_setup_preserved_meta_fields", vec![arg_idx])?;
    sim.set_global("standard_metadata.instance_type", PktInstanceType::Resubmit)
}

/// Marks the context as a clone to `port` preserving field list `idx`.
fn prepare_clone_ctx(
    sim: &mut Sim<V1Model>,
    clone_type: CloneType,
    port: usize,
    idx: usize,
) -> Result<(), ExternError> {
    // Preserve the `@field_list(idx)` metadata fields
    let arg_idx = RelArg::Value(P4Value::Bit { width: 8, int: idx.into() });
    sim.run_init("V1Model_setup_preserved_meta_fields", vec![arg_idx])?;
    // An I2E clone is an ingress clone, an E2E clone an egress clone
    let instance_type = match clone_type {
        CloneType::I2E => PktInstanceType::IngressClone,
        CloneType::E2E => PktInstanceType::EgressClone,
    };
    sim.set_global("standard_metadata.instance_type", instance_type)?;
    sim.set_global(
        "standard_metadata.egress_spec",
        P4Value::Bit { width: PORT_WIDTH, int: port.into() },
    )
}

/// Marks the context as a recirculation preserving field list `idx`.
fn prepare_recirculate_ctx(sim: &mut Sim<V1Model>, idx: usize) -> Result<(), ExternError> {
    // Preserve the `@field_list(idx)` metadata fields
    let arg_idx = RelArg::Value(P4Value::Bit { width: 8, int: idx.into() });
    sim.run_init("V1Model_setup_preserved_meta_fields", vec![arg_idx])?;
    sim.set_global("standard_metadata.instance_type", PktInstanceType::Recirc)
}

/// Marks the context as replica `rid` of a multicast to `port`.
fn prepare_multicast_ctx(
    sim: &mut Sim<V1Model>,
    rid: usize,
    port: usize,
) -> Result<(), ExternError> {
    // Each replica carries its node's replication id and port
    sim.set_global("standard_metadata.egress_rid", P4Value::Bit { width: 16, int: rid.into() })?;
    sim.set_global(
        "standard_metadata.egress_spec",
        P4Value::Bit { width: PORT_WIDTH, int: port.into() },
    )?;
    sim.set_global("standard_metadata.instance_type", PktInstanceType::Replication)
}

/// Copies `egress_spec` into `egress_port` for the egress control.
fn prepare_egress_ctx(sim: &mut Sim<V1Model>) -> Result<(), ExternError> {
    let port = sim.get_global("standard_metadata.egress_spec")?;
    sim.set_global("standard_metadata.egress_port", port)
}

// == Scheduling

/// Queues the current packet to resume at `entrypoint`.
fn schedule_packet(sim: &mut Sim<V1Model>, entrypoint: Entrypoint) -> Result<(), ExternError> {
    let packet = Packet { value_ctx: sim.get_ctx(), packet_in: sim.get_packet_in()?, entrypoint };
    let mut arch = sim.get_arch_state()?;
    // Ingress work runs before queued egress copies
    match entrypoint {
        Entrypoint::Ingress => arch.queue.push_front(packet),
        Entrypoint::Egress => arch.queue.push_back(packet),
    }
    sim.set_arch_state(&arch)
}

/// Acts on a resubmit request: parse the input again and queue it for ingress;
/// whether one was requested.
fn schedule_resubmit(sim: &mut Sim<V1Model>, arch: &ArchState) -> Result<bool, ExternError> {
    // No resubmit requested
    let Some(idx) = arch.action.resubmit_opt else {
        return Ok(false);
    };
    // The new packet gets its own context; this one continues unchanged
    let value_ctx_original = sim.get_ctx();
    prepare_resubmit_ctx(sim, idx)?;
    parse_and_verify(sim)?;
    schedule_packet(sim, Entrypoint::Ingress)?;
    sim.set_ctx(value_ctx_original);
    Ok(true)
}

/// Acts on a clone request: queue a copy for egress on the session's port.
fn schedule_clone(sim: &mut Sim<V1Model>, arch: &ArchState) -> Result<(), ExternError> {
    // No clone requested
    let Some(CloneInfo(clone_type, session, idx)) = arch.action.clone_opt else {
        return Ok(());
    };
    // An unconfigured session makes no clone
    let Some(&port) = arch.mirrortable.get(&session) else {
        return Ok(());
    };
    // The new packet gets its own context; this one continues unchanged
    let value_ctx_original = sim.get_ctx();
    prepare_clone_ctx(sim, clone_type, port, idx)?;
    // I2E clones are parsed again; E2E clones keep the egress state
    if clone_type == CloneType::I2E {
        parse_and_verify(sim)?;
    }
    schedule_packet(sim, Entrypoint::Egress)?;
    sim.set_ctx(value_ctx_original);
    Ok(())
}

/// Acts on a recirculate request: deparse, then parse the output again;
/// whether one was requested.
fn schedule_recirculate(sim: &mut Sim<V1Model>, arch: &ArchState) -> Result<bool, ExternError> {
    // No recirculate requested
    let Some(idx) = arch.action.recirculate_opt else {
        return Ok(false);
    };
    // The new packet gets its own context; this one continues unchanged
    let value_ctx_original = sim.get_ctx();
    prepare_recirculate_ctx(sim, idx)?;
    checksum_and_deparse(sim)?;
    // The deparsed bytes become the new input
    let packet = deparsed_packet(sim)?;
    set_packet_in(sim, PacketIn::new(&packet)?)?;
    // Parser and verify run before the ingress packet enters the queue
    parse_and_verify(sim)?;
    schedule_packet(sim, Entrypoint::Ingress)?;
    sim.set_ctx(value_ctx_original);
    Ok(true)
}

/// Queues one egress copy per node of multicast `group`.
fn schedule_multicast(
    sim: &mut Sim<V1Model>,
    arch: &ArchState,
    group: usize,
) -> Result<(), ExternError> {
    // Unknown group: nothing is replicated
    let Some(handles) = arch.multicast.groups.get(&group) else {
        return Ok(());
    };
    // One egress copy per (port, rid) node, in association order
    for handle in handles {
        if let Some(nodes) = arch.multicast.nodes.get(handle) {
            for node in nodes {
                prepare_multicast_ctx(sim, node.rid, node.port)?;
                schedule_packet(sim, Entrypoint::Egress)?;
            }
        }
    }
    Ok(())
}

/// Checksums, deparses, and sends the packet out of `egress_spec`.
fn transmit_packet(sim: &mut Sim<V1Model>) -> Result<(), ExternError> {
    checksum_and_deparse(sim)?;
    // The output port is the final `egress_spec`
    let port: usize = sim
        .get_global("standard_metadata.egress_spec")?
        .try_into()?;
    let packet = deparsed_packet(sim)?;
    sim.transmit(port, packet);
    Ok(())
}

// == Blocks

/// Front half of the pipeline: reset requests, rewind the input,
/// then run the parser and the VerifyChecksum control;
/// a parser rejection is recorded in `parser_error`.
fn parse_and_verify(sim: &mut Sim<V1Model>) -> Result<(), ExternError> {
    // Forget the previous pass's requests
    let mut arch = sim.get_arch_state()?;
    arch.reset();
    sim.set_arch_state(&arch)?;
    // Parse the same bytes from the start
    let mut packet_in = sim.get_packet_in()?;
    packet_in.reset();
    set_packet_in(sim, packet_in)?;
    // Parser errors are visible to the controls
    if let BlockResult::Reject(error) = sim.run_block("V1Model_parser")? {
        sim.set_global("standard_metadata.parser_error", P4Value::Error(error))?;
    }
    sim.run_block("V1Model_verify")?;
    Ok(())
}

/// Back half of the pipeline: the ComputeChecksum control,
/// then the deparser into an empty `packet_out`.
fn checksum_and_deparse(sim: &mut Sim<V1Model>) -> Result<(), ExternError> {
    sim.run_block("V1Model_check")?;
    sim.set_object("packet_out", ObjectState::PacketOut(PacketOut::default()))?;
    sim.run_block("V1Model_deparse")?;
    Ok(())
}

/// Runs ingress, then schedules the clone, resubmit, multicast, or unicast.
fn ingress(sim: &mut Sim<V1Model>) -> Result<(), ExternError> {
    sim.run_block("V1Model_ingress")?;
    let arch = sim.get_arch_state()?;
    // A clone request never stops the original
    schedule_clone(sim, &arch)?;
    // A resubmit replaces the packet's normal continuation
    if schedule_resubmit(sim, &arch)? {
        return Ok(());
    }
    let group: usize = sim.get_global("standard_metadata.mcast_grp")?.try_into()?;
    // Multicast wins over unicast; a dropped packet goes nowhere
    if group != 0 {
        schedule_multicast(sim, &arch, group)?;
    } else if !is_dropped(sim)? {
        schedule_packet(sim, Entrypoint::Egress)?;
    }
    Ok(())
}

/// Runs egress, then transmits the packet unless it was dropped or recirculated.
fn egress(sim: &mut Sim<V1Model>) -> Result<(), ExternError> {
    prepare_egress_ctx(sim)?;
    sim.run_block("V1Model_egress")?;
    let arch = sim.get_arch_state()?;
    // A clone request never stops the original
    schedule_clone(sim, &arch)?;
    // Dropped in egress: nothing to transmit
    if is_dropped(sim)? {
        return Ok(());
    }
    // An I2E clone parses again, which clears the recirculate request
    let arch = sim.get_arch_state()?;
    // Recirculation consumes the packet
    if schedule_recirculate(sim, &arch)? {
        return Ok(());
    }
    transmit_packet(sim)
}

// == Driver

/// Resumes a queued packet at its entrypoint.
fn resume_packet(sim: &mut Sim<V1Model>, packet: Packet) -> Result<(), ExternError> {
    // The packet's input and context become current
    set_packet_in(sim, packet.packet_in)?;
    sim.set_ctx(packet.value_ctx);
    match packet.entrypoint {
        Entrypoint::Ingress => ingress(sim),
        Entrypoint::Egress => egress(sim),
    }
}

/// Drains the packet queue, running each packet to its next stop.
fn run_scheduler(sim: &mut Sim<V1Model>) -> Result<(), ExternError> {
    loop {
        let mut arch = sim.get_arch_state()?;
        // Empty queue: the received packet is fully processed
        let Some(packet) = arch.queue.pop_front() else {
            return Ok(());
        };
        // Each packet starts with no pending requests
        arch.reset();
        sim.set_arch_state(&arch)?;
        resume_packet(sim, packet)?;
    }
}

/// Processes one received packet through the pipeline and its scheduler.
pub fn drive_pipe(sim: &mut Sim<V1Model>, rx: &Rx) -> Result<(), ExternError> {
    setup_rx(sim, rx)?;
    parse_and_verify(sim)?;
    schedule_packet(sim, Entrypoint::Ingress)?;
    run_scheduler(sim)
}
