//! PSA has a parser, control, and deparser for both ingress and egress
//!
//! ```text
//! Rx -> Ingress parser -> Ingress control -> Ingress deparser -> PRE
//!                                                               |
//!                                                               v
//!     Egress parser <- queued packet <--------------------------+
//!           |
//!           v
//!     Egress control -> Egress deparser -> BQE -> Tx
//! ```
//!
//! The packet replication engine (PRE) sends unicast, multicast, and ingress
//! clones to egress, or resubmits to ingress
//! The buffering queueing engine (BQE) sends egress clones to egress, or
//! recirculates to ingress
//!
//! Both engines can drop the current packet after scheduling its clones
//! The scheduler runs queued packets until none remain

use p4spectec::runner::ExternError;

use crate::p4::{self, BlockResult, P4Value, P4Vars, RelArg, Sim, SpecValue};

use crate::spec::host::ArchHost;

use crate::error;

use crate::arch::{
    core::object::{PacketIn, PacketOut, packet},
    io::Rx,
    state::SimState,
};

use super::super::{ObjectState, Psa};

use super::packet::{Entrypoint, Packet};

// == Constants

/// The recirculation port, bmv2's `PSA_PORT_RECIRCULATE` in `psa_switch.h`.
const PSA_PORT_RECIRCULATE: usize = 0xffff_fffa;

/// The width of `PortId_t` in bmv2's `psa.p4`.
const PORT_WIDTH: usize = 32;

/// The paths a packet takes, `PSA_PacketPath_t` in `psa.p4`.
#[derive(Clone, Copy, Debug)]
enum PacketPath {
    #[allow(dead_code, reason = "PSA_ingress_init_globals sets up a received packet as NORMAL")]
    Normal,
    NormalUnicast,
    NormalMulticast,
    CloneI2E,
    CloneE2E,
    Resubmit,
    Recirculate,
}

impl PacketPath {
    /// The member name, as the metadata relations take it.
    fn name(self) -> &'static str {
        match self {
            Self::Normal => "NORMAL",
            Self::NormalUnicast => "NORMAL_UNICAST",
            Self::NormalMulticast => "NORMAL_MULTICAST",
            Self::CloneI2E => "CLONE_I2E",
            Self::CloneE2E => "CLONE_E2E",
            Self::Resubmit => "RESUBMIT",
            Self::Recirculate => "RECIRCULATE",
        }
    }
}

// == Packet state

/// The name of the `packet_in` object of the ingress or egress pipeline.
fn packet_in_name(entrypoint: Entrypoint) -> &'static str {
    match entrypoint {
        Entrypoint::Ingress => "ingress_packet_in",
        Entrypoint::Egress => "egress_packet_in",
    }
}

/// The name of the `packet_out` object of the ingress or egress pipeline.
fn packet_out_name(entrypoint: Entrypoint) -> &'static str {
    match entrypoint {
        Entrypoint::Ingress => "ingress_packet_out",
        Entrypoint::Egress => "egress_packet_out",
    }
}

/// The `packet_in` object of the ingress or egress pipeline.
fn get_packet_in(sim: &mut Sim<Psa>, entrypoint: Entrypoint) -> Result<PacketIn, ExternError> {
    let name = packet_in_name(entrypoint);
    match sim.get_object(name)? {
        ObjectState::PacketIn(packet_in) => Ok(packet_in),
        _ => Err(error::extern_object_undefined(format!("{name} extern not found")).into()),
    }
}

/// The `packet_out` object of the ingress or egress pipeline.
fn get_packet_out(sim: &mut Sim<Psa>, entrypoint: Entrypoint) -> Result<PacketOut, ExternError> {
    let name = packet_out_name(entrypoint);
    match sim.get_object(name)? {
        ObjectState::PacketOut(packet_out) => Ok(packet_out),
        _ => Err(error::extern_object_undefined(format!("{name} extern not found")).into()),
    }
}

/// Makes `packet_in` the input of the ingress or egress pipeline.
fn set_packet_in(
    sim: &mut Sim<Psa>,
    entrypoint: Entrypoint,
    packet_in: PacketIn,
) -> Result<(), ExternError> {
    sim.set_object(packet_in_name(entrypoint), ObjectState::PacketIn(packet_in))
}

/// Replaces the `packet_out` of the ingress or egress pipeline with an empty one.
fn clear_packet_out(sim: &mut Sim<Psa>, entrypoint: Entrypoint) -> Result<(), ExternError> {
    sim.set_object(packet_out_name(entrypoint), ObjectState::PacketOut(PacketOut::default()))
}

/// The deparsed packet of the ingress or egress pipeline:
/// the emitted headers followed by the unparsed payload.
fn deparsed_packet(sim: &mut Sim<Psa>, entrypoint: Entrypoint) -> Result<String, ExternError> {
    let packet_in = get_packet_in(sim, entrypoint)?;
    let packet_out = get_packet_out(sim, entrypoint)?;
    packet::to_string(&packet_in, &packet_out)
}

/// Whether the egress port is the 32-bit recirculation port.
fn is_egress_recirculate(sim: &mut Sim<Psa>) -> Result<bool, ExternError> {
    match sim.get_global("egress_input_metadata.egress_port")? {
        P4Value::Bit { width, int: port } => {
            Ok(width == PORT_WIDTH && port == PSA_PORT_RECIRCULATE.into())
        }
        egress_port => Err(egress_port.mismatch("a bit<W>")),
    }
}

// == Initialization

/// Instantiates the program and returns the initial simulator state.
pub fn init_pipe(
    host: &mut dyn ArchHost<Psa>,
    program: SpecValue,
) -> Result<SimState, ExternError> {
    p4::init_program(host, "PSA_init", program)
}

/// Installs the received packet and metadata for both pipelines.
fn setup_rx(sim: &mut Sim<Psa>, rx: &Rx) -> Result<(), ExternError> {
    // Set up packet_in objects
    let packet_in = ObjectState::PacketIn(PacketIn::new(&rx.packet)?);
    sim.run_init_object("PSA_ingress_init_packet_in", packet_in.clone())?;
    sim.run_init_object("PSA_egress_init_packet_in", packet_in)?;
    // Set up packet_out objects
    let packet_out = ObjectState::PacketOut(PacketOut::default());
    sim.run_init_object("PSA_ingress_init_packet_out", packet_out.clone())?;
    sim.run_init_object("PSA_egress_init_packet_out", packet_out)?;
    // Set up global variables
    sim.run_init("PSA_ingress_init_globals", vec![RelArg::Int(rx.port)])?;
    sim.run_init("PSA_egress_init_globals", vec![RelArg::Int(rx.port)])
}

// == Contexts

/// Initializes the ingress metadata for input `port` on packet `path`.
fn init_ingress_metadata(
    sim: &mut Sim<Psa>,
    port: usize,
    path: PacketPath,
) -> Result<(), ExternError> {
    let args = vec![RelArg::Int(port), RelArg::Text(path.name().to_owned())];
    sim.run_init("PSA_ingress_init_metadata", args)
}

/// Initializes the egress metadata for output `port` on packet `path`,
/// with class of service `cos` and replica `instance`.
fn init_egress_metadata(
    sim: &mut Sim<Psa>,
    port: usize,
    path: PacketPath,
    cos: usize,
    instance: usize,
) -> Result<(), ExternError> {
    let args = vec![
        RelArg::Int(port),
        RelArg::Text(path.name().to_owned()),
        RelArg::Int(cos),
        RelArg::Int(instance),
    ];
    sim.run_init("PSA_egress_init_metadata", args)
}

/// Sets up egress metadata for a normal unicast to the chosen port.
fn prepare_unicast_ctx(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    let port: usize = sim
        .get_global("ingress_output_metadata.egress_port")?
        .try_into()?;
    let cos: usize = sim
        .get_global("ingress_output_metadata.class_of_service")?
        .try_into()?;
    init_egress_metadata(sim, port, PacketPath::NormalUnicast, cos, 0)
}

/// Sets up egress metadata for a multicast replica.
fn prepare_multicast_ctx(
    sim: &mut Sim<Psa>,
    instance: usize,
    port: usize,
) -> Result<(), ExternError> {
    let cos: usize = sim
        .get_global("ingress_output_metadata.class_of_service")?
        .try_into()?;
    init_egress_metadata(sim, port, PacketPath::NormalMulticast, cos, instance)
}

/// Sets up egress metadata for an ingress-to-egress clone.
fn prepare_clone_i2e_ctx(
    sim: &mut Sim<Psa>,
    instance: usize,
    port: usize,
) -> Result<(), ExternError> {
    let cos: usize = sim
        .get_global("ingress_output_metadata.class_of_service")?
        .try_into()?;
    init_egress_metadata(sim, port, PacketPath::CloneI2E, cos, instance)
}

/// Sets up egress metadata for an egress-to-egress clone.
fn prepare_clone_e2e_ctx(
    sim: &mut Sim<Psa>,
    instance: usize,
    port: usize,
) -> Result<(), ExternError> {
    let cos: usize = sim
        .get_global("egress_input_metadata.class_of_service")?
        .try_into()?;
    init_egress_metadata(sim, port, PacketPath::CloneE2E, cos, instance)
}

/// Sets up ingress metadata for a resubmit.
fn prepare_resubmit_ctx(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    let port: usize = sim
        .get_global("ingress_input_metadata.ingress_port")?
        .try_into()?;
    init_ingress_metadata(sim, port, PacketPath::Resubmit)
}

/// Sets up ingress metadata for a recirculation.
fn prepare_recirculate_ctx(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    init_ingress_metadata(sim, PSA_PORT_RECIRCULATE, PacketPath::Recirculate)
}

// == Scheduling

/// Queues the current packet to resume at `entrypoint`.
fn schedule_packet(sim: &mut Sim<Psa>, entrypoint: Entrypoint) -> Result<(), ExternError> {
    // Ingress and egress read their own `packet_in`
    let packet =
        Packet { value_ctx: sim.get_ctx(), packet_in: get_packet_in(sim, entrypoint)?, entrypoint };
    let mut arch = sim.get_arch_state()?;
    arch.queue.push_back(packet);
    sim.set_arch_state(&arch)
}

/// Deparses the ingress packet and queues one egress copy.
fn schedule_unicast(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    // The deparsed ingress packet is the egress input
    let packet = deparsed_packet(sim, Entrypoint::Ingress)?;
    set_packet_in(sim, Entrypoint::Egress, PacketIn::new(&packet)?)?;
    prepare_unicast_ctx(sim)?;
    schedule_packet(sim, Entrypoint::Egress)
}

/// Queues one egress copy per node of multicast `group`.
fn schedule_multicast(sim: &mut Sim<Psa>, group: usize) -> Result<(), ExternError> {
    let arch = sim.get_arch_state()?;
    // Unknown group: nothing is replicated
    let Some(handles) = arch.multicast.groups.get(&group) else {
        return Ok(());
    };
    // The deparsed ingress packet is the egress input of every copy
    let packet = deparsed_packet(sim, Entrypoint::Ingress)?;
    set_packet_in(sim, Entrypoint::Egress, PacketIn::new(&packet)?)?;
    // One egress copy per (port, instance) node, each from the same context
    for handle in handles {
        if let Some(nodes) = arch.multicast.nodes.get(handle) {
            for node in nodes {
                let value_ctx_original = sim.get_ctx();
                prepare_multicast_ctx(sim, node.instance, node.port)?;
                schedule_packet(sim, Entrypoint::Egress)?;
                sim.set_ctx(value_ctx_original);
            }
        }
    }
    Ok(())
}

/// Queues ingress-to-egress clones for the session's group.
fn schedule_clone_i2e(sim: &mut Sim<Psa>, session: usize) -> Result<(), ExternError> {
    let arch = sim.get_arch_state()?;
    // An unconfigured session makes no clone
    let Some(group) = arch.mirrortable.get(&session) else {
        return Ok(());
    };
    let Some(handles) = arch.multicast.groups.get(group) else {
        return Ok(());
    };
    // The clone is the ingress input from its start; the objects are restored after
    let objects_original = sim.save_objects();
    let mut packet_in_clone = get_packet_in(sim, Entrypoint::Ingress)?;
    packet_in_clone.reset();
    set_packet_in(sim, Entrypoint::Egress, packet_in_clone)?;
    // One egress copy per (port, instance) node, each from the same context
    for handle in handles {
        if let Some(nodes) = arch.multicast.nodes.get(handle) {
            for node in nodes {
                let value_ctx_original = sim.get_ctx();
                prepare_clone_i2e_ctx(sim, node.instance, node.port)?;
                schedule_packet(sim, Entrypoint::Egress)?;
                sim.set_ctx(value_ctx_original);
            }
        }
    }
    // Restore the objects, keeping the queued copies
    sim.restore_objects(objects_original)
}

/// Queues egress-to-egress clones for the session's group.
fn schedule_clone_e2e(sim: &mut Sim<Psa>, session: usize) -> Result<(), ExternError> {
    let arch = sim.get_arch_state()?;
    // An unconfigured session makes no clone
    let Some(group) = arch.mirrortable.get(&session) else {
        return Ok(());
    };
    let Some(handles) = arch.multicast.groups.get(group) else {
        return Ok(());
    };
    // The clone is the deparsed egress packet; the objects are restored after
    let objects_original = sim.save_objects();
    let packet = deparsed_packet(sim, Entrypoint::Egress)?;
    set_packet_in(sim, Entrypoint::Egress, PacketIn::new(&packet)?)?;
    // One egress copy per (port, instance) node, each from the same context
    for handle in handles {
        if let Some(nodes) = arch.multicast.nodes.get(handle) {
            for node in nodes {
                let value_ctx_original = sim.get_ctx();
                prepare_clone_e2e_ctx(sim, node.instance, node.port)?;
                schedule_packet(sim, Entrypoint::Egress)?;
                sim.set_ctx(value_ctx_original);
            }
        }
    }
    // Restore the objects, keeping the queued copies
    sim.restore_objects(objects_original)
}

/// Rewinds the ingress input and queues it for ingress again.
fn schedule_resubmit(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    prepare_resubmit_ctx(sim)?;
    // Parse the same bytes from the start
    let mut packet_in = get_packet_in(sim, Entrypoint::Ingress)?;
    packet_in.reset();
    set_packet_in(sim, Entrypoint::Ingress, packet_in)?;
    schedule_packet(sim, Entrypoint::Ingress)
}

/// Deparses the egress packet and queues it for ingress again.
fn schedule_recirculate(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    // The deparsed egress packet is the ingress input
    let packet = deparsed_packet(sim, Entrypoint::Egress)?;
    set_packet_in(sim, Entrypoint::Ingress, PacketIn::new(&packet)?)?;
    prepare_recirculate_ctx(sim)?;
    schedule_packet(sim, Entrypoint::Ingress)
}

/// Sends the deparsed egress packet out of its egress port.
fn transmit_packet(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    let port: usize = sim
        .get_global("egress_input_metadata.egress_port")?
        .try_into()?;
    let packet = deparsed_packet(sim, Entrypoint::Egress)?;
    sim.transmit(port, packet);
    Ok(())
}

// == Blocks

/// Runs the ingress parser, control, and deparser in order;
/// a parser rejection is recorded in `parser_error`.
fn ingress_pipeline(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    // A parser rejection is visible to the control
    if let BlockResult::Reject(error) = sim.run_block("PSA_ingress_parser")? {
        sim.set_global("ingress_input_metadata.parser_error", P4Value::Error(error))?;
    }
    sim.run_block("PSA_ingress")?;
    // The deparser writes into an empty `ingress_packet_out`
    clear_packet_out(sim, Entrypoint::Ingress)?;
    sim.run_block("PSA_ingress_deparser")?;
    Ok(())
}

/// Runs the egress parser, control, and deparser in order;
/// a parser rejection is recorded in `parser_error`.
fn egress_pipeline(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    // A parser rejection is visible to the control
    if let BlockResult::Reject(error) = sim.run_block("PSA_egress_parser")? {
        sim.set_global("egress_input_metadata.parser_error", P4Value::Error(error))?;
    }
    sim.run_block("PSA_egress")?;
    // The deparser writes into an empty `egress_packet_out`
    clear_packet_out(sim, Entrypoint::Egress)?;
    sim.run_block("PSA_egress_deparser")?;
    Ok(())
}

/// The packet replication engine: clone, drop, resubmit, multicast, or unicast.
fn run_pre(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    // A clone goes to egress whatever happens to the original
    if sim
        .get_global("ingress_output_metadata.clone")?
        .try_into()?
    {
        let session: usize = sim
            .get_global("ingress_output_metadata.clone_session_id")?
            .try_into()?;
        schedule_clone_i2e(sim, session)?;
    }
    // A dropped packet goes nowhere, though its clone was scheduled
    if sim.get_global("ingress_output_metadata.drop")?.try_into()? {
        return Ok(());
    }
    // A resubmit replaces the packet's normal continuation
    if sim
        .get_global("ingress_output_metadata.resubmit")?
        .try_into()?
    {
        return schedule_resubmit(sim);
    }
    let group: usize = sim
        .get_global("ingress_output_metadata.multicast_group")?
        .try_into()?;
    // A non-zero group multicasts; otherwise unicast
    if group != 0 { schedule_multicast(sim, group) } else { schedule_unicast(sim) }
}

/// The buffering queueing engine: clone, drop, recirculate, or transmit.
fn run_bqe(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    // A clone goes to egress whatever happens to the original
    if sim.get_global("egress_output_metadata.clone")?.try_into()? {
        let session: usize = sim
            .get_global("egress_output_metadata.clone_session_id")?
            .try_into()?;
        schedule_clone_e2e(sim, session)?;
    }
    // A dropped packet goes nowhere, though its clone was scheduled
    if sim.get_global("egress_output_metadata.drop")?.try_into()? {
        return Ok(());
    }
    // Recirculation returns the packet to ingress instead of transmitting
    if is_egress_recirculate(sim)? { schedule_recirculate(sim) } else { transmit_packet(sim) }
}

// == Driver

/// Resumes a queued packet through its pipeline and replication engine.
fn resume_packet(sim: &mut Sim<Psa>, packet: Packet) -> Result<(), ExternError> {
    // The packet's input and context become current
    set_packet_in(sim, packet.entrypoint, packet.packet_in)?;
    sim.set_ctx(packet.value_ctx);
    match packet.entrypoint {
        Entrypoint::Ingress => {
            ingress_pipeline(sim)?;
            run_pre(sim)
        }
        Entrypoint::Egress => {
            egress_pipeline(sim)?;
            run_bqe(sim)
        }
    }
}

/// Drains the packet queue, running each packet to its next stop.
fn run_scheduler(sim: &mut Sim<Psa>) -> Result<(), ExternError> {
    loop {
        let mut arch = sim.get_arch_state()?;
        // Empty queue: the received packet is fully processed
        let Some(packet) = arch.queue.pop_front() else {
            return Ok(());
        };
        sim.set_arch_state(&arch)?;
        resume_packet(sim, packet)?;
    }
}

/// Processes one received packet through both pipelines and the scheduler.
pub fn drive_pipe(sim: &mut Sim<Psa>, rx: &Rx) -> Result<(), ExternError> {
    setup_rx(sim, rx)?;
    schedule_packet(sim, Entrypoint::Ingress)?;
    run_scheduler(sim)
}
