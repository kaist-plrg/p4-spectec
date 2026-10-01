//! Architecture operations used by native STF execution
//!
//! An `Architecture` is an extern that also knows how to set up
//! and drive its pipeline
//! and how to apply the STF control-plane statements.
//! Operations an architecture lacks default to a "not implemented" failure.
//! The impls at the bottom delegate to each architecture's `pipe` module.

use num_bigint::BigInt;

use crate::lang::data::value::Value;

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::stf::ast::Statement;

use crate::sim_plugin::error;

use super::{io::Rx, state::SimState};

/// What the STF runner needs from an architecture.
pub trait Architecture: Extern {
    /// The name used on the command line and in failure messages.
    const NAME: &'static str;

    /// Rewrites an STF statement into the form this architecture executes.
    fn transform_stf_stmt(stmt: Statement) -> Statement;

    /// Initializes the pipeline for a parsed program.
    fn init_pipe<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        program: Value,
    ) -> Result<SimState, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Pushes one packet through the pipeline, collecting outputs in the state.
    fn drive_pipe<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        state: &mut SimState,
        rx: &Rx,
    ) -> Result<(), ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// `mirroring_add`: maps a session to a port.
    fn add_mirror_session<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _value_arch: Value,
        _session: usize,
        _port: usize,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(error::control_operation_unsupported(format!(
            "add_mirror_session is not implemented for the {} simulator",
            Self::NAME
        ))
        .into())
    }

    /// `mirroring_add_mc`: maps a session to a multicast group.
    fn add_mirror_session_mc<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _value_arch: Value,
        _session: usize,
        _group: usize,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(error::control_operation_unsupported(format!(
            "add_mirror_session_mc is not implemented for the {} simulator",
            Self::NAME
        ))
        .into())
    }

    /// `mc_mgrp_create`: creates a multicast group.
    fn mc_mgrp_create<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _value_arch: Value,
        _group: usize,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(error::control_operation_unsupported(format!(
            "mc_mgrp_create is not implemented for the {} simulator",
            Self::NAME
        ))
        .into())
    }

    /// `mc_node_create`: creates a replication node over ports.
    fn mc_node_create<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _value_arch: Value,
        _instance: usize,
        _ports: &[usize],
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(error::control_operation_unsupported(format!(
            "mc_node_create is not implemented for the {} simulator",
            Self::NAME
        ))
        .into())
    }

    /// `mc_node_associate`: adds a node to a group.
    fn mc_node_associate<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _value_arch: Value,
        _group: usize,
        _handle: usize,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(error::control_operation_unsupported(format!(
            "mc_node_associate is not implemented for the {} simulator",
            Self::NAME
        ))
        .into())
    }

    /// `register_read`: reads a register cell.
    fn register_read<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _value_arch: Value,
        _name: &str,
        _idx: usize,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(error::control_operation_unsupported(format!(
            "register_read is not implemented for the {} simulator",
            Self::NAME
        ))
        .into())
    }

    /// `register_write`: writes a register cell.
    fn register_write<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _value_arch: Value,
        _name: &str,
        _idx: usize,
        _int: BigInt,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(error::control_operation_unsupported(format!(
            "register_write is not implemented for the {} simulator",
            Self::NAME
        ))
        .into())
    }

    /// `register_reset`: clears a register.
    fn register_reset<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _value_arch: Value,
        _name: &str,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Err(error::control_operation_unsupported(format!(
            "register_reset is not implemented for the {} simulator",
            Self::NAME
        ))
        .into())
    }
}

/// Implements the pipeline methods by forwarding to a `pipe` module.
macro_rules! delegate_pipe {
    ($pipe:path) => {
        fn transform_stf_stmt(stmt: Statement) -> Statement {
            use $pipe;
            pipe::transform_stf_stmt(stmt)
        }

        fn init_pipe<Interp, Iface>(
            ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
            program: Value,
        ) -> Result<SimState, ExternError>
        where
            Iface: Interface,
            Interp: Interpreter<Iface, Self>,
        {
            use $pipe;
            pipe::init_pipe(ctx, program)
        }

        fn drive_pipe<Interp, Iface>(
            ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
            state: &mut SimState,
            rx: &Rx,
        ) -> Result<(), ExternError>
        where
            Iface: Interface,
            Interp: Interpreter<Iface, Self>,
        {
            use $pipe;
            pipe::drive_pipe(ctx, state, rx)
        }
    };
}

/// Implements one control-plane method by forwarding to a `pipe` module.
macro_rules! delegate_method {
    ($pipe:path, $name:ident $(, $arg:ident: $typ:ty)*) => {
        fn $name<Interp, Iface>(
            ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
            value_arch: Value,
            $($arg: $typ),*
        ) -> Result<Value, ExternError>
        where
            Iface: Interface,
            Interp: Interpreter<Iface, Self>,
        {
            use $pipe;
            pipe::$name(ctx, value_arch, $($arg),*)
        }
    };
}

// eBPF has no control plane beyond tables
impl Architecture for super::ebpf::Ebpf {
    const NAME: &'static str = "ebpf";

    delegate_pipe!(super::ebpf::pipe);
}

// PSA has multicast and registers but no port mirroring
impl Architecture for super::psa::Psa {
    const NAME: &'static str = "psa";

    delegate_pipe!(super::psa::pipe);

    delegate_method!(super::psa::pipe, add_mirror_session_mc, session: usize, group: usize);
    delegate_method!(super::psa::pipe, mc_mgrp_create, group: usize);
    delegate_method!(super::psa::pipe, mc_node_create, instance: usize, ports: &[usize]);
    delegate_method!(super::psa::pipe, mc_node_associate, group: usize, handle: usize);
    delegate_method!(super::psa::pipe, register_read, name: &str, idx: usize);
    delegate_method!(super::psa::pipe, register_write, name: &str, idx: usize, int: BigInt);
    delegate_method!(super::psa::pipe, register_reset, name: &str);
}

// v1model supports every operation
impl Architecture for super::v1model::V1Model {
    const NAME: &'static str = "v1model";

    delegate_pipe!(super::v1model::pipe);

    delegate_method!(super::v1model::pipe, add_mirror_session, session: usize, port: usize);
    delegate_method!(super::v1model::pipe, add_mirror_session_mc, session: usize, group: usize);
    delegate_method!(super::v1model::pipe, mc_mgrp_create, group: usize);
    delegate_method!(super::v1model::pipe, mc_node_create, instance: usize, ports: &[usize]);
    delegate_method!(super::v1model::pipe, mc_node_associate, group: usize, handle: usize);
    delegate_method!(super::v1model::pipe, register_read, name: &str, idx: usize);
    delegate_method!(super::v1model::pipe, register_write, name: &str, idx: usize, int: BigInt);
    delegate_method!(super::v1model::pipe, register_reset, name: &str);
}
