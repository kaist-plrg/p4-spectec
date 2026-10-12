//! eBPF architecture that parses, filters, and forwards accepted packets
//!
//! Extern calls reach the pipeline module;
//! the only stateful object is a counter array.

use crate::lang::data::value::flat::Value;

use crate::runner::{ExternError, Interface, Interpreter, RunnerContext};

use super::externs;

pub mod object;
pub mod pipe;

pub use pipe::{Ebpf, drive_pipe, init_pipe, transform_stf_stmt};

// == Extern calls

/// Hands every extern hook to the pipeline module.
impl externs::Impl for Ebpf {
    fn eval_extern_init<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        pipe::eval_extern_init(ctx, values)
    }

    fn eval_extern_func_call<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Result<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        pipe::eval_extern_func_call(ctx, values)
    }

    fn eval_extern_method_call<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Result<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        pipe::eval_extern_method_call(ctx, values)
    }

    fn init_arch_state<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        pipe::init_arch_state(ctx)
    }
}
