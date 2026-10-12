//! v1model architecture with its scheduler, objects, and extern functions
//!
//! Extern calls record clone, resubmit, and recirculate requests
//! in the architecture state;
//! the scheduler in `pipe` acts on them once the control returns.
//! Stateful objects live in `object`, extern functions in `func`.

use crate::lang::data::value::flat::Value;

use crate::runner::{ExternError, Interface, Interpreter, RunnerContext};

use super::externs;

pub mod arch;
pub mod func;
pub mod mirror;
pub mod multicast;
pub mod object;
pub mod packet;
pub mod pipe;

pub use pipe::{V1Model, drive_pipe, init_pipe, transform_stf_stmt};

// == Extern calls

/// Hands every extern hook to the pipeline module.
impl externs::Impl for V1Model {
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
