//! v1model architecture with its scheduler, objects, and extern functions
//!
//! Extern calls record clone, resubmit, and recirculate requests
//! in the architecture state;
//! the scheduler in `pipe` acts on them once the control returns.
//! Stateful objects live in `object`, extern functions in `func`.

use p4spectec::lang::data::value::{flat::Value, host::HostTyp};

use p4spectec::runner::{ExternError, Interface, Interpreter, RunnerContext};

use super::externs;

pub(crate) mod arch;
pub(crate) mod func;
pub(crate) mod mirror;
pub(crate) mod multicast;
pub(crate) mod object;
pub(crate) mod packet;
pub(crate) mod pipe;

pub use pipe::V1Model;

// == Extern calls

/// Hands every extern hook to the pipeline module.
impl externs::Impl for V1Model {
    fn host_typdefs(&self) -> Vec<HostTyp> {
        vec![self.arch_state.erase(), self.object_state.erase()]
    }

    fn eval_extern_init<Interp, Iface>(
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
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        pipe::init_arch_state(ctx)
    }
}
