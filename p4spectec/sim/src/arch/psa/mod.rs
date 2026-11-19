//! PSA architecture with its two pipelines, replication engines, and objects
//!
//! Ingress and egress each parse, control, and deparse;
//! the PRE and BQE schedule clones, multicast, resubmit, and recirculate.
//! Stateful objects live in `object`, the scheduler in `pipe`.

use p4spectec::lang::data::value::{flat::Value, host::HostTyp};

use p4spectec::runner::{ExternError, Interface, Interpreter, RunnerContext};

use super::externs;

pub(crate) mod arch;
pub(crate) mod mirror;
pub(crate) mod multicast;
pub(crate) mod object;
pub(crate) mod packet;
pub(crate) mod pipe;

pub use pipe::Psa;

// == Extern calls

/// Hands every extern hook to the pipeline module.
impl externs::Impl for Psa {
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
