//! Dummy architecture with empty states and no packet pipeline
//!
//! Used when a program needs no target model.
//! Compile-time assertions such as `static_assert` are supported;
//! runtime extern function and method calls are not.

use p4spectec::lang::data::value::{flat::Value, host::HostTyp};

use p4spectec::runner::{ExternError, Interface, Interpreter, RunnerContext};

use super::externs;

pub mod pipe;

pub use pipe::Dummy;

// == Extern calls

/// Hands every extern hook to the pipeline module.
impl externs::Impl for Dummy {
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
