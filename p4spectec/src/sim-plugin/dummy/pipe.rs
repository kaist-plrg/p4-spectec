//! Dummy externs initialize empty states and support compile-time assertions
//!
//! Runtime extern function and method calls remain unsupported.

use crate::lang::{
    common::source::Span,
    data::value::{
        flat::{self as value, Value},
        host::HostTypDef,
    },
};

use crate::runner::{ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::error;

// == Configuration

/// The dummy architecture, with its definitions of the extern types.
pub struct Dummy {
    /// The definition of `archState`, which the dummy architecture leaves empty.
    pub(super) arch_state: HostTypDef<()>,
    /// The definition of `objectState`, which every dummy object leaves empty.
    pub(super) object_state: HostTypDef<()>,
}

// Every instance makes definitions of its own
impl Default for Dummy {
    fn default() -> Self {
        Self {
            arch_state: HostTypDef::new("archState"),
            object_state: HostTypDef::new("objectState"),
        }
    }
}

// == Architectural state

/// The initial architecture state: the unit payload.
pub(super) fn init_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Dummy>,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Dummy>,
{
    let payload = ctx.external().arch_state.payload(());
    Ok(value::make::external(ctx.arena_mut(), payload, Span::default())?)
}

// == Extern calls

// - Initialization

/// Every object starts as the unit payload.
pub(super) fn eval_extern_init<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Dummy>,
    _values: &[Value],
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Dummy>,
{
    let payload = ctx.external().object_state.payload(());
    Ok(value::make::external(ctx.arena_mut(), payload, Span::default())?)
}

// - Function calls

/// Extern function calls are unsupported.
pub(super) fn eval_extern_func_call<Interp, Iface>(
    _ctx: &mut RunnerContext<'_, Interp, Iface, Dummy>,
    _values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Dummy>,
{
    Err(error::extern_relation_unsupported(
        "unimplemented extern relation: ExternFunctionCall_eval".to_owned(),
    )
    .into())
}

// - Method calls

/// Extern method calls are unsupported.
pub(super) fn eval_extern_method_call<Interp, Iface>(
    _ctx: &mut RunnerContext<'_, Interp, Iface, Dummy>,
    _values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Dummy>,
{
    Err(error::extern_relation_unsupported(
        "unimplemented extern relation: ExternMethodCall_eval".to_owned(),
    )
    .into())
}
