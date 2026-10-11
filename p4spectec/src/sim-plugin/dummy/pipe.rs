//! Dummy externs initialize empty states and support compile-time assertions
//!
//! Runtime extern function and method calls remain unsupported.

use crate::lang::{
    common::source::Span,
    data::{
        typ,
        value::flat::{self as value, Value, external::encode},
    },
};

use crate::runner::{ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::error;

// == Configuration

/// The dummy architecture, holding no state.
pub struct Dummy;

// == Architectural state

/// The initial architecture state: an encoded unit value.
pub(super) fn init_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Dummy>,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Dummy>,
{
    let payload = encode(ctx.arena(), &())?;
    let typ = typ::make::var(
        crate::phrase!(node: "archState".to_owned(), span: Span::default()),
        Vec::new(),
    );
    Ok(value::make::external(ctx.arena_mut(), typ.node.into(), payload.into(), Span::default())?)
}

// == Extern calls

// - Initialization

/// Every object starts as an encoded unit value.
pub(super) fn eval_extern_init<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Dummy>,
    _values: &[Value],
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Dummy>,
{
    let payload = encode(ctx.arena(), &())?;
    let typ = typ::make::var(
        crate::phrase!(node: "objectState".to_owned(), span: Span::default()),
        Vec::new(),
    );
    Ok(value::make::external(ctx.arena_mut(), typ.node.into(), payload.into(), Span::default())?)
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
