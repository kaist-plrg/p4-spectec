//! Dummy externs initialize empty states and support compile-time assertions
//!
//! Runtime extern function and method calls remain unsupported.

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::{ValueError, extern_value::TypDef, flat::Value},
    },
};

use crate::runner::{ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::error;

// == Configuration

/// The dummy architecture, whose states carry no payload.
pub struct Dummy {
    /// Definition of `archState`.
    typdef_arch: TypDef<()>,
    /// Definition of `objectState`.
    typdef_object: TypDef<()>,
}

impl Dummy {
    /// Defines the architecture's extern syntax types in `arena`.
    pub fn new(arena: &mut Arena) -> Result<Self, ValueError> {
        Ok(Self {
            typdef_arch: TypDef::new(arena, "archState")?,
            typdef_object: TypDef::new(arena, "objectState")?,
        })
    }
}

// == Architectural state

/// The initial architecture state, without a payload.
pub(super) fn init_arch_state<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Dummy>,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Dummy>,
{
    let typdef_arch = ctx.external().typdef_arch;
    Ok(typdef_arch.make(ctx.arena_mut(), (), Span::default())?)
}

// == Extern calls

// - Initialization

/// Every object starts without a payload.
pub(super) fn eval_extern_init<Interp, Iface>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Dummy>,
    _values: &[Value],
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Interp: Interpreter<Iface, Dummy>,
{
    let typdef_object = ctx.external().typdef_object;
    Ok(typdef_object.make(ctx.arena_mut(), (), Span::default())?)
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
