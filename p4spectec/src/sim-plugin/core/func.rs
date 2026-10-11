//! Core extern functions `static_assert` and `verify`
//!
//! Both read their arguments from the specification's local context
//! and return specification values:
//! a boolean, or a parser `RETURN`/`REJECT` result.

use crate::lang::{
    common::source::Span,
    data::{
        typ,
        value::flat::{self as value, Value},
    },
};

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::error;

use super::super::spec::{func, unpack};

/// Evaluates a boolean expression at compilation time
/// and stops compilation with the supplied message when it is false.
///
/// The boolean result can initialize a global constant, for example:
/// ```text
/// const bool _check = static_assert(
///     V1MODEL_VERSION > 20180000,
///     "Expected a v1 model version >= 20180000");
/// ```
///
/// The overload without a message uses the default failure message:
/// ```text
/// extern bool static_assert(bool check, string message);
/// extern bool static_assert(bool check);
/// ```
pub fn static_assert<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: &Value,
    has_message: bool,
) -> Result<Value, ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // Arguments are bound as local variables of the call
    let value_check = func::find_var_value_t_local(ctx, value_ctx, "check")?;
    let value_message = if has_message {
        Some(func::find_var_value_t_local(ctx, value_ctx, "message")?)
    } else {
        None
    };
    let check = unpack::p4_bool(ctx.arena(), &value_check)?;
    // A passing assertion evaluates to its check
    if check {
        return Ok(value_check);
    }
    // The default message when the one-argument overload is used
    let message = match value_message {
        Some(value) => unpack::p4_string(ctx.arena(), &value)?,
        None => "static_assert failed".to_owned(),
    };
    Err(error::assertion_unmet(message).into())
}

/// Checks a predicate in the parser, leaving execution unchanged when true.
///
/// A false predicate sets the parser error to `toSignal` and transitions to
/// the `reject` state:
/// ```text
/// extern void verify(in bool check, in error toSignal);
/// ```
pub fn verify<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    value_ctx: Value,
    value_arch: Value,
) -> Result<(Value, Value, Value), ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // Arguments are bound as local variables of the call
    let value_check = func::find_var_e_local(ctx, value_ctx, "check")?;
    let value_signal = func::find_var_e_local(ctx, value_ctx, "toSignal")?;
    let check = unpack::p4_bool(ctx.arena(), &value_check)?;
    // True: return nothing; false: reject with the given error
    let value_call_result = if check {
        let typ = typ::make::opt(typ::make::var(
            crate::phrase!(node: "value".to_owned(), span: Span::default()),
            Vec::new(),
        ));
        let value_opt = value::make::opt(ctx.arena_mut(), typ.node.into(), None, Span::default())?;
        value::make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "RETURN value?",
            args: vec![value_opt],
            typ: "returnResult",
            span: Span::default(),
        }?
    } else {
        value::make::case_shaped! {
            arena: ctx.arena_mut(),
            shape: "REJECT errorValue",
            args: vec![value_signal],
            typ: "rejectResult",
            span: Span::default(),
        }?
    };
    Ok((value_ctx, value_arch, value_call_result))
}
