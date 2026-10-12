//! Helpers for invoking relations taking a program in the spec
//!
//! Each architecture's `_init` relation takes the parsed program
//! and returns the initial context and architecture values.

use crate::lang::data::value::flat::{self as value, Value};

use crate::runner::{Extern, ExternError, Interface, Interpreter, InterpreterError, RunnerContext};

// == eBPF

/// Runs `EBPF_init` on a program.
pub fn ebpf_init<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    program: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_program("EBPF_init", program)?;
    let (value_ctx, value_arch) = value::get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}

// == PSA

/// Runs `PSA_init` on a program.
pub fn psa_init<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    program: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_program("PSA_init", program)?;
    let (value_ctx, value_arch) = value::get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}

// == v1model

/// Runs `V1Model_init` on a program.
pub fn v1model_init<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    program: Value,
) -> Result<(Value, Value), InterpreterError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    let values = ctx.call_program("V1Model_init", program)?;
    let (value_ctx, value_arch) = value::get::two(&values).map_err(ExternError::from)?;
    Ok((*value_ctx, *value_arch))
}
