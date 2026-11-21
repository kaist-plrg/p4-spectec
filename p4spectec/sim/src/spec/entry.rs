//! How the specification calls into the simulator
//!
//! The specification calls externs through a few fixed relation and function names.
//! `impl_extern!` gives each `Architecture` an `Extern` impl that routes each name
//! to a function here, which reads the values handed over into a `p4` call,
//! runs the architecture on it,
//! and builds the values the specification expects back:
//! `ExternFunctionCall_eval` reads a `Signature`
//! and returns the context, the architecture state, and the call result.
//! `Dummy`, which has no pipeline, answers only compile-time calls.

use p4spectec::lang::{
    data::value::{
        flat::{self as value, Value},
        host::HostTyp,
    },
    il::ast::Typ,
};

use p4spectec::runner::{
    Eval, Extern, ExternError, Interface, Interpreter, RunnerContext, SideEffect,
};

use crate::{
    arch::{core::func::call_compile_time, dummy::Dummy, state::SimState},
    error,
    p4::{Architecture, InitEnv, MethodCall, Signature, Sim},
};

use super::{
    SpecValue,
    bridge::{FromSpec, ToSpec},
    host::{ArchHost, SpecHost},
    pipe::{arch_state_to_spec, host_value_to_spec, object_to_spec},
};

// == Runner dispatch

/// Implements `Extern` for each architecture by routing names to the functions below.
///
/// A blanket impl over `Architecture` would implement the core's trait
/// for every type, which only the core may do.
macro_rules! impl_extern {
    ($($arch:ty),+ $(,)?) => {
        $(
            impl Extern for $arch {
                fn host_typdefs(&self) -> Vec<HostTyp> {
                    vec![self.typdef_state().erase(), self.typdef_object().erase()]
                }

                fn eval_rel<Interp, Iface>(
                    ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
                    name: &str,
                    values: &[Value],
                ) -> Eval<Vec<Value>, ExternError>
                where
                    Iface: Interface,
                    Interp: Interpreter<Iface, Self>,
                {
                    let checkpoint = ctx.effect_checkpoint();
                    let result = match name {
                        "ExternFunctionCall_eval_lctk" => eval_compile_time_call(ctx, values),
                        "ExternFunctionCall_eval" => eval_function_call(ctx, values),
                        "ExternMethodCall_eval" => eval_method_call(ctx, values),
                        _ => Err(error::extern_relation_unsupported(format!(
                            "unimplemented extern relation: {name}"
                        ))
                        .into()),
                    };
                    // State is in the values; reentry and output can still have effects
                    let effect =
                        if result.is_err() { SideEffect::Impure } else { ctx.effect_since(checkpoint) };
                    Eval::new(result, effect)
                }

                fn eval_func<Interp, Iface>(
                    ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
                    name: &str,
                    _targs: &[Typ],
                    values: &[Value],
                ) -> Eval<Value, ExternError>
                where
                    Iface: Interface,
                    Interp: Interpreter<Iface, Self>,
                {
                    let checkpoint = ctx.effect_checkpoint();
                    let result = match name {
                        "init_objectState" => eval_init_object(ctx, values),
                        "init_archState" => {
                            arch_state_to_spec(ctx, &<Self as Architecture>::State::default()).map(|value| value.0)
                        }
                        _ => Err(error::extern_function_unsupported(format!(
                            "unimplemented extern function: {name}"
                        ))
                        .into()),
                    };
                    // Preserve the conservative effect classification of host failures
                    let effect =
                        if result.is_err() { SideEffect::Impure } else { ctx.effect_since(checkpoint) };
                    Eval::new(result, effect)
                }

                fn clear(&mut self) {}
            }
        )+
    };
}

impl_extern!(crate::arch::ebpf::Ebpf, crate::arch::psa::Psa, crate::arch::v1model::V1Model);

/// Answers compile-time calls only; every state is the unit payload.
impl Extern for Dummy {
    fn host_typdefs(&self) -> Vec<HostTyp> {
        vec![self.typdef_state.erase(), self.typdef_object.erase()]
    }

    fn eval_rel<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let checkpoint = ctx.effect_checkpoint();
        let result = match name {
            "ExternFunctionCall_eval_lctk" => eval_compile_time_call(ctx, values),
            _ => Err(error::extern_relation_unsupported(format!(
                "unimplemented extern relation: {name}"
            ))
            .into()),
        };
        // State is in the values; reentry and output can still have effects
        let effect =
            if result.is_err() { SideEffect::Impure } else { ctx.effect_since(checkpoint) };
        Eval::new(result, effect)
    }

    fn eval_func<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let checkpoint = ctx.effect_checkpoint();
        let result = match name {
            "init_objectState" => {
                let typdef = ctx.external().typdef_object;
                host_value_to_spec(ctx, typdef, ()).map(|value| value.0)
            }
            "init_archState" => {
                let typdef = ctx.external().typdef_state;
                host_value_to_spec(ctx, typdef, ()).map(|value| value.0)
            }
            _ => Err(error::extern_function_unsupported(format!(
                "unimplemented extern function: {name}"
            ))
            .into()),
        };
        // Preserve the conservative effect classification of host failures
        let effect =
            if result.is_err() { SideEffect::Impure } else { ctx.effect_since(checkpoint) };
        Eval::new(result, effect)
    }

    fn clear(&mut self) {}
}

// == Extern calls

/// `ExternFunctionCall_eval_lctk`: evaluates a compile-time known function call;
/// the call result.
fn eval_compile_time_call(
    host: &mut dyn SpecHost,
    values: &[Value],
) -> Result<Vec<Value>, ExternError> {
    let [value_ctx, value_name, value_params] = values else {
        return Err(arity_mismatch("local compile-time known extern function call"));
    };
    let signature = read_signature(host, value_name, value_params)?;
    let p4value = call_compile_time(host, &signature, SpecValue(*value_ctx))?;
    Ok(vec![p4value.to_spec(host.arena_mut())?])
}

/// `ExternFunctionCall_eval`: evaluates an extern function call;
/// the context, the architecture state, and the call result.
fn eval_function_call<Arch: Architecture>(
    host: &mut dyn ArchHost<Arch>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError> {
    let [value_ctx, value_arch, value_name, value_params] = values else {
        return Err(arity_mismatch("extern function call"));
    };
    // Read the call and the state it starts in
    let signature = read_signature(host, value_name, value_params)?;
    let mut state = SimState::new(SpecValue(*value_ctx), SpecValue(*value_arch));
    // Run it, then return the state it leaves with its result
    let call_result = Arch::call_function(&mut Sim::new(host, &mut state), &signature)?;
    let value_result = call_result.to_spec(host.arena_mut())?;
    Ok(vec![state.value_ctx.0, state.value_arch.0, value_result])
}

/// `ExternMethodCall_eval`: evaluates a method call on an extern object;
/// the context, the architecture state, and the call result.
fn eval_method_call<Arch: Architecture>(
    host: &mut dyn ArchHost<Arch>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError> {
    let [value_ctx, value_arch, value_id, value_name, value_params] = values else {
        return Err(arity_mismatch("extern method call"));
    };
    // Read the call and the state it starts in
    let call = MethodCall {
        value_id: SpecValue(*value_id),
        ids: Vec::from_spec(host.arena_mut(), value_id)?,
        signature: read_signature(host, value_name, value_params)?,
    };
    let mut state = SimState::new(SpecValue(*value_ctx), SpecValue(*value_arch));
    // Run it, then return the state it leaves with its result
    let call_result = Arch::call_method(&mut Sim::new(host, &mut state), &call)?;
    let value_result = call_result.to_spec(host.arena_mut())?;
    Ok(vec![state.value_ctx.0, state.value_arch.0, value_result])
}

/// `init_objectState`: creates the initial state of an extern object
/// from its type name, type arguments, parameter names, and arguments.
fn eval_init_object<Arch: Architecture>(
    host: &mut dyn ArchHost<Arch>,
    values: &[Value],
) -> Result<Value, ExternError> {
    let (value_name, value_targs, value_ids, value_args) = value::get::four(values)?;
    let name = String::from_spec(host.arena_mut(), value_name)?;
    // Construct the object from its arguments
    let mut env =
        InitEnv::new(host, SpecValue(*value_targs), SpecValue(*value_ids), SpecValue(*value_args))?;
    let object = Arch::init_object(&name, &mut env)?;
    Ok(object_to_spec(host, object)?.0)
}

// == Helpers

/// Reads a signature from the callee's name and parameter names.
fn read_signature(
    host: &mut dyn SpecHost,
    value_name: &Value,
    value_params: &Value,
) -> Result<Signature, ExternError> {
    Ok(Signature {
        name: String::from_spec(host.arena_mut(), value_name)?,
        params: Vec::from_spec(host.arena_mut(), value_params)?,
    })
}

/// The failure for a call with the wrong number of values.
fn arity_mismatch(kind: &str) -> ExternError {
    error::extern_argument_arity_mismatch(format!("unexpected number of arguments to {kind}"))
        .into()
}
