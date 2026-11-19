//! Extern dispatch shared by the architectures
//!
//! The specification calls externs through a few fixed relation
//! and function names;
//! `Impl` is what an architecture provides for them,
//! and `impl_extern!` gives each architecture an `Extern` impl
//! that routes each name to it.
//! Compile-time known calls (`static_assert`) are handled here
//! for all architectures.

use p4spectec::lang::data::value::{flat::Value, host::HostTyp};

use p4spectec::lang::il::ast::Typ;

use p4spectec::runner::{
    Eval, Extern, ExternError, Interface, Interpreter, RunnerContext, SideEffect,
};

use crate::error;

use super::core;

// == Architecture extern operations

/// Architecture-specific extern behavior.
pub(crate) trait Impl: Extern {
    /// Gives the definitions of `archState` and `objectState`.
    fn host_typdefs(&self) -> Vec<HostTyp>;

    /// Creates the initial state of an extern object instance.
    fn eval_extern_init<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Evaluates a compile-time known extern function call; shared by default.
    fn eval_extern_func_lctk_call<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Result<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        eval_func_lctk(ctx, values)
    }

    /// Evaluates an extern function call.
    fn eval_extern_func_call<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Result<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Evaluates a method call on an extern object.
    fn eval_extern_method_call<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Result<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Creates the architecture's initial state.
    fn init_arch_state<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
    ) -> Result<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;
}

// == Runner dispatch

/// Implements `Extern` for each architecture by routing names to its `Impl`.
///
/// A blanket impl over `Impl` would implement the core's trait
/// for every type, which only the core may do.
macro_rules! impl_extern {
    ($($arch:ty),+ $(,)?) => {
        $(
            impl Extern for $arch {
                fn host_typdefs(&self) -> Vec<HostTyp> {
                    Impl::host_typdefs(self)
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
                        "ExternFunctionCall_eval_lctk" => Self::eval_extern_func_lctk_call(ctx, values),
                        "ExternFunctionCall_eval" => Self::eval_extern_func_call(ctx, values),
                        "ExternMethodCall_eval" => Self::eval_extern_method_call(ctx, values),
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
                        "init_objectState" => Self::eval_extern_init(ctx, values),
                        "init_archState" => Self::init_arch_state(ctx),
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

impl_extern!(crate::dummy::Dummy, crate::ebpf::Ebpf, crate::psa::Psa, crate::v1model::V1Model);

// == Compile-time extern calls

/// Evaluates `static_assert`, the only compile-time known extern function.
pub(crate) fn eval_func_lctk<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    values: &[Value],
) -> Result<Vec<Value>, ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    // Arguments: context, function name, parameter names
    let [value_ctx, value_name, value_names_param] = values else {
        return Err(error::extern_argument_arity_mismatch(
            "unexpected number of arguments to local compile-time known extern function call"
                .to_owned(),
        )
        .into());
    };
    let name_func = p4spectec::lang::data::value::flat::get::text(ctx.arena(), value_name)?;
    let values_name_param =
        p4spectec::lang::data::value::flat::get::list(ctx.arena(), value_names_param)?;
    let names_param = values_name_param
        .iter()
        .map(|value| {
            p4spectec::lang::data::value::flat::get::text(ctx.arena(), value)
                .map_err(ExternError::from)
        })
        .collect::<Result<Vec<_>, _>>()?;
    // Both overloads of `static_assert`; nothing else is known
    let has_message = match (name_func, names_param.as_slice()) {
        ("static_assert", ["check", "message"]) => true,
        ("static_assert", ["check"]) => false,
        _ => {
            return Err(error::extern_function_unsupported(format!(
                "unsupported local compile-time known extern function call: {name_func}({})",
                names_param.join(", ")
            ))
            .into());
        }
    };
    let value = core::func::static_assert(ctx, value_ctx, has_message)?;
    Ok(vec![value])
}
