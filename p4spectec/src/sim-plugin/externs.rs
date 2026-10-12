//! Extern dispatch shared by the architectures
//!
//! The specification calls externs through a few fixed relation
//! and function names;
//! `Impl` is what an architecture provides for them,
//! and the blanket `Extern` impl routes each name to it.
//! Compile-time known calls (`static_assert`) are handled here
//! for all architectures.

use crate::runner::{Eval, SideEffect};

use crate::lang::data::value::flat::Value;

use crate::lang::il::ast::Typ;

use crate::runner::{Extern, ExternError, Interface, Interpreter, RunnerContext};

use crate::sim_plugin::error;

use super::core;

// == Architecture extern operations

/// Architecture-specific extern behavior.
pub(crate) trait Impl: Extern {
    /// Creates the initial state of an extern object instance.
    fn eval_extern_init<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Evaluates a compile-time known extern function call; shared by default.
    fn eval_extern_func_lctk_call<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        eval_func_lctk(ctx, values)
    }

    /// Evaluates an extern function call.
    fn eval_extern_func_call<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Evaluates a method call on an extern object.
    fn eval_extern_method_call<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Creates the architecture's initial state.
    fn init_arch_state<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;
}

// == Runner dispatch

impl<Ext: Impl> Extern for Ext {
    fn eval_rel<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let mut eval = Eval::with(|effect| {
            // The three extern relations the specification defines
            let values = match name {
                "ExternFunctionCall_eval_lctk" => {
                    effect.include(self.eval_extern_func_lctk_call(ctx, values))?
                }
                "ExternFunctionCall_eval" => {
                    effect.include(self.eval_extern_func_call(ctx, values))?
                }
                "ExternMethodCall_eval" => {
                    effect.include(self.eval_extern_method_call(ctx, values))?
                }
                _ => {
                    return Err(error::extern_relation_unsupported(format!(
                        "unimplemented extern relation: {name}"
                    ))
                    .into());
                }
            };
            // Extern state lives in the values, so calls report no hidden effect
            Ok(values)
        });
        // Legacy host errors have no finer effect classification
        if eval.result.is_err() {
            eval.effect = SideEffect::Impure;
        }
        eval
    }

    fn eval_func<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let mut eval = Eval::with(|effect| {
            // The two extern functions the specification defines
            let value = match name {
                "init_objectState" => effect.include(self.eval_extern_init(ctx, values))?,
                "init_archState" => effect.include(self.init_arch_state(ctx))?,
                _ => {
                    return Err(error::extern_function_unsupported(format!(
                        "unimplemented extern function: {name}"
                    ))
                    .into());
                }
            };
            Ok(value)
        });
        // Legacy host errors have no finer effect classification
        if eval.result.is_err() {
            eval.effect = SideEffect::Impure;
        }
        eval
    }

    fn clear(&mut self) {}
}

// == Compile-time extern calls

/// Evaluates `static_assert`, the only compile-time known extern function.
pub(crate) fn eval_func_lctk<Interp, Iface, Ext>(
    ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    values: &[Value],
) -> Eval<Vec<Value>, ExternError>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    Eval::with(|effect| {
        // Arguments: context, function name, parameter names
        let [value_ctx, value_name, value_names_param] = values else {
            return Err(error::extern_argument_arity_mismatch(
                "unexpected number of arguments to local compile-time known extern function call"
                    .to_owned(),
            )
            .into());
        };
        let name_func = crate::lang::data::value::flat::get::text(ctx.arena(), value_name)?;
        let values_name_param =
            crate::lang::data::value::flat::get::list(ctx.arena(), value_names_param)?;
        let names_param = values_name_param
            .iter()
            .map(|value| {
                crate::lang::data::value::flat::get::text(ctx.arena(), value)
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
        let value = effect.include(core::func::static_assert(ctx, value_ctx, has_message))?;
        Ok(vec![value])
    })
}
