//! Native host operations requested by the AL interpreter specification
//!
//! BuiltinExtern handles only builtins of a standalone target script.
//! TowerExtern decodes one representation layer and delegates to the next runner.
//! Clearing either extern resets its owned runners before arena values are reset.

use super::{codec, tower::Tower};
use crate::{
    interface::al::AlInterface,
    lang::{data::value::flat::Value, il::ast::Typ},
    runner::{
        Eval, Extern, ExternError, Interface, Interpreter, NullExtern, Runner, RunnerContext,
        SideEffect,
    },
};

/// Builtin calls resolved against the standalone target's own specification.
pub(super) struct BuiltinExtern {
    pub runner_target: Runner<AlInterface, NullExtern>,
}

/// Host calls resolved against the next layer in a runtime tower.
pub(super) struct TowerExtern {
    pub runner_next: Box<Tower>,
}

/// Rejects an operation outside the host boundary's contract.
fn unsupported<T>(name: &str) -> Eval<T, ExternError> {
    Eval::new(
        Err(ExternError::diagnostic_message(format!("unsupported meta extern: {name}"))),
        SideEffect::Pure,
    )
}

impl Extern for BuiltinExtern {
    fn eval_rel<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        // Standalone scripts expose only their builtins to the meta interpreter
        if name != "Call_builtin_func" {
            return unsupported(name);
        }
        let (arena, external) = ctx.arena_and_external_mut();
        let (id, targs, values) = match codec::func_args(arena, values) {
            Ok(args) => args,
            Err(error) => return Eval::new(Err(error), SideEffect::Pure),
        };
        // Dispatch through the separately prepared target definition environment
        let eval = external
            .runner_target
            .eval_func(arena, &id.node, &targs, &values);
        codec::func_result(arena, eval)
    }

    fn eval_func<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        unsupported(name)
    }

    fn clear(&mut self) {
        self.runner_target.reset();
    }
}

impl Extern for TowerExtern {
    fn eval_rel<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let (arena, external) = ctx.arena_and_external_mut();
        match name {
            // Builtin and extern functions share a call shape and one result value
            "Call_builtin_func" | "Call_extern_func" => {
                let (id, targs, values) = match codec::func_args(arena, values) {
                    Ok(args) => args,
                    Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                };
                let eval = external
                    .runner_next
                    .eval_func(arena, &id.node, &targs, &values);
                codec::func_result(arena, eval)
            }
            // Relations return an ordered list of outputs
            "Call_extern_rel" => {
                let (id, values) = match codec::rel_args(arena, values) {
                    Ok(args) => args,
                    Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                };
                let eval = external.runner_next.eval_rel(arena, &id.node, &values);
                codec::rel_result(arena, eval)
            }
            // Host functions are reached through the three declared meta relations
            _ => unsupported(name),
        }
    }

    fn eval_func<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        unsupported(name)
    }

    fn clear(&mut self) {
        self.runner_next.reset();
    }
}
