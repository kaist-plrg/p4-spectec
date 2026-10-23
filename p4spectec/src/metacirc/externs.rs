//! Native host operations requested by the AL interpreter specification
//!
//! AlExtern hosts the script that an AL specification interprets.
//! RelayExtern decodes one representation layer and delegates to the next level.
//! TerminalExtern handles only builtins of the AL program above the target.
//! Clearing an extern resets its owned runners before arena values are reset.

use super::{codec, tower::Layer};
use crate::{
    interface::al::{AlInterface, Cache},
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::{
        Eval, Extern, ExternError, Interface, Interpreter, NullExtern, Runner, RunnerContext,
        SideEffect,
    },
};

/// The host of the script that an AL specification interprets.
pub(super) enum AlExtern {
    /// The script is the next level; host calls go to its layer.
    Relay(RelayExtern),
    /// The script is the AL program; only its builtins are answered.
    Terminal(TerminalExtern),
}

/// Builtin calls resolved against the AL program's own specification.
pub(super) struct TerminalExtern {
    pub cache: Box<Cache>,
    pub runner_program: Box<Runner<AlInterface, NullExtern>>,
}

/// Host calls resolved against the next layer in a runtime tower.
pub(super) struct RelayExtern {
    pub cache: Box<Cache>,
    pub layer_next: Box<Layer>,
}

/// Rejects an operation outside the host boundary's contract.
fn unsupported<T>(name: &str) -> Eval<T, ExternError> {
    Eval::new(
        Err(ExternError::diagnostic_message(format!("unsupported meta extern: {name}"))),
        SideEffect::Pure,
    )
}

impl Extern for AlExtern {
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
        match external {
            Self::Relay(relay) => relay.eval_rel(arena, name, values),
            Self::Terminal(terminal) => terminal.eval_rel(arena, name, values),
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
        match self {
            Self::Relay(relay) => relay.clear(),
            Self::Terminal(terminal) => terminal.clear(),
        }
    }
}

impl TerminalExtern {
    /// Answers a builtin call of the AL program and rejects other meta relations.
    fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError> {
        // The AL program exposes only its builtins to the meta interpreter
        if name != "Call_builtin_func" {
            return unsupported(name);
        }
        let (id, targs, values) = match codec::decode::func_args(arena, &mut self.cache, values) {
            Ok(args) => args,
            Err(error) => return Eval::new(Err(error), SideEffect::Pure),
        };
        // Dispatch through the separately prepared program definition environment
        let eval = self
            .runner_program
            .eval_func(arena, &id.node, &targs, &values);
        codec::encode::func_result(arena, &mut self.cache, eval)
    }

    fn clear(&mut self) {
        self.cache.clear();
        self.runner_program.reset();
    }
}

impl RelayExtern {
    /// Decodes a host call of the next level and evaluates it at its layer.
    fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError> {
        match name {
            // Builtin and extern functions share a call shape and one result value
            "Call_builtin_func" | "Call_extern_func" => {
                let (id, targs, values) =
                    match codec::decode::func_args(arena, &mut self.cache, values) {
                        Ok(args) => args,
                        Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                    };
                let eval = self.layer_next.eval_func(arena, &id.node, &targs, &values);
                codec::encode::func_result(arena, &mut self.cache, eval)
            }
            // Relations return an ordered list of outputs
            "Call_extern_rel" => {
                let (id, values) = match codec::decode::rel_args(arena, &mut self.cache, values) {
                    Ok(args) => args,
                    Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                };
                let eval = self.layer_next.eval_rel(arena, &id.node, &values);
                codec::encode::rel_result(arena, &mut self.cache, eval)
            }
            // Host functions are reached through the three declared meta relations
            _ => unsupported(name),
        }
    }

    fn clear(&mut self) {
        self.cache.clear();
        self.layer_next.reset();
    }
}
