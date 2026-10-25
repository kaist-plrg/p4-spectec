//! Native host operations requested by the AL interpreter specification
//!
//! MetaExtern hosts the AL script that a meta specification interprets.
//! RelayExtern decodes one representation layer and delegates to the next level.
//! TerminalExtern handles only builtins of the AL program above the top level.
//! Clearing an extern resets the host it owns before arena values are reset.

use super::{HostLayer, codec, reentry::NoReentry};
use crate::{
    interface::meta::{MetaInterface, shared::Cache},
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::{Eval, Extern, ExternError, Interface, Interpreter, RunnerContext, SideEffect},
};

/// The host of the AL script that a meta specification interprets.
pub(in crate::metacirc) enum MetaExtern {
    /// The script is the next level; host calls go to its layer.
    Relay(RelayExtern),
    /// The script is the AL program; only its builtins are answered.
    Terminal(TerminalExtern),
}

/// Builtin calls of the program, answered by the program's interface.
pub(in crate::metacirc) struct TerminalExtern {
    pub cache: Box<Cache>,
    pub interface_program: Box<MetaInterface>,
}

/// Host calls resolved against the next level's host in a runtime tower.
pub(in crate::metacirc) struct RelayExtern {
    pub cache: Box<Cache>,
    pub layer_next: Box<HostLayer>,
}

/// Rejects an operation outside the host boundary's contract.
fn unsupported<T>(name: &str) -> Eval<T, ExternError> {
    Eval::new(
        Err(ExternError::diagnostic_message(format!("unsupported meta extern: {name}"))),
        SideEffect::Pure,
    )
}

impl Extern for MetaExtern {
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
    /// Answers a builtin call of the program and rejects other meta relations.
    fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError> {
        // The program exposes only its builtins to the meta interpreter
        if name != "Call_builtin_func" {
            return unsupported(name);
        }
        let (id, targs, values) = match codec::decode::func_args(arena, &mut self.cache, values) {
            Ok(args) => args,
            Err(error) => return Eval::new(Err(error), SideEffect::Pure),
        };
        // Answer the builtin through the program's interface
        let eval = self
            .interface_program
            .call_builtin(arena, &id, &targs, &values)
            .map_err(Into::into);
        codec::encode::func_result(arena, &mut self.cache, eval)
    }

    fn clear(&mut self) {
        self.cache.clear();
        self.interface_program.clear();
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
            // Builtins are answered by the next level's interface
            "Call_builtin_func" => {
                let (id, targs, values) =
                    match codec::decode::func_args(arena, &mut self.cache, values) {
                        Ok(args) => args,
                        Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                    };
                let eval = self.layer_next.eval_builtin(arena, &id, &targs, &values);
                codec::encode::func_result(arena, &mut self.cache, eval)
            }
            // Host functions are answered by the next level's extern
            "Call_extern_func" => {
                let (id, targs, values) =
                    match codec::decode::func_args(arena, &mut self.cache, values) {
                        Ok(args) => args,
                        Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                    };
                let eval = self.layer_next.eval_extern_func(
                    arena,
                    &mut NoReentry,
                    &id.node,
                    &targs,
                    &values,
                );
                codec::encode::func_result(arena, &mut self.cache, eval)
            }
            // Host relations return an ordered list of outputs
            "Call_extern_rel" => {
                let (id, values) = match codec::decode::rel_args(arena, &mut self.cache, values) {
                    Ok(args) => args,
                    Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                };
                let eval =
                    self.layer_next
                        .eval_extern_rel(arena, &mut NoReentry, &id.node, &values);
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
