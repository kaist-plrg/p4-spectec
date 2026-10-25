//! Host calls relayed across one level boundary of a tower
//!
//! A level above the base is a host: the level below interprets it,
//! so it only answers the builtin and host calls relayed from below.
//! MetaExtern hosts the AL or IL script that a meta specification interprets.
//! RelayExtern decodes one representation layer (`codec`)
//! and delegates to the next level through a RelayTarget:
//! the host the relay stores (`stored`),
//! or the P4 host lent to spec-meta for one P4 call (`lent`).
//! TerminalExtern (`terminal`) handles only builtins of the AL or IL program
//! above the top level.
//! Clearing an extern resets the host it owns before arena values are reset.

pub(super) mod codec;
pub(super) mod lent;
mod stored;
mod terminal;

pub(super) use terminal::TerminalExtern;

use super::tower::HostLayer;
use crate::{
    interface::meta::shared::Cache,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Extern, ExternError, Interface, Interpreter, RunnerContext, SideEffect},
};
use std::borrow::BorrowMut;

/// The host of the AL or IL script that a meta specification interprets.
pub(in crate::metacirc) enum MetaExtern<Ext: Extern> {
    /// The script is the next level; host calls go to its layer.
    Relay(RelayExtern<Box<Cache>, Box<HostLayer<Ext>>>),
    /// The script is the AL or IL program; only its builtins are answered.
    Terminal(TerminalExtern),
}

/// Host calls resolved against the next level's host in a runtime tower.
///
/// The relay owns its codec cache and target when the tower stores it.
pub(in crate::metacirc) struct RelayExtern<C: BorrowMut<Cache>, T: RelayTarget> {
    pub cache: C,
    pub layer_next: T,
}

/// The level a relay sends decoded host calls to.
///
/// Each call receives the relay's codec cache,
/// which a target that runs another level shares.
pub(in crate::metacirc) trait RelayTarget {
    /// Calls a decoded builtin at this level.
    fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        cache: &mut Cache,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value>;

    /// Calls a decoded host relation at this level.
    fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        cache: &mut Cache,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>>;

    /// Calls a decoded host function at this level.
    fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        cache: &mut Cache,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value>;

    /// Resets the state this target owns.
    fn reset(&mut self);
}

/// Rejects an operation outside the host boundary's contract.
fn unsupported<T>(name: &str) -> Eval<T, ExternError> {
    Eval::new(
        Err(ExternError::diagnostic_message(format!("unsupported meta extern: {name}"))),
        SideEffect::Pure,
    )
}

impl<Ext: Extern> Extern for MetaExtern<Ext> {
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

impl<C: BorrowMut<Cache>, T: RelayTarget> Extern for RelayExtern<C, T> {
    fn eval_rel<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let (arena, relay) = ctx.arena_and_external_mut();
        relay.eval_rel(arena, name, values)
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
        RelayExtern::clear(self)
    }
}

impl<C: BorrowMut<Cache>, T: RelayTarget> RelayExtern<C, T> {
    /// Decodes a host call of the next level and evaluates it at its target.
    fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError> {
        let cache = self.cache.borrow_mut();
        match name {
            // Builtins are answered by the next level's interface
            "Call_builtin_func" => {
                let (id, targs, values) = match codec::decode::func_args(arena, cache, values) {
                    Ok(args) => args,
                    Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                };
                let eval = self
                    .layer_next
                    .eval_builtin(arena, cache, &id, &targs, &values);
                codec::encode::func_result(arena, cache, eval)
            }
            // Host functions are answered by the next level's extern
            "Call_extern_func" => {
                let (id, targs, values) = match codec::decode::func_args(arena, cache, values) {
                    Ok(args) => args,
                    Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                };
                let eval = self
                    .layer_next
                    .eval_extern_func(arena, cache, &id.node, &targs, &values);
                codec::encode::func_result(arena, cache, eval)
            }
            // Host relations return an ordered list of outputs
            "Call_extern_rel" => {
                let (id, values) = match codec::decode::rel_args(arena, cache, values) {
                    Ok(args) => args,
                    Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                };
                let eval = self
                    .layer_next
                    .eval_extern_rel(arena, cache, &id.node, &values);
                codec::encode::rel_result(arena, cache, eval)
            }
            // Host functions are reached through the three declared meta relations
            _ => unsupported(name),
        }
    }

    fn clear(&mut self) {
        self.cache.borrow_mut().clear();
        self.layer_next.reset();
    }
}
