//! Host calls relayed across one level boundary of a tower
//!
//! A level above the base is a host: the level below interprets it,
//! so it only answers the builtin and host calls relayed from below.
//! `eval_rel` decodes one representation layer (`codec`)
//! and delegates to the level above a boundary through a RelayTarget:
//! the levels a tower stores (`boundary`),
//! or the levels lent to spec-meta for one P4 call (`lent`).

pub(super) mod codec;
pub(super) mod lent;

use crate::{
    interface::meta::shared::Cache,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, ExternError, SideEffect},
};

/// A level boundary, sending decoded host calls to the level above it.
pub(in crate::metacirc) trait RelayTarget {
    /// Returns the codec cache of this boundary.
    fn cache(&mut self) -> &mut Cache;

    /// Calls a decoded builtin at this level.
    fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value>;

    /// Calls a decoded host relation at this level.
    fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>>;

    /// Calls a decoded host function at this level.
    fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value>;
}

/// Rejects an operation outside the host boundary's contract.
pub(super) fn unsupported<T>(name: &str) -> Eval<T, ExternError> {
    Eval::new(
        Err(ExternError::diagnostic_message(format!("unsupported meta extern: {name}"))),
        SideEffect::Pure,
    )
}

/// Decodes a host call from above a boundary and evaluates it at `target`.
pub(super) fn eval_rel<T: RelayTarget>(
    target: &mut T,
    arena: &mut Arena,
    name: &str,
    values: &[Value],
) -> Eval<Vec<Value>, ExternError> {
    match name {
        // Builtins are answered by the next level's interface
        "Call_builtin_func" => {
            let (id, targs, values) = match codec::decode::func_args(arena, target.cache(), values)
            {
                Ok(args) => args,
                Err(error) => return Eval::new(Err(error), SideEffect::Pure),
            };
            let eval = target.eval_builtin(arena, &id, &targs, &values);
            codec::encode::func_result(arena, target.cache(), eval)
        }
        // Host functions are answered by the next level's extern
        "Call_extern_func" => {
            let (id, targs, values) = match codec::decode::func_args(arena, target.cache(), values)
            {
                Ok(args) => args,
                Err(error) => return Eval::new(Err(error), SideEffect::Pure),
            };
            let eval = target.eval_extern_func(arena, &id.node, &targs, &values);
            codec::encode::func_result(arena, target.cache(), eval)
        }
        // Host relations return an ordered list of outputs
        "Call_extern_rel" => {
            let (id, values) = match codec::decode::rel_args(arena, target.cache(), values) {
                Ok(args) => args,
                Err(error) => return Eval::new(Err(error), SideEffect::Pure),
            };
            let eval = target.eval_extern_rel(arena, &id.node, &values);
            codec::encode::rel_result(arena, target.cache(), eval)
        }
        // Host functions are reached through the three declared meta relations
        _ => unsupported(name),
    }
}
