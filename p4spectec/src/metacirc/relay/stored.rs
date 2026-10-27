//! The host a tower stores as the next level of a relay
//!
//! The relay owns its codec cache and the next level's host
//! and answers through it, lending no interpreter:
//! a stored host extern that reenters fails.

use super::super::reentry::NoReentry;
use super::{RelayExtern, RelayTarget};
use crate::{
    interface::meta::shared::Cache,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Extern},
};

impl<Ext: Extern> RelayTarget for RelayExtern<Ext> {
    fn cache(&mut self) -> &mut Cache {
        &mut self.cache
    }

    fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        self.layer_next.eval_builtin(arena, id, targs, values)
    }

    fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        self.layer_next
            .eval_extern_rel(arena, &mut NoReentry, name, values)
    }

    fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        self.layer_next
            .eval_extern_func(arena, &mut NoReentry, name, targs, values)
    }
}
