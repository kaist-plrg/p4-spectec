//! The host a tower stores as the next level of a relay
//!
//! The relay owns the next level's host and answers through it,
//! lending no interpreter: a stored host extern that reenters fails.

use super::super::reentry::NoReentry;
use super::{HostLayer, RelayTarget};
use crate::{
    interface::meta::shared::Cache,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Extern},
};

impl<Ext: Extern> RelayTarget for Box<HostLayer<Ext>> {
    fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        _cache: &mut Cache,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        HostLayer::eval_builtin(self, arena, id, targs, values)
    }

    fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        _cache: &mut Cache,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        HostLayer::eval_extern_rel(self, arena, &mut NoReentry, name, values)
    }

    fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        _cache: &mut Cache,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        HostLayer::eval_extern_func(self, arena, &mut NoReentry, name, targs, values)
    }

    fn reset(&mut self) {
        HostLayer::reset(self)
    }
}
