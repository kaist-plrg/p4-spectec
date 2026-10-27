//! Host calls across the boundary below one level of a stored tower
//!
//! The base's specification interprets the levels above it,
//! so its host calls reach `Upper`, its extern,
//! and enter `Boundary { level: HostLevel::FIRST }`.
//! A boundary decodes the call (`relay::eval_rel`) for its level:
//! a meta level answers its builtins and sends its own host calls
//! across the boundary above it, up to the top.
//! The top answers P4 builtins and externs, or the program's builtins;
//! a P4 extern that reenters fails, since no interpreter is lent.

use super::{
    reentry::NoReentry,
    relay::{RelayTarget, eval_rel, unsupported},
    tower::{HostLevel, Top, Upper},
};
use crate::{
    interface::meta::shared::Cache,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Extern, ExternError, Interface, Interpreter, RunnerContext},
};

/// The boundary below `level`, sending decoded host calls up to it.
pub(in crate::metacirc) struct Boundary<'u, Ext: Extern> {
    pub upper: &'u mut Upper<Ext>,
    pub level: HostLevel,
}

impl<Ext: Extern> Boundary<'_, Ext> {
    /// Returns whether this boundary is below the top.
    fn is_top(&self) -> bool {
        self.level == self.upper.level_top()
    }
}

impl<Ext: Extern> RelayTarget for Boundary<'_, Ext> {
    fn cache(&mut self) -> &mut Cache {
        &mut self.upper.caches[self.level.index()]
    }

    fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        // The top answers P4 or program builtins; a meta level answers meta builtins
        let eval = if self.is_top() {
            match &mut self.upper.top {
                Top::P4(host) => host.eval_builtin(arena, id, targs, values),
                Top::Program(interface) => interface.call_builtin(arena, id, targs, values),
            }
        } else {
            self.upper.hosts[self.level.index()].call_builtin(arena, id, targs, values)
        };
        eval.map_err(Into::into)
    }

    fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        // A meta level's host calls cross the boundary above it
        if !self.is_top() {
            let mut boundary = Boundary { upper: &mut *self.upper, level: self.level.above() };
            return eval_rel(&mut boundary, arena, name, values).map_err(Into::into);
        }
        match &mut self.upper.top {
            // A P4 extern answers without reentry
            Top::P4(host) => host.eval_extern_rel(arena, &(), &mut NoReentry, name, values),
            // The program declares no host relations
            Top::Program(_) => unsupported(name).map_err(Into::into),
        }
    }

    fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        let is_top = self.is_top();
        match &mut self.upper.top {
            // A P4 extern answers without reentry
            Top::P4(host) if is_top => {
                host.eval_extern_func(arena, &(), &mut NoReentry, name, targs, values)
            }
            // A meta level and the program declare no host functions
            _ => unsupported(name).map_err(Into::into),
        }
    }
}

impl<Ext: Extern> Extern for Upper<Ext> {
    fn eval_rel<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        // The base's specification sends host calls to the first level above it
        let (arena, upper) = ctx.arena_and_external_mut();
        let mut boundary = Boundary { upper, level: HostLevel::FIRST };
        eval_rel(&mut boundary, arena, name, values)
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
        Upper::clear(self);
    }
}
