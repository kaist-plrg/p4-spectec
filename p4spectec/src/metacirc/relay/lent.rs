//! The levels above the base, lent to spec-meta while it runs one P4 call
//!
//! A P4 call reaches spec-meta in a context that has borrowed the P4 host's
//! interface and extern, so spec-meta's host calls relay back to them here
//! rather than to the hosts the tower stores.
//! `LentBoundary` relays across the boundary below one `HostLevel`:
//! a meta level answers its builtins and relays its own host calls
//! across the boundary above it, up to the P4 top.
//! A P4 extern that reenters, such as V1Model calling find_var_e,
//! runs on the next depth, made on its first use and kept for the program.

use super::{RelayTarget, eval_rel, unsupported};
use crate::{
    interface::{
        meta::{MetaInterface, shared::Cache},
        p4::P4Interface,
    },
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    metacirc::{
        reentry::{MetaDepth, MetaEnv, MetaInterp},
        tower::HostLevel,
    },
    runner::{Eval, Extern, ExternError, Interface, Interpreter, InterpreterError, RunnerContext},
};

/// The levels above the base, borrowed from the context of one P4 call.
pub(in crate::metacirc) struct LentLevels<'h, 's, Ext: Extern> {
    pub env: &'h MetaEnv<'s>,
    /// The meta builtins of each level between the base and the top.
    pub interfaces: &'h mut [&'s mut MetaInterface],
    pub interface_top: &'h mut P4Interface,
    pub external_top: &'h mut Ext,
    /// The depths beneath the calling one, which answer reentry.
    pub depth_next: &'h mut Option<Box<MetaDepth>>,
    /// C of each level above the base, shared by every depth.
    pub ctx_targets: &'h mut [Option<Value>],
    /// The codec cache of each level boundary, shared by every depth.
    pub caches: &'h mut [Cache],
}

impl<Ext: Extern> LentLevels<'_, '_, Ext> {
    /// Returns the codec cache of the boundary below `level`.
    pub(in crate::metacirc) fn cache(&mut self, level: HostLevel) -> &mut Cache {
        &mut self.caches[level.index()]
    }

    /// Returns C of `level`, if loaded for the current program.
    pub(in crate::metacirc) fn ctx(&self, level: HostLevel) -> Option<Value> {
        self.ctx_targets[level.index()]
    }

    /// Records C of `level` for the current program.
    pub(in crate::metacirc) fn set_ctx(&mut self, level: HostLevel, value_ctx: Value) {
        self.ctx_targets[level.index()] = Some(value_ctx);
    }

    /// Returns the meta builtins of `level`, which is not the top.
    fn interface(&mut self, level: HostLevel) -> &mut MetaInterface {
        self.interfaces[level.index()]
    }

    fn is_top(&self, level: HostLevel) -> bool {
        level == HostLevel::top(self.env.levels)
    }

    /// Runs `call` in a P4 context whose interpreter is the next depth.
    fn with_next<T>(
        &mut self,
        arena: &mut Arena,
        call: impl FnOnce(
            &mut RunnerContext<'_, MetaInterp<'_>, P4Interface, Ext>,
        ) -> Result<T, InterpreterError>,
    ) -> Eval<T> {
        // The next depth lives until the program is reset
        let count = self.interfaces.len();
        let depth = self
            .depth_next
            .get_or_insert_with(|| Box::new(MetaDepth::new(self.env.options, count)));
        let mut meta_next = depth.lend(self.ctx_targets, self.caches);
        let mut effects = 0;
        let mut ctx_next = RunnerContext::new(
            arena,
            self.env,
            &mut meta_next,
            self.interface_top,
            self.external_top,
            &mut effects,
        );
        let checkpoint = ctx_next.effect_checkpoint();
        let result = call(&mut ctx_next);
        Eval::new(result, ctx_next.effect_since(checkpoint))
    }
}

impl<Ext: Extern> Extern for LentLevels<'_, '_, Ext> {
    fn eval_rel<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        // spec-meta at the base relays to the first level above it
        let (arena, levels) = ctx.arena_and_external_mut();
        let mut boundary = LentBoundary { levels, level: HostLevel::FIRST };
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

    // The P4 call that lent these levels clears them through its interpreter
    fn clear(&mut self) {}
}

/// The boundary below `level`, relaying host calls up to it.
struct LentBoundary<'l, 'h, 's, Ext: Extern> {
    levels: &'l mut LentLevels<'h, 's, Ext>,
    level: HostLevel,
}

impl<Ext: Extern> RelayTarget for LentBoundary<'_, '_, '_, Ext> {
    fn cache(&mut self) -> &mut Cache {
        self.levels.cache(self.level)
    }

    fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        // The top answers P4 builtins; a meta level answers meta builtins
        if self.levels.is_top(self.level) {
            self.levels
                .interface_top
                .call_builtin(arena, id, targs, values)
                .map_err(Into::into)
        } else {
            self.levels
                .interface(self.level)
                .call_builtin(arena, id, targs, values)
                .map_err(Into::into)
        }
    }

    fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        // A P4 extern may reenter, on the next depth
        if self.levels.is_top(self.level) {
            return self
                .levels
                .with_next(arena, |ctx| ctx.call_extern_rel(name, values));
        }
        // A meta level's host calls cross the boundary above it
        let mut boundary = LentBoundary { levels: &mut *self.levels, level: self.level.above() };
        eval_rel(&mut boundary, arena, name, values).map_err(Into::into)
    }

    fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        // A meta level declares no host functions of its own
        if !self.levels.is_top(self.level) {
            return unsupported(name).map_err(Into::into);
        }
        self.levels
            .with_next(arena, |ctx| ctx.call_extern_func(name, targs, values))
    }
}
