//! The P4 host lent to spec-meta while it runs one P4 call
//!
//! A P4 call reaches spec-meta in a context that has borrowed the host's
//! interface and extern, so spec-meta's host calls relay back to them here
//! rather than to the host the tower stores.
//! A host extern that reenters, such as V1Model calling find_var_e,
//! runs on the next depth, made on its first use and kept for the program.

use super::RelayTarget;
use crate::{
    interface::{meta::shared::Cache, p4::P4Interface},
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    metacirc::reentry::{Depth, MetaEnv, MetaInterp},
    runner::{Eval, Extern, Interface, InterpreterError, RunnerContext},
};

/// The P4 host's components, borrowed from the context of one P4 call.
pub(in crate::metacirc) struct LentHost<'h, 's, Ext: Extern> {
    pub env: &'h MetaEnv<'s>,
    pub interface: &'h mut P4Interface,
    pub external: &'h mut Ext,
    /// The depths beneath the calling one, which answer reentry.
    pub depth_next: &'h mut Option<Box<Depth>>,
    /// C for the current program, shared by every depth.
    pub ctx_target: &'h mut Option<Value>,
}

impl<Ext: Extern> LentHost<'_, '_, Ext> {
    /// Runs `call` in a P4 context whose interpreter is the next depth.
    fn with_next<T>(
        &mut self,
        arena: &mut Arena,
        cache: &mut Cache,
        call: impl FnOnce(
            &mut RunnerContext<'_, MetaInterp<'_>, P4Interface, Ext>,
        ) -> Result<T, InterpreterError>,
    ) -> Eval<T> {
        // The next depth lives until the program is reset
        let depth = self
            .depth_next
            .get_or_insert_with(|| Box::new(Depth::new(self.env.options)));
        let mut meta_next = depth.lend(self.ctx_target, cache);
        let mut effects = 0;
        let mut ctx_next = RunnerContext::new(
            arena,
            self.env,
            &mut meta_next,
            self.interface,
            self.external,
            &mut effects,
        );
        let checkpoint = ctx_next.effect_checkpoint();
        let result = call(&mut ctx_next);
        Eval::new(result, ctx_next.effect_since(checkpoint))
    }
}

impl<Ext: Extern> RelayTarget for LentHost<'_, '_, Ext> {
    fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        _cache: &mut Cache,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        self.interface
            .call_builtin(arena, id, targs, values)
            .map_err(Into::into)
    }

    fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        cache: &mut Cache,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        self.with_next(arena, cache, |ctx| ctx.call_extern_rel(name, values))
    }

    fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        cache: &mut Cache,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        self.with_next(arena, cache, |ctx| ctx.call_extern_func(name, targs, values))
    }

    // The tower resets the host this target borrows from
    fn reset(&mut self) {}
}
