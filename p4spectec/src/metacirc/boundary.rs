//! Host calls across the boundary below one level of a tower
//!
//! The base's specification interprets the levels above it,
//! so its host calls reach `Levels`, the levels lent for one execution,
//! and enter `Boundary { level: HostLevel::FIRST }`.
//! A boundary decodes one representation layer (`relay::codec`) for its level:
//! a meta level answers its builtins and sends its own host calls
//! across the boundary above it, up to the top.
//! The top answers P4 builtins and externs, or the program's builtins.
//! A P4 extern that reenters, such as V1Model calling find_var_e,
//! runs on the next depth, made on its first use and kept for the program;
//! the run path lends no definitions for it, so its reentry fails.

use super::{
    reentry::{MetaDepth, MetaEnv, MetaInterp, NoReentry},
    relay::codec,
    tower::{HostLevel, Top, Upper},
};
use crate::{
    interface::{
        meta::{MetaInterface, shared::Cache},
        p4::P4Interface,
    },
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{
        Eval, Extern, ExternError, Interface, Interpreter, InterpreterError, RunnerContext,
        SideEffect,
    },
};

// == Lent levels

/// The levels above the base, borrowed for one execution of the base's specification.
pub(in crate::metacirc) struct Levels<'l, Ext: Extern> {
    /// Definitions shared by every depth, which a reentry runs on.
    pub env: Option<&'l MetaEnv<'l>>,
    /// The codec cache of the boundary below each level, shared by every depth.
    pub caches: &'l mut [Cache],
    /// C of each level for the current program, shared by every depth.
    pub ctxs: &'l mut [Option<Value>],
    /// The meta builtins of each level between the base and the top, at this depth.
    pub hosts: &'l mut [MetaInterface],
    pub top: TopMut<'l, Ext>,
    /// The depths beneath this one, which answer reentry.
    pub depth_next: &'l mut Option<Box<MetaDepth>>,
}

/// The top of `Levels`, borrowed from `Top` or from the context of a P4 call.
pub(in crate::metacirc) enum TopMut<'l, Ext: Extern> {
    /// A P4 specification, answering the builtins and externs of its program.
    P4 { interface: &'l mut P4Interface, external: &'l mut Ext },
    /// The AL or IL program of the highest meta level, answering only its builtins.
    Program(&'l mut MetaInterface),
}

impl<Ext: Extern> Upper<Ext> {
    /// Lends the stored levels at depth 0; `env`, if any, answers reentry.
    pub(in crate::metacirc) fn lend<'l>(
        &'l mut self,
        env: Option<&'l MetaEnv<'l>>,
    ) -> Levels<'l, Ext> {
        let top = match &mut self.top {
            Top::P4(host) => {
                let (interface, external) = host.parts_mut();
                TopMut::P4 { interface, external }
            }
            Top::Program(interface) => TopMut::Program(interface),
        };
        Levels {
            env,
            caches: &mut self.caches,
            ctxs: &mut self.ctxs,
            hosts: &mut self.hosts,
            top,
            depth_next: &mut self.depth_next,
        }
    }
}

impl<'l, Ext: Extern> Levels<'l, Ext> {
    /// Returns the definitions of a P4 call, which always lends them.
    pub(in crate::metacirc) fn env(&self) -> &'l MetaEnv<'l> {
        self.env.expect("a P4 call lends its definitions")
    }

    /// Returns the codec cache of the boundary below `level`.
    pub(in crate::metacirc) fn cache(&mut self, level: HostLevel) -> &mut Cache {
        &mut self.caches[level.index()]
    }

    /// Returns C of `level`, if loaded for the current program.
    pub(in crate::metacirc) fn ctx(&self, level: HostLevel) -> Option<Value> {
        self.ctxs[level.index()]
    }

    /// Records C of `level` for the current program.
    pub(in crate::metacirc) fn set_ctx(&mut self, level: HostLevel, value_ctx: Value) {
        self.ctxs[level.index()] = Some(value_ctx);
    }

    /// Returns the level of the top, the highest level with a boundary.
    fn level_top(&self) -> HostLevel {
        HostLevel::new(self.caches.len())
    }

    /// Runs `call` in a P4 context whose interpreter is the next depth.
    fn with_next<T>(
        &mut self,
        arena: &mut Arena,
        call: impl FnOnce(
            &mut RunnerContext<'_, MetaInterp<'_>, P4Interface, Ext>,
        ) -> Result<T, InterpreterError>,
    ) -> Eval<T> {
        let env = self.env();
        let Self { caches, ctxs, hosts, top, depth_next, .. } = self;
        let TopMut::P4 { interface, external } = top else {
            unreachable!("only a P4 top reenters");
        };
        // The next depth lives until the program is reset
        let count = hosts.len();
        let depth = depth_next.get_or_insert_with(|| Box::new(MetaDepth::new(env.options, count)));
        let mut interp = depth.lend(caches, ctxs);
        let mut effects = 0;
        let mut ctx = RunnerContext::new(
            arena,
            env,
            &mut interp,
            &mut **interface,
            &mut **external,
            &mut effects,
        );
        eval_in(&mut ctx, call)
    }

    /// Runs `call` in a P4 context whose interpreter fails on reentry.
    fn without_next<T>(
        &mut self,
        arena: &mut Arena,
        call: impl FnOnce(
            &mut RunnerContext<'_, NoReentry, P4Interface, Ext>,
        ) -> Result<T, InterpreterError>,
    ) -> Eval<T> {
        let TopMut::P4 { interface, external } = &mut self.top else {
            unreachable!("only a P4 top answers externs");
        };
        let mut effects = 0;
        let mut interp = NoReentry;
        let mut ctx = RunnerContext::new(
            arena,
            &(),
            &mut interp,
            &mut **interface,
            &mut **external,
            &mut effects,
        );
        eval_in(&mut ctx, call)
    }
}

/// Runs `call` in `ctx`, reporting the effects it records.
fn eval_in<Interp, Ext, T>(
    ctx: &mut RunnerContext<'_, Interp, P4Interface, Ext>,
    call: impl FnOnce(&mut RunnerContext<'_, Interp, P4Interface, Ext>) -> Result<T, InterpreterError>,
) -> Eval<T>
where
    Interp: Interpreter<P4Interface, Ext>,
    Ext: Extern,
{
    let checkpoint = ctx.effect_checkpoint();
    let result = call(ctx);
    Eval::new(result, ctx.effect_since(checkpoint))
}

// == Boundary

/// The boundary below `level`, sending decoded host calls up to it.
pub(in crate::metacirc) struct Boundary<'b, 'l, Ext: Extern> {
    pub levels: &'b mut Levels<'l, Ext>,
    pub level: HostLevel,
}

/// Rejects an operation outside the host boundary's contract.
fn unsupported<T>(name: &str) -> Eval<T, ExternError> {
    Eval::new(
        Err(ExternError::diagnostic_message(format!("unsupported meta extern: {name}"))),
        SideEffect::Pure,
    )
}

impl<Ext: Extern> Boundary<'_, '_, Ext> {
    /// Returns the codec cache of this boundary.
    fn cache(&mut self) -> &mut Cache {
        self.levels.cache(self.level)
    }

    /// Returns whether this boundary is below the top.
    fn is_top(&self) -> bool {
        self.level == self.levels.level_top()
    }

    /// Decodes a host call from the level below and evaluates it at this level.
    pub(in crate::metacirc) fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError> {
        match name {
            // Builtins are answered by this level's interface
            "Call_builtin_func" => {
                let (id, targs, values) =
                    match codec::decode::func_args(arena, self.cache(), values) {
                        Ok(args) => args,
                        Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                    };
                let eval = self.eval_builtin(arena, &id, &targs, &values);
                codec::encode::func_result(arena, self.cache(), eval)
            }
            // Host functions are answered by this level's extern
            "Call_extern_func" => {
                let (id, targs, values) =
                    match codec::decode::func_args(arena, self.cache(), values) {
                        Ok(args) => args,
                        Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                    };
                let eval = self.eval_extern_func(arena, &id.node, &targs, &values);
                codec::encode::func_result(arena, self.cache(), eval)
            }
            // Host relations return an ordered list of outputs
            "Call_extern_rel" => {
                let (id, values) = match codec::decode::rel_args(arena, self.cache(), values) {
                    Ok(args) => args,
                    Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                };
                let eval = self.eval_extern_rel(arena, &id.node, &values);
                codec::encode::rel_result(arena, self.cache(), eval)
            }
            // Host functions are reached through the three declared meta relations
            _ => unsupported(name),
        }
    }

    /// Calls a decoded builtin at this level.
    fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        // The top answers P4 or program builtins; a meta level answers meta builtins
        let eval = if self.is_top() {
            match &mut self.levels.top {
                TopMut::P4 { interface, .. } => interface.call_builtin(arena, id, targs, values),
                TopMut::Program(interface) => interface.call_builtin(arena, id, targs, values),
            }
        } else {
            self.levels.hosts[self.level.index()].call_builtin(arena, id, targs, values)
        };
        eval.map_err(Into::into)
    }

    /// Calls a decoded host relation at this level.
    fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        // A meta level's host calls cross the boundary above it
        if !self.is_top() {
            let mut boundary = Boundary { levels: &mut *self.levels, level: self.level.above() };
            return boundary.eval_rel(arena, name, values).map_err(Into::into);
        }
        match (&self.levels.top, self.levels.env) {
            // A P4 extern may reenter, on the next depth
            (TopMut::P4 { .. }, Some(_)) => self
                .levels
                .with_next(arena, |ctx| ctx.call_extern_rel(name, values)),
            // Without definitions for a next depth, its reentry fails
            (TopMut::P4 { .. }, None) => self
                .levels
                .without_next(arena, |ctx| ctx.call_extern_rel(name, values)),
            // The program declares no host relations
            (TopMut::Program(_), _) => unsupported(name).map_err(Into::into),
        }
    }

    /// Calls a decoded host function at this level.
    fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        // A meta level declares no host functions of its own
        if !self.is_top() {
            return unsupported(name).map_err(Into::into);
        }
        match (&self.levels.top, self.levels.env) {
            // A P4 extern may reenter, on the next depth
            (TopMut::P4 { .. }, Some(_)) => self
                .levels
                .with_next(arena, |ctx| ctx.call_extern_func(name, targs, values)),
            // Without definitions for a next depth, its reentry fails
            (TopMut::P4 { .. }, None) => self
                .levels
                .without_next(arena, |ctx| ctx.call_extern_func(name, targs, values)),
            // The program declares no host functions
            (TopMut::Program(_), _) => unsupported(name).map_err(Into::into),
        }
    }
}

// == Externs of the base's specification

impl<Ext: Extern> Extern for Levels<'_, Ext> {
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
        let (arena, levels) = ctx.arena_and_external_mut();
        Boundary { levels, level: HostLevel::FIRST }.eval_rel(arena, name, values)
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

    // The call that lent these levels clears them through their owner
    fn clear(&mut self) {}
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
        // The base runner lends its stored levels, without definitions for reentry
        let (arena, upper) = ctx.arena_and_external_mut();
        let mut levels = upper.lend(None);
        Boundary { levels: &mut levels, level: HostLevel::FIRST }.eval_rel(arena, name, values)
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
