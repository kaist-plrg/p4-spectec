//! Host calls across the boundary below one level of a tower
//!
//! The base's host calls reach `UpperMut`, the levels lent for one execution,
//! and enter `Boundary { level: HostLevel::FIRST }`,
//! which decodes them with the protocol of the level below it.
//! A meta level answers its builtins and sends its own host calls up;
//! the top answers P4 builtins and externs, or the program's builtins.
//! A P4 extern that reenters, as static_assert does to read its arguments,
//! runs on the next depth, made on its first use and kept for the program.

use super::{
    error,
    interp::{Depth, TowerInterp},
    protocol::{HostCall, Output, Protocol},
    tower::{HostLevel, TopMut, Upper, UpperMut},
};
use crate::{
    interface::p4::P4Interface,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{
        Eval, Extern, ExternError, Interface, Interpreter, InterpreterError, RunnerContext,
        SideEffect,
    },
};

// == Externs of the base's specification

impl<Ext: Extern> Extern for UpperMut<'_, Ext> {
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
        Boundary { upper, level: HostLevel::FIRST }.eval_rel(arena, name, values)
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

/// The base runner never executes with the stored levels as its extern:
/// `Tower::eval_program` and `SimTower` lend them as `UpperMut`,
/// with the spec-meta definitions a reentry runs on.
/// The runner only clears them between programs.
impl<Ext: Extern> Extern for Upper<Ext> {
    fn eval_rel<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        not_lent(name)
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
        not_lent(name)
    }

    fn clear(&mut self) {
        Upper::clear(self);
    }
}

/// Rejects a host call that reached the stored levels without lending them.
fn not_lent<T>(name: &str) -> Eval<T, ExternError> {
    Eval::new(Err(error::base_not_lent(name).into()), SideEffect::Pure)
}

// == Boundary

/// The boundary below `level`, sending decoded host calls up to it.
pub(in crate::metacirc) struct Boundary<'b, 'l, Ext: Extern> {
    pub upper: &'b mut UpperMut<'l, Ext>,
    pub level: HostLevel,
}

impl<Ext: Extern> Boundary<'_, '_, Ext> {
    /// Returns whether this boundary is below the top.
    fn is_top(&self) -> bool {
        self.level == self.upper.level_top()
    }

    /// Decodes a host call from the level below and evaluates it at this level.
    pub(in crate::metacirc) fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError> {
        let env = self.upper.env;
        let protocol = env.protocol(self.level);
        // Decode with the protocol of the level below
        let call = match protocol.host(arena, self.upper.cache(self.level), name, values) {
            Ok(call) => call,
            Err(error) => return Eval::new(Err(error), SideEffect::Pure),
        };
        let eval = match &call {
            // Builtins are answered by this level's interface
            HostCall::Builtin { id, targs, values } => {
                into_output(self.builtin(arena, id, targs, values), Output::Func)
            }
            // Host relations return an ordered list of outputs
            HostCall::ExternRel { id, values } => {
                into_output(self.extern_rel(arena, &id.node, values), Output::Rel)
            }
            // Host functions are answered by this level's extern
            HostCall::ExternFunc { id, targs, values } => {
                into_output(self.extern_func(arena, &id.node, targs, values), Output::Func)
            }
        };
        // Encode the outcome for the level below
        protocol.reply(arena, self.upper.cache(self.level), &call, eval)
    }
}

/// Wraps the result of an evaluation as an `Output`.
fn into_output<T>(eval: Eval<T>, output: impl FnOnce(T) -> Output) -> Eval<Output> {
    Eval::new(eval.result.map(output), eval.effect)
}

/// The levels above a boundary, answering the host calls decoded below it.
pub(in crate::metacirc) trait HostCalls {
    /// Calls a decoded builtin at this level.
    fn builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value>;

    /// Calls a decoded host relation at this level.
    fn extern_rel(&mut self, arena: &mut Arena, name: &str, values: &[Value]) -> Eval<Vec<Value>>;

    /// Calls a decoded host function at this level.
    fn extern_func(
        &mut self,
        arena: &mut Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value>;
}

impl<Ext: Extern> HostCalls for Boundary<'_, '_, Ext> {
    fn builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        // The top answers P4 or program builtins; a meta level answers meta builtins
        let eval = if self.is_top() {
            match &mut self.upper.top {
                TopMut::P4 { interface, .. } => interface.call_builtin(arena, id, targs, values),
                TopMut::Program(interface) => interface.call_builtin(arena, id, targs, values),
            }
        } else {
            self.upper.hosts[self.level.index()].call_builtin(arena, id, targs, values)
        };
        eval.map_err(Into::into)
    }

    fn extern_rel(&mut self, arena: &mut Arena, name: &str, values: &[Value]) -> Eval<Vec<Value>> {
        // A meta level's host calls cross the boundary above it
        if !self.is_top() {
            let mut boundary = Boundary { upper: &mut *self.upper, level: self.level.above() };
            return boundary.eval_rel(arena, name, values).map_err(Into::into);
        }
        match self.upper.top {
            // A P4 extern may reenter, on the next depth
            TopMut::P4 { .. } => self
                .upper
                .with_next(arena, |ctx| ctx.call_extern_rel(name, values)),
            // The program declares no host relations
            TopMut::Program(_) => unsupported(name).map_err(Into::into),
        }
    }

    fn extern_func(
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
        match self.upper.top {
            // A P4 extern may reenter, on the next depth
            TopMut::P4 { .. } => self
                .upper
                .with_next(arena, |ctx| ctx.call_extern_func(name, targs, values)),
            // The program declares no host functions
            TopMut::Program(_) => unsupported(name).map_err(Into::into),
        }
    }
}

/// Rejects an operation outside the host boundary's contract.
fn unsupported<T>(name: &str) -> Eval<T, ExternError> {
    Eval::new(Err(error::extern_unsupported(name).into()), SideEffect::Pure)
}

// == Reentry

impl<'l, Ext: Extern> UpperMut<'l, Ext> {
    /// Runs `call` in a P4 context whose interpreter is the next depth.
    fn with_next<T>(
        &mut self,
        arena: &mut Arena,
        call: impl FnOnce(
            &mut RunnerContext<'_, TowerInterp<'_>, P4Interface, Ext>,
        ) -> Result<T, InterpreterError>,
    ) -> Eval<T> {
        // Only the P4 top's interface and extern join the next depth's context
        let Self { env, caches, ctxs, hosts, top, depth_next } = self;
        let env = *env;
        let TopMut::P4 { interface, external } = top else {
            unreachable!("only a P4 top reenters");
        };
        // The next depth lives until the program is reset
        let count = hosts.len();
        let depth = depth_next
            .get_or_insert_with(|| Box::new(Depth::new(env.options, env.spec_base, count)));
        let mut interp = depth.lend(env.spec_base, caches, ctxs);
        // Count the call's effects apart from the calling depth's
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
