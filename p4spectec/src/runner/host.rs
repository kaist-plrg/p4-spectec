//! Host components of a level that no native interpreter runs
//!
//! A host pairs an interface with an extern and has no interpreter:
//! the level below interprets its specification and relays builtin and
//! host calls to it. An extern call runs in a context built around the
//! interpreter the caller lends, which answers if the extern reenters;
//! a caller that supports no reentry lends one that always fails.

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::il::ast::{Id, Typ};

use crate::runner::{Eval, Extern, Interface, InterfaceError, Interpreter, RunnerContext};

/// Interface and extern state of a level without an interpreter.
pub struct Host<Iface: Interface, Ext: Extern> {
    interface: Iface,
    external: Ext,
}

impl<Iface: Interface, Ext: Extern> Host<Iface, Ext> {
    pub fn new(interface: Iface, external: Ext) -> Self {
        Self { interface, external }
    }

    /// Calls a builtin through the interface.
    pub fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterfaceError> {
        self.interface.call_builtin(arena, id, targs, values)
    }

    /// Calls a host relation through the extern; `interp` answers its reentry.
    pub fn eval_extern_rel<Interp>(
        &mut self,
        arena: &mut Arena,
        interp: &mut Interp,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>>
    where
        Interp: Interpreter<Iface, Ext, SpecEnv = ()>,
    {
        let mut effects = 0;
        let mut ctx = RunnerContext::new(
            arena,
            &(),
            interp,
            &mut self.interface,
            &mut self.external,
            &mut effects,
        );
        let checkpoint = ctx.effect_checkpoint();
        let result = ctx.call_extern_rel(name, values);
        Eval::new(result, ctx.effect_since(checkpoint))
    }

    /// Calls a host function through the extern; `interp` answers its reentry.
    pub fn eval_extern_func<Interp>(
        &mut self,
        arena: &mut Arena,
        interp: &mut Interp,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value>
    where
        Interp: Interpreter<Iface, Ext, SpecEnv = ()>,
    {
        let mut effects = 0;
        let mut ctx = RunnerContext::new(
            arena,
            &(),
            interp,
            &mut self.interface,
            &mut self.external,
            &mut effects,
        );
        let checkpoint = ctx.effect_checkpoint();
        let result = ctx.call_extern_func(name, targs, values);
        Eval::new(result, ctx.effect_since(checkpoint))
    }

    /// Clears extern and interface state between programs.
    pub fn reset(&mut self) {
        self.external.clear();
        self.interface.clear();
    }
}
