//! Operation-local access to the components assembled by a runner
//!
//! The context splits the runner into independent borrows for one evaluation.
//! An extern receives the same context and can reenter the interpreter
//! after releasing any borrow of the host component it used.
//! Effects remain in the runner even when a caller only consumes the result.

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::{al, sl};

use crate::lang::il::ast::{Id, Typ};

use crate::runner::{Eval, Extern, Hooks, Interface, Interpreter, InterpreterError, SideEffect};

/// A snapshot of one runner's effect count, valid until that runner resets.
#[derive(Clone, Copy)]
pub struct Checkpoint(u64);

// == Runner context

/// Split borrows of a runner's components for one evaluation.
pub struct RunnerContext<'runner, Interp, Iface, Ext>
where
    Interp: Interpreter<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
{
    /// Caller-owned arena used for this evaluation.
    arena: &'runner mut Arena,
    /// Prepared definitions for the selected interpreter stage.
    spec_env: &'runner Interp::SpecEnv,
    /// Interpreter state, including configuration and caches.
    interp: &'runner mut Interp,
    /// Interface state used for builtin dispatch.
    interface: &'runner mut Iface,
    /// Host state used by extern relation and function calls.
    external: &'runner mut Ext,
    /// Runner effect count shared across nested calls.
    effects: &'runner mut u64,
    /// Observers shared across nested calls and host reentry.
    hooks: &'runner mut Hooks,
}

impl<'runner, Interp, Iface, Ext> RunnerContext<'runner, Interp, Iface, Ext>
where
    Interp: Interpreter<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
{
    pub(crate) fn new(
        arena: &'runner mut Arena,
        spec_env: &'runner Interp::SpecEnv,
        interp: &'runner mut Interp,
        interface: &'runner mut Iface,
        external: &'runner mut Ext,
        effects: &'runner mut u64,
        hooks: &'runner mut Hooks,
    ) -> Self {
        Self { arena, spec_env, interp, interface, external, effects, hooks }
    }

    // - Semantic components

    pub fn spec_env(&self) -> &'runner Interp::SpecEnv {
        self.spec_env
    }

    pub fn interp(&self) -> &Interp {
        self.interp
    }

    pub fn interp_mut(&mut self) -> &mut Interp {
        self.interp
    }

    pub fn arena(&self) -> &Arena {
        self.arena
    }

    pub fn arena_mut(&mut self) -> &mut Arena {
        self.arena
    }

    pub fn external(&self) -> &Ext {
        self.external
    }

    pub fn external_mut(&mut self) -> &mut Ext {
        self.external
    }

    /// Borrows the arena and extern state together for a nested runner call.
    pub fn arena_and_external_mut(&mut self) -> (&mut Arena, &mut Ext) {
        (self.arena, self.external)
    }

    /// Borrows the arena and interface state for builtin dispatch.
    pub fn arena_and_interface_mut(&mut self) -> (&mut Arena, &mut Iface) {
        (self.arena, self.interface)
    }

    // - Interpreter events

    /// Dispatches an AL premise with the arena used to prepare it.
    pub(crate) fn on_prem(&mut self, prem_al: &al::prepared::Prem) {
        self.hooks.on_prem(self.arena, prem_al);
    }

    /// Dispatches an SL instruction with the arena used to prepare it.
    pub(crate) fn on_instr(&mut self, instr_sl: &sl::prepared::Instr) {
        self.hooks.on_instr(self.arena, instr_sl);
    }

    // - Evaluation effects

    /// Records a host or output effect independently of its result.
    pub fn record_effect(&mut self, effect: SideEffect) {
        if effect == SideEffect::Impure {
            *self.effects = self.effects.saturating_add(1);
        }
    }

    /// Takes a snapshot without clearing effects of enclosing calls.
    pub fn effect_checkpoint(&self) -> Checkpoint {
        Checkpoint(*self.effects)
    }

    /// Reports effects since a checkpoint from this runner's current execution.
    ///
    /// A saturated counter conservatively reports Impure until reset.
    pub fn effect_since(&self, checkpoint: Checkpoint) -> SideEffect {
        if *self.effects == u64::MAX || *self.effects != checkpoint.0 {
            SideEffect::Impure
        } else {
            SideEffect::Pure
        }
    }

    // - Evaluation dispatch

    /// Runs the interpreter's program entry.
    ///
    /// With type guards disabled, the program must match the entry's input type.
    pub fn call_program(
        &mut self,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        let checkpoint = self.effect_checkpoint();
        let result = Interp::eval_program(self, name, program);
        Eval::new(result, self.effect_since(checkpoint))
    }

    /// Calls a relation by name through the interpreter.
    ///
    /// With type guards disabled, values must match the declared input types.
    pub fn call_rel(&mut self, name: &str, values: &[Value]) -> Eval<Vec<Value>, InterpreterError> {
        let checkpoint = self.effect_checkpoint();
        let result = Interp::eval_rel(self, name, values);
        Eval::new(result, self.effect_since(checkpoint))
    }

    /// Calls a function by name through the interpreter.
    ///
    /// With type guards disabled, values and type arguments must match
    /// the declared signature.
    pub fn call_func(
        &mut self,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterpreterError> {
        let checkpoint = self.effect_checkpoint();
        let result = Interp::eval_func(self, name, targs, values);
        Eval::new(result, self.effect_since(checkpoint))
    }

    // - Host dispatch

    /// Calls a builtin, recording its effect before exposing the result.
    pub fn call_builtin(
        &mut self,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        let eval = Iface::call_builtin(self, id, targs, values);
        self.record_effect(eval.effect);
        eval.result.map_err(Into::into)
    }

    /// Calls a host relation, recording effects in the context used for reentry.
    pub fn call_extern_rel(
        &mut self,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        let eval = Ext::eval_rel(self, name, values);
        self.record_effect(eval.effect);
        eval.result.map_err(Into::into)
    }

    /// Calls a host function, recording effects in the context used for reentry.
    pub fn call_extern_func(
        &mut self,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        let eval = Ext::eval_func(self, name, targs, values);
        self.record_effect(eval.effect);
        eval.result.map_err(Into::into)
    }
}
