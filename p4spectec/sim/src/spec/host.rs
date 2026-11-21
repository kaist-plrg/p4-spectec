//! The runner as the simulator sees it
//!
//! `SpecHost` is the part of `RunnerContext` the simulator calls:
//! the execution arena and the specification's relations and functions.
//! `ArchHost` adds the architecture running as the extern.
//! Both are object safe, so simulator code takes `&mut dyn ArchHost<V1Model>`
//! instead of carrying the interpreter and interface as type parameters;
//! an `ArchHost` upcasts to a `SpecHost` where the architecture does not matter.
//! `with_host` lends a `Runner` out as an `ArchHost`,
//! whichever specification stage it loaded,
//! and `parse_program` parses a P4 program through it.

use std::path::PathBuf;

use p4spectec::lang::{data::value::flat::Value, il::ast::Typ};

/// The execution arena that holds every specification value of a run.
pub use p4spectec::lang::data::arena::Arena;

use p4spectec::runner::{
    Backend, Eval, Extern, Interface, InterfaceError, Interpreter, InterpreterError, Runner,
    RunnerContext, SideEffect,
};

use super::SpecValue;

// == Hosts

/// The execution arena and the specification's definitions.
pub trait SpecHost {
    /// Borrows the execution arena.
    fn arena(&self) -> &Arena;

    /// Borrows the execution arena mutably.
    fn arena_mut(&mut self) -> &mut Arena;

    /// Records a host or output effect independently of its result.
    fn record_effect(&mut self, effect: SideEffect);

    /// Runs the interpreter's program entry.
    fn call_program(&mut self, name: &str, program: Value) -> Eval<Vec<Value>, InterpreterError>;

    /// Calls a relation by name.
    fn call_rel(&mut self, name: &str, values: &[Value]) -> Eval<Vec<Value>, InterpreterError>;

    /// Calls a function by name.
    fn call_func(
        &mut self,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterpreterError>;
}

/// A `SpecHost` whose extern is the architecture `Arch`.
pub trait ArchHost<Arch>: SpecHost {
    /// Borrows the architecture.
    fn external(&self) -> &Arch;
}

// == Runner context

impl<Interp, Iface, Ext> SpecHost for RunnerContext<'_, Interp, Iface, Ext>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    fn arena(&self) -> &Arena {
        RunnerContext::arena(self)
    }

    fn arena_mut(&mut self) -> &mut Arena {
        RunnerContext::arena_mut(self)
    }

    fn record_effect(&mut self, effect: SideEffect) {
        RunnerContext::record_effect(self, effect)
    }

    fn call_program(&mut self, name: &str, program: Value) -> Eval<Vec<Value>, InterpreterError> {
        RunnerContext::call_program(self, name, program)
    }

    fn call_rel(&mut self, name: &str, values: &[Value]) -> Eval<Vec<Value>, InterpreterError> {
        RunnerContext::call_rel(self, name, values)
    }

    fn call_func(
        &mut self,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterpreterError> {
        RunnerContext::call_func(self, name, targs, values)
    }
}

impl<Interp, Iface, Ext> ArchHost<Ext> for RunnerContext<'_, Interp, Iface, Ext>
where
    Iface: Interface,
    Ext: Extern,
    Interp: Interpreter<Iface, Ext>,
{
    fn external(&self) -> &Ext {
        RunnerContext::external(self)
    }
}

// == Runners

/// Parses the P4 program `paths` through the runner's interface.
pub fn parse_program<Iface: Interface, Arch: Extern>(
    arena: &mut Arena,
    runner: &mut Runner<Iface, Arch>,
    includes: &[PathBuf],
    paths: &[PathBuf],
) -> Result<SpecValue, InterfaceError> {
    runner.parse_program(arena, includes, paths).map(SpecValue)
}

/// Runs `f` with `runner` as an `ArchHost`, whichever stage it loaded.
pub fn with_host<Iface: Interface, Arch: Extern, T>(
    arena: &mut Arena,
    runner: &mut Runner<Iface, Arch>,
    f: impl FnOnce(&mut dyn ArchHost<Arch>) -> T,
) -> T {
    let Runner { backend, interface, external, effects, hooks } = runner;
    match backend {
        Backend::Al { spec_env, interp } => {
            f(&mut RunnerContext::new(arena, spec_env, interp, interface, external, effects, hooks))
        }
        Backend::Sl { spec_env, interp } => {
            f(&mut RunnerContext::new(arena, spec_env, interp, interface, external, effects, hooks))
        }
        Backend::Pl { spec_env, interp } => {
            f(&mut RunnerContext::new(arena, spec_env, interp, interface, external, effects, hooks))
        }
    }
}
