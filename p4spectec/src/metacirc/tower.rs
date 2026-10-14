//! Interpreter towers and their host-call layers
//!
//! The tower exposes program execution through its native entry point.
//! Target and intermediate helpers dispatch host calls toward the input program.
//! A single-level tower executes its target directly as its base.

use super::externs::{RelayExtern, TerminalExtern};
use crate::{
    interface::{al::AlInterface, p4::P4Interface},
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::{Eval, Runner},
    sim_plugin::dummy::Dummy,
};

/// The native execution entry point.
pub(super) enum Tower {
    Direct(Target),
    Nested(Runner<AlInterface, RelayExtern>),
}

/// A host-call destination beneath the base.
pub(super) enum Layer {
    Target(Target),
    Interm(Runner<AlInterface, RelayExtern>),
}

/// The specification accepting the input program.
pub(super) enum Target {
    Al(Runner<AlInterface, TerminalExtern>),
    P4(Runner<P4Interface, Dummy>),
}

impl Tower {
    /// Enters the tower with the prepared input value.
    pub fn eval_program(
        &mut self,
        arena: &mut Arena,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>> {
        match self {
            Self::Direct(Target::Al(runner)) => runner.eval_program(arena, name, program),
            Self::Direct(Target::P4(runner)) => runner.eval_program(arena, name, program),
            Self::Nested(runner) => runner.eval_program(arena, name, program),
        }
    }
}

impl Layer {
    /// Dispatches a decoded relation at this layer.
    pub fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        match self {
            Self::Target(Target::Al(runner)) => runner.eval_rel(arena, name, values),
            Self::Target(Target::P4(runner)) => runner.eval_rel(arena, name, values),
            Self::Interm(runner) => runner.eval_rel(arena, name, values),
        }
    }

    /// Dispatches a decoded function at this layer.
    pub fn eval_func(
        &mut self,
        arena: &mut Arena,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        match self {
            Self::Target(Target::Al(runner)) => runner.eval_func(arena, name, targs, values),
            Self::Target(Target::P4(runner)) => runner.eval_func(arena, name, targs, values),
            Self::Interm(runner) => runner.eval_func(arena, name, targs, values),
        }
    }

    /// Resets this layer and all layers owned by its extern implementation.
    pub fn reset(&mut self) {
        match self {
            Self::Target(Target::Al(runner)) => runner.reset(),
            Self::Target(Target::P4(runner)) => runner.reset(),
            Self::Interm(runner) => runner.reset(),
        }
    }
}
