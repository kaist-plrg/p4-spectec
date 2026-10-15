//! Interpreter towers and their host-call layers
//!
//! The tower exposes program execution through its native entry point.
//! Target and intermediate helpers dispatch host calls toward the input program.
//! A single-level tower executes its target directly as its base.

use super::{
    config::LevelConfig,
    externs::{RelayExtern, TerminalExtern},
};
use crate::{
    interface::{al::AlInterface, p4::P4Interface},
    lang::{
        al::ast as al,
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::{Eval, Runner},
    sim_plugin::dummy::Dummy,
};

/// Prepared specifications and host layers of an interpreter tower.
pub struct Tower {
    // Nonempty, ordered from base through intermediates to target
    pub(super) levels: Vec<Level>,
    pub(super) base: Layer,
}

/// A lowered specification paired with its configuration.
pub(super) struct Level {
    pub(super) config: LevelConfig,
    pub(super) spec_al: al::Spec,
}

/// A native base or a host-call destination beneath it.
pub(super) enum Layer {
    Target(Target),
    Meta(Runner<AlInterface, RelayExtern>),
}

/// The specification accepting the input program.
pub(super) enum Target {
    Al(Runner<AlInterface, TerminalExtern>),
    P4(Runner<P4Interface, Dummy>),
}

impl Tower {
    /// Clears program state in the base and every owned host layer.
    pub fn reset(&mut self) {
        self.base.reset();
    }

    /// Executes a patched input at the native base's entry relation.
    pub fn eval_program(
        &mut self,
        arena: &mut Arena,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>> {
        // A direct tower uses the caller's entry; nested towers use the base entry
        let name_base = if self.levels.len() == 1 { name } else { &self.levels[0].config.rel };
        self.base.eval_program(arena, name_base, program)
    }
}

impl Layer {
    /// Enters the native runner with the patched input value.
    fn eval_program(&mut self, arena: &mut Arena, name: &str, program: Value) -> Eval<Vec<Value>> {
        match self {
            Self::Target(Target::Al(runner)) => runner.eval_program(arena, name, program),
            Self::Target(Target::P4(runner)) => runner.eval_program(arena, name, program),
            Self::Meta(runner) => runner.eval_program(arena, name, program),
        }
    }

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
            Self::Meta(runner) => runner.eval_rel(arena, name, values),
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
            Self::Meta(runner) => runner.eval_func(arena, name, targs, values),
        }
    }

    /// Resets this layer and all layers owned by its extern implementation.
    pub fn reset(&mut self) {
        match self {
            Self::Target(Target::Al(runner)) => runner.reset(),
            Self::Target(Target::P4(runner)) => runner.reset(),
            Self::Meta(runner) => runner.reset(),
        }
    }
}
