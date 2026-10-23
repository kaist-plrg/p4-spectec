//! Interpreter towers and their host-call layers
//!
//! The tower exposes program execution through its native entry point.
//! Each level runs natively as one layer.
//! An AL specification's extern hosts the script above it:
//! the next level's layer, or the builtins of the AL program.
//! A single-level tower executes its top level directly as its base.

use super::{config::LevelConfig, externs::AlExtern};
use crate::{
    interface::{meta::al::AlInterface, p4::P4Interface},
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
    // Nonempty, ordered from base through intermediates to the top level
    pub(super) levels: Vec<Level>,
    pub(super) base: Layer,
}

/// The AL of a specification paired with its configuration.
pub(super) struct Level {
    pub(super) config: LevelConfig,
    pub(super) spec_al: al::Spec,
}

/// The native runner of one level, selected by the language it interprets.
pub(super) enum Layer {
    /// A P4 specification, interpreting a parsed P4 program.
    P4Spec(Runner<P4Interface, Dummy>),
    /// A specification interpreting AL, hosting the script above it.
    AlSpec(Runner<AlInterface, AlExtern>),
}

impl Tower {
    /// Clears program state in the base and every owned host layer.
    pub fn reset(&mut self) {
        self.base.reset();
    }

    /// Executes a patched input at the native base's entry relation.
    pub fn eval_program(&mut self, arena: &mut Arena, program: Value) -> Eval<Vec<Value>> {
        self.base
            .eval_program(arena, &self.levels[0].config.rel, program)
    }
}

impl Layer {
    /// Enters the native runner with the patched input value.
    fn eval_program(&mut self, arena: &mut Arena, name: &str, program: Value) -> Eval<Vec<Value>> {
        match self {
            Self::P4Spec(runner) => runner.eval_program(arena, name, program),
            Self::AlSpec(runner) => runner.eval_program(arena, name, program),
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
            Self::P4Spec(runner) => runner.eval_rel(arena, name, values),
            Self::AlSpec(runner) => runner.eval_rel(arena, name, values),
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
            Self::P4Spec(runner) => runner.eval_func(arena, name, targs, values),
            Self::AlSpec(runner) => runner.eval_func(arena, name, targs, values),
        }
    }

    /// Resets this layer and all layers owned by its extern implementation.
    pub fn reset(&mut self) {
        match self {
            Self::P4Spec(runner) => runner.reset(),
            Self::AlSpec(runner) => runner.reset(),
        }
    }
}
