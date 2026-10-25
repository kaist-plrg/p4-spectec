//! Interpreter towers and their native base
//!
//! The tower exposes program execution through its native entry point.
//! The base is a runner with an interpreter; every other level is a host,
//! since the level below interprets it, and is reached through the base's
//! extern (see `host`).
//! A single-level tower executes its top level directly as its base.

use super::{config::LevelConfig, host::MetaExtern};
use crate::{
    interface::{meta::MetaInterface, p4::P4Interface},
    lang::{
        al::ast as al,
        data::{arena::Arena, value::flat::Value},
    },
    runner::{Eval, Runner},
    sim_plugin::dummy::Dummy,
};

/// Prepared specifications and host layers of an interpreter tower.
pub struct Tower {
    // Nonempty, ordered from base through intermediates to the top level
    pub(super) levels: Vec<Level>,
    pub(super) base: BaseLayer,
}

/// A configured specification, by the form it is passed to the level below.
pub(super) enum Level {
    /// The base, which no level interprets.
    Base { config: LevelConfig },
    /// A level above one accepting AL, which receives the patched AL.
    AboveAl { config: LevelConfig, spec_al: al::Spec },
}

impl Level {
    pub(super) fn config(&self) -> &LevelConfig {
        match self {
            Self::Base { config, .. } | Self::AboveAl { config, .. } => config,
        }
    }
}

/// The native runner of the base, selected by the language it interprets.
pub(super) enum BaseLayer {
    /// A P4 specification, interpreting a parsed P4 program.
    P4Spec(Runner<P4Interface, Dummy>),
    /// A meta specification interpreting an AL script, hosting the script above it.
    MetaSpec(Runner<MetaInterface, MetaExtern>),
}

impl Tower {
    /// Clears program state in the base and every owned host layer.
    ///
    /// Call before each input, and before resetting the arena values for parsing.
    pub fn reset(&mut self) {
        self.base.reset();
    }

    /// Executes a patched input at the native base's entry relation.
    pub fn eval_program(&mut self, arena: &mut Arena, program: Value) -> Eval<Vec<Value>> {
        self.base
            .eval_program(arena, &self.levels[0].config().rel, program)
    }
}

impl BaseLayer {
    /// Enters the native runner with the patched input value.
    fn eval_program(&mut self, arena: &mut Arena, name: &str, program: Value) -> Eval<Vec<Value>> {
        match self {
            Self::P4Spec(runner) => runner.eval_program(arena, name, program),
            Self::MetaSpec(runner) => runner.eval_program(arena, name, program),
        }
    }

    /// Resets the runner and all layers owned by its extern implementation.
    fn reset(&mut self) {
        match self {
            Self::P4Spec(runner) => runner.reset(),
            Self::MetaSpec(runner) => runner.reset(),
        }
    }
}
