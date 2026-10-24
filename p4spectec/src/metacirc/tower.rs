//! Interpreter towers and their host-call layers
//!
//! The tower exposes program execution through its native entry point.
//! The base is a runner with an interpreter; every other level is a host,
//! since the level below interprets it.
//! A meta specification's extern hosts the script above it:
//! the next level's layer, or the builtins of the AL or IL program.
//! A single-level tower executes its top level directly as its base.

use super::{config::LevelConfig, externs::MetaExtern, reentry::NoReentry};
use crate::{
    interface::{meta::MetaInterface, p4::P4Interface},
    lang::{
        al::ast as al,
        data::{arena::Arena, value::flat::Value},
        il::ast::{self as il, Id, Typ},
    },
    runner::{Eval, Host, Runner},
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
    /// A level above one accepting IL, which receives the patched IL.
    AboveIl { config: LevelConfig, spec_il: il::Spec },
}

impl Level {
    pub(super) fn config(&self) -> &LevelConfig {
        match self {
            Self::Base { config, .. }
            | Self::AboveAl { config, .. }
            | Self::AboveIl { config, .. } => config,
        }
    }
}

/// The native runner of the base, selected by the language it interprets.
pub(super) enum BaseLayer {
    /// A P4 specification, interpreting a parsed P4 program.
    P4Spec(Runner<P4Interface, Dummy>),
    /// A meta specification interpreting an AL or IL script, hosting the script above it.
    MetaSpec(Runner<MetaInterface, MetaExtern>),
}

/// The host of a level above the base, which the level below interprets.
pub(super) enum HostLayer {
    /// A P4 specification, answering the builtins and externs of its program.
    P4Spec(Host<P4Interface, Dummy>),
    /// A meta specification, hosting the script above it.
    MetaSpec(Host<MetaInterface, MetaExtern>),
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

impl HostLayer {
    /// Calls a decoded builtin at this layer.
    pub fn eval_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        match self {
            Self::P4Spec(host) => host.eval_builtin(arena, id, targs, values),
            Self::MetaSpec(host) => host.eval_builtin(arena, id, targs, values),
        }
        .map_err(Into::into)
    }

    /// Calls a decoded host relation at this layer; `interp` answers its reentry.
    pub fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        interp: &mut NoReentry,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        match self {
            Self::P4Spec(host) => host.eval_extern_rel(arena, interp, name, values),
            Self::MetaSpec(host) => host.eval_extern_rel(arena, interp, name, values),
        }
    }

    /// Calls a decoded host function at this layer; `interp` answers its reentry.
    pub fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        interp: &mut NoReentry,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        match self {
            Self::P4Spec(host) => host.eval_extern_func(arena, interp, name, targs, values),
            Self::MetaSpec(host) => host.eval_extern_func(arena, interp, name, targs, values),
        }
    }

    /// Resets this layer and all layers owned by its extern implementation.
    pub fn reset(&mut self) {
        match self {
            Self::P4Spec(host) => host.reset(),
            Self::MetaSpec(host) => host.reset(),
        }
    }
}
