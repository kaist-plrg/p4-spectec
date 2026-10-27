//! Interpreter towers and their native base
//!
//! The tower exposes program execution through its native entry point.
//! The base is a runner with an interpreter; every other level is a host,
//! since the level below interprets it, and is reached through the base's
//! extern (see `relay`).
//! A single-level tower executes its top level directly as its base.
//! `build` assembles the levels; `patch` adds the entry the base calls.

mod build;
mod patch;

use super::{config::LevelConfig, reentry::NoReentry, relay::MetaExtern};
use crate::{
    interface::{meta::MetaInterface, p4::P4Interface},
    lang::{
        al::ast as al,
        data::{arena::Arena, value::flat::Value},
        il::ast::{self as il, Id, Typ},
    },
    runner::{Eval, Extern, Host, Runner},
    sim_plugin::dummy::Dummy,
};
use std::num::NonZeroUsize;

/// Prepared specifications and host layers of an interpreter tower,
/// with `Ext` the extern of its P4 level.
pub struct Tower<Ext: Extern = Dummy> {
    // Nonempty, ordered from base through intermediates to the top level
    pub(super) levels: Vec<Level>,
    pub(super) base: BaseLayer<Ext>,
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

/// A level above the base, which the level below it interprets.
///
/// Level k pairs with the boundary below it:
/// per-level storage keeps its C, that boundary's codec cache,
/// and its meta builtins at `index`, in order from level 1.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::metacirc) struct HostLevel(NonZeroUsize);

impl HostLevel {
    /// The first level above the base.
    pub(in crate::metacirc) const FIRST: Self = Self(NonZeroUsize::MIN);

    /// Returns the top level of a tower of `levels`.
    pub(in crate::metacirc) fn top(levels: &[Level]) -> Self {
        let number = NonZeroUsize::new(levels.len() - 1);
        Self(number.expect("a tower with a host level has two or more levels"))
    }

    /// Returns the level below this one.
    pub(in crate::metacirc) fn below(self) -> Floor {
        NonZeroUsize::new(self.0.get() - 1).map_or(Floor::Base, |number| Floor::Host(Self(number)))
    }

    /// Returns the host level above this one.
    pub(in crate::metacirc) fn above(self) -> Self {
        Self(self.0.checked_add(1).expect("a level number fits in usize"))
    }

    /// Returns this level's position among all levels, the base at 0.
    pub(in crate::metacirc) fn number(self) -> usize {
        self.0.get()
    }

    /// Returns this level's position in per-level storage, level 1 at 0.
    pub(in crate::metacirc) fn index(self) -> usize {
        self.0.get() - 1
    }
}

/// A level of the tower: the base, or a host level above it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::metacirc) enum Floor {
    /// The base, which runs natively.
    Base,
    /// A level above the base, which the level below interprets.
    Host(HostLevel),
}

/// The native runner of the base, selected by the language it interprets.
pub(super) enum BaseLayer<Ext: Extern> {
    /// A P4 specification, interpreting a parsed P4 program.
    P4Spec(Runner<P4Interface, Ext>),
    /// A meta specification interpreting an AL or IL script, hosting the script above it.
    MetaSpec(Runner<MetaInterface, MetaExtern<Ext>>),
}

impl<Ext: Extern> Tower<Ext> {
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

impl<Ext: Extern> BaseLayer<Ext> {
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

/// The host of a level above the base, which the level below interprets.
pub(in crate::metacirc) enum HostLayer<Ext: Extern> {
    /// A P4 specification, answering the builtins and externs of its program.
    P4Spec(Host<P4Interface, Ext>),
    /// A meta specification, hosting the script above it.
    MetaSpec(Host<MetaInterface, MetaExtern<Ext>>),
}

impl<Ext: Extern> HostLayer<Ext> {
    /// Calls a decoded builtin at this layer.
    pub(in crate::metacirc) fn eval_builtin(
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
    pub(in crate::metacirc) fn eval_extern_rel(
        &mut self,
        arena: &mut Arena,
        interp: &mut NoReentry,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>> {
        match self {
            Self::P4Spec(host) => host.eval_extern_rel(arena, &(), interp, name, values),
            Self::MetaSpec(host) => host.eval_extern_rel(arena, &(), interp, name, values),
        }
    }

    /// Calls a decoded host function at this layer; `interp` answers its reentry.
    pub(in crate::metacirc) fn eval_extern_func(
        &mut self,
        arena: &mut Arena,
        interp: &mut NoReentry,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value> {
        match self {
            Self::P4Spec(host) => host.eval_extern_func(arena, &(), interp, name, targs, values),
            Self::MetaSpec(host) => host.eval_extern_func(arena, &(), interp, name, targs, values),
        }
    }

    /// Resets this layer and all layers owned by its extern implementation.
    pub(in crate::metacirc) fn reset(&mut self) {
        match self {
            Self::P4Spec(host) => host.reset(),
            Self::MetaSpec(host) => host.reset(),
        }
    }
}
