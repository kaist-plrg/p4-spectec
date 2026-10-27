//! Interpreter towers and their native base
//!
//! The tower exposes program execution through its native entry point.
//! The base is a runner with an interpreter; every other level is a host,
//! since the level below interprets it, and `Upper` stores them all
//! in its extern slot, lent per call as `UpperMut` (see `upper`).
//! A single-level tower executes its top level directly as its base.
//! `build` assembles the levels; `input` encodes the inputs of the base's entry;
//! `Tower::eval_input` runs the entry on the base's `Engine`.

mod build;
mod input;
mod upper;

pub(in crate::metacirc) use upper::{Top, TopMut, Upper, UpperMut};

use super::{
    config::LevelConfig,
    engine::{Engine, EngineKind, SpecTecEngine},
    interp::TowerEnv,
    protocol::ProtocolKind,
};
use crate::{
    interface::{meta::MetaInterface, p4::P4Interface},
    lang::{
        al::ast as al,
        data::{arena::Arena, value::flat::Value},
        il::ast as il,
    },
    runner::{self, Extern, InterpreterError, Runner},
    sim_plugin::dummy::Dummy,
};
use std::num::NonZeroUsize;

// == Tower

/// Prepared specifications and host layers of an interpreter tower,
/// with `Ext` the extern of its P4 level.
pub struct Tower<Ext: Extern = Dummy> {
    // Nonempty, ordered from base through intermediates to the top level
    pub(super) levels: Vec<PreparedLevel>,
    /// The protocol of the boundary below each level above the base.
    pub(super) protocols: Vec<ProtocolKind>,
    pub(super) base: BaseRunner<Ext>,
    /// Options for the interpreters of reentry depths.
    pub(super) options: runner::Config,
}

/// A configured level, prepared in the form the level below loads.
pub(super) enum PreparedLevel {
    /// The base, which no level interprets.
    Base { config: LevelConfig },
    /// A level passed to the level below as AL.
    Al { config: LevelConfig, spec_al: al::Spec },
    /// A level passed to the level below as IL.
    Il { config: LevelConfig, spec_il: il::Spec },
}

/// The native runner of the base, selected by the language it interprets.
pub(super) enum BaseRunner<Ext: Extern> {
    /// A P4 specification, interpreting a parsed P4 program.
    P4(Runner<P4Interface, Ext>),
    /// A meta specification interpreting an AL or IL script, hosting the levels above it.
    Meta(Runner<MetaInterface, Upper<Ext>>),
}

impl<Ext: Extern> Tower<Ext> {
    /// Clears program state in the base and every level above it.
    ///
    /// Call before each input, and before resetting the arena values for parsing.
    pub fn reset(&mut self) {
        self.base.reset();
    }

    /// Executes the native base's entry relation on encoded inputs.
    pub fn eval_input(
        &mut self,
        arena: &mut Arena,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        let Self { levels, protocols, base, options } = self;
        let name = &levels[0].config().rel;
        match base {
            // A P4 base runs the program directly
            BaseRunner::P4(runner) => runner.eval_rel(arena, name, values).result,
            // A meta base lends the levels above it with its definitions
            BaseRunner::Meta(runner) => {
                let Runner { backend, interface, external: upper, effects } = runner;
                let (engine, spec_base) = SpecTecEngine::lend_base(backend, interface);
                let env = TowerEnv { spec_base, levels, protocols, options: *options };
                let mut upper = upper.lend(&env);
                EngineKind::SpecTec(engine).call_rel(arena, &mut upper, effects, name, values)
            }
        }
    }
}

impl PreparedLevel {
    pub(super) fn config(&self) -> &LevelConfig {
        match self {
            Self::Base { config, .. } | Self::Al { config, .. } | Self::Il { config, .. } => config,
        }
    }
}

impl<Ext: Extern> BaseRunner<Ext> {
    /// Resets the runner and the levels its extern stores.
    fn reset(&mut self) {
        match self {
            Self::P4(runner) => runner.reset(),
            Self::Meta(runner) => runner.reset(),
        }
    }
}

// == Positions

/// A level of the tower: the base, or a host level above it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::metacirc) enum LevelAt {
    /// The base, which runs natively.
    Base,
    /// A level above the base, which the level below interprets.
    Host(HostLevel),
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

    /// Returns the level at `num` among all levels, the base at 0.
    pub(in crate::metacirc) fn new(num: usize) -> Self {
        Self(NonZeroUsize::new(num).expect("a host level is above the base"))
    }

    /// Returns the level below this one.
    pub(in crate::metacirc) fn below(self) -> LevelAt {
        NonZeroUsize::new(self.0.get() - 1)
            .map_or(LevelAt::Base, |number| LevelAt::Host(Self(number)))
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
