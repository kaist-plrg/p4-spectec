//! Interpreter towers and their native base
//!
//! The tower exposes program execution through its native entry point.
//! The base is a runner with an interpreter; every other level is a host,
//! since the level below interprets it, and `Upper` stores them all
//! in its extern slot, lent per call as `UpperMut` (see `upper`).
//! A single-level tower executes its top level directly as its base.
//! `build` assembles the levels; `input` reads the program the top accepts
//! and encodes the inputs of the base's entry;
//! `Tower::eval_input` runs the entry on the base's `Engine`.

mod build;
mod input;
mod upper;

pub(in crate::metacirc) use upper::{Top, TopMut, Upper, UpperMut};

use super::{
    config::LevelConfig,
    engine::{
        BaseSpec, Engine, EngineK, EngineKind, EngineRedex, EngineSt, InterfaceSt, RuntimeK,
        RuntimeRedex, StateK, StateRedex,
    },
    interp::TowerEnv,
    protocol::Entered,
};
use crate::{
    interface::p4::P4Interface,
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
    pub(super) base: Base<Ext>,
    /// Options for the interpreters of reentry depths.
    pub(super) options: runner::Config,
}

/// The input of a tower, prepared in the form the top accepts:
/// a P4 program for a P4 top level, a script above an AL or IL top level,
/// or a K definition above spec-k with its initial term.
pub enum Input {
    P4(Value),
    Script(Script),
    Kore(Kore),
}

/// A K definition and an initial term above spec-k, as `Krun` takes them.
pub struct Kore {
    /// The definition, as spec-k's `kdefinition` value.
    pub definition: Value,
    /// The initial term, as spec-k's `kore` value.
    pub init: Value,
    /// At most this many rewriting steps, as `krun --depth` does.
    pub depth: Option<u64>,
}

/// A script above the top level, entered at its function `main`.
pub enum Script {
    Al(al::Spec),
    Il(il::Spec),
}

impl Script {
    fn entered(&self) -> Entered<'_> {
        match self {
            Self::Al(spec_al) => Entered::Al(spec_al),
            Self::Il(spec_il) => Entered::Il(spec_il),
        }
    }
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
pub(super) enum Base<Ext: Extern> {
    /// A P4 specification, interpreting a parsed P4 program.
    P4(Runner<P4Interface, Ext>),
    /// A P4-SpecTec specification run natively on AL or SL, hosting the levels above it:
    /// spec-meta interpreting an AL or IL script, or spec-k a K definition.
    St(Runner<InterfaceSt, Upper<Ext>>),
    /// A K specification, compiled to a library, interpreting an AL script
    /// and hosting the levels above it.
    K(BaseK<Ext>),
    /// A PLT Redex specification, run by a `racket` process,
    /// interpreting an AL script and hosting the levels above it.
    Redex(BaseRedex<Ext>),
}

/// A base written in K: its library, depth 0's K thread, and the levels above it.
pub(super) struct BaseK<Ext: Extern> {
    pub(super) runtime: RuntimeK,
    pub(super) state: StateK,
    pub(super) upper: Upper<Ext>,
    pub(super) effects: u64,
}

/// A base written in PLT Redex: its process, depth 0's state, and the levels above it.
pub(super) struct BaseRedex<Ext: Extern> {
    pub(super) runtime: RuntimeRedex,
    pub(super) state: StateRedex,
    pub(super) upper: Upper<Ext>,
    pub(super) effects: u64,
}

/// A base that hosts levels above it, lent for one execution.
pub(super) struct LentBase<'b, Ext: Extern> {
    /// Depth 0's engine.
    pub(super) engine: EngineKind<'b>,
    /// The base's definitions, which deeper depths share.
    pub(super) spec_base: BaseSpec<'b>,
    /// The stored levels above the base.
    pub(super) upper: &'b mut Upper<Ext>,
    pub(super) effects: &'b mut u64,
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
    ) -> std::result::Result<Vec<Value>, InterpreterError> {
        let Self { levels, base, options } = self;
        let name = &levels[0].config().rel;
        // A P4 base runs the program directly
        if let Base::P4(runner) = base {
            return runner.eval_rel(arena, name, values).result;
        }
        // Any other base lends the levels above it with its definitions
        let LentBase { mut engine, spec_base, upper, effects } =
            base.lend().expect("a base other than P4 hosts levels");
        let env = TowerEnv { spec_base, levels, options: *options };
        let mut upper = upper.lend(&env);
        engine.call_rel(arena, &mut upper, effects, name, values)
    }
}

impl PreparedLevel {
    pub(super) fn config(&self) -> &LevelConfig {
        match self {
            Self::Base { config, .. } | Self::Al { config, .. } | Self::Il { config, .. } => config,
        }
    }

    /// Returns the specification the level below enters.
    fn entered(&self) -> Entered<'_> {
        match self {
            Self::Al { spec_al, .. } => Entered::Al(spec_al),
            Self::Il { spec_il, .. } => Entered::Il(spec_il),
            Self::Base { .. } => unreachable!("the base is never entered from another level"),
        }
    }
}

impl<Ext: Extern> Base<Ext> {
    /// Resets the runner and the levels its extern stores.
    fn reset(&mut self) {
        match self {
            Self::P4(runner) => runner.reset(),
            Self::St(runner) => runner.reset(),
            Self::K(base) => {
                base.effects = 0;
                EngineK::lend(&base.runtime, &mut base.state).reset();
                base.upper.clear();
            }
            Self::Redex(base) => {
                base.effects = 0;
                EngineRedex::lend(&base.runtime, &mut base.state).reset();
                base.upper.clear();
            }
        }
    }

    /// Returns the levels above the base, or nothing for a P4 base, which hosts no levels.
    fn upper(&self) -> Option<&Upper<Ext>> {
        match self {
            Self::P4(_) => None,
            Self::St(runner) => Some(&runner.external),
            Self::K(BaseK { upper, .. }) | Self::Redex(BaseRedex { upper, .. }) => Some(upper),
        }
    }

    /// Lends depth 0's engine and the levels above the base,
    /// or nothing for a P4 base, which hosts no levels.
    pub(super) fn lend(&mut self) -> Option<LentBase<'_, Ext>> {
        match self {
            Self::P4(_) => None,
            Self::St(runner) => {
                let Runner { backend, interface, external: upper, effects, hooks: _ } = runner;
                let (engine, spec_base) = EngineSt::lend_base(backend, interface);
                let engine = EngineKind::St(engine);
                let spec_base = BaseSpec::St(spec_base);
                Some(LentBase { engine, spec_base, upper, effects })
            }
            Self::K(BaseK { runtime, state, upper, effects }) => {
                let engine = EngineKind::K(EngineK::lend(runtime, state));
                let spec_base = BaseSpec::K(runtime);
                Some(LentBase { engine, spec_base, upper, effects })
            }
            Self::Redex(BaseRedex { runtime, state, upper, effects }) => {
                let engine = EngineKind::Redex(EngineRedex::lend(runtime, state));
                let spec_base = BaseSpec::Redex(runtime);
                Some(LentBase { engine, spec_base, upper, effects })
            }
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
/// per-level storage keeps its C, that boundary's protocol,
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
