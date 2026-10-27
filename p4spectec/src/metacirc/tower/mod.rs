//! Interpreter towers and their native base
//!
//! The tower exposes program execution through its native entry point.
//! The base is a runner with an interpreter; every other level is a host,
//! since the level below interprets it, and `Upper` stores them all
//! in its extern slot, lent per call as `UpperMut` (see `upper`).
//! A single-level tower executes its top level directly as its base.
//! `build` assembles the levels; `patch` adds the entry the base calls;
//! `Tower::eval_program` runs it on the base's `Engine`.
//! A KORE input enters spec-k through `Tower::eval_krun` instead.

mod build;
mod patch;
mod upper;

pub(in crate::metacirc) use upper::{Top, TopMut, Upper, UpperMut};

use super::{
    Result,
    config::LevelConfig,
    engine::{
        BaseSpec, Engine, EngineKind, InterfaceKind, KEngine, KEngineState, KRuntime, SpecTecEngine,
    },
    error,
    interp::TowerEnv,
    protocol::ProtocolKind,
};
use crate::{
    interface::{kore::KoreInterface, p4::P4Interface},
    lang::{
        al::ast as al,
        common::{prim::num::Natural, source::Span},
        data::{
            arena::Arena,
            typ,
            value::flat::{self as value, Value},
        },
        il::ast as il,
    },
    runner::{self, Eval, Extern, InterpreterError, Runner},
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
    Meta(Runner<InterfaceKind, Upper<Ext>>),
    /// A K specification, compiled to a library, interpreting an AL script
    /// and hosting the levels above it.
    K(KBase<Ext>),
}

/// A base written in K: its library, depth 0's K thread, and the levels above it.
pub(super) struct KBase<Ext: Extern> {
    pub(super) runtime: KRuntime,
    pub(super) state: KEngineState,
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

    /// Executes a patched input at the native base's entry relation.
    pub fn eval_program(&mut self, arena: &mut Arena, program: Value) -> Eval<Vec<Value>> {
        let Self { levels, protocols, base, options } = self;
        let name = &levels[0].config().rel;
        // A P4 base runs the program directly
        if let BaseRunner::P4(runner) = base {
            return runner.eval_program(arena, name, program);
        }
        // Any other base lends the levels above it with its definitions
        let LentBase { mut engine, spec_base, upper, effects } =
            base.lend().expect("a base other than P4 hosts levels");
        let env = TowerEnv { spec_base, levels, protocols, options: *options };
        let mut upper = upper.lend(&env);
        engine.call_program(arena, &mut upper, effects, name, program)
    }
}

impl<Ext: Extern> Tower<Ext> {
    /// Runs spec-k's entry relation `Krun` on a K definition and an initial term,
    /// rewriting at most `depth` steps if given, as `krun --depth` does.
    ///
    /// `definition` and `init` are what `KoreInterface::parse_program` read;
    /// the result is the `runres` that `KoreInterface::print_runres` prints.
    pub fn eval_krun(
        &mut self,
        arena: &mut Arena,
        definition: Value,
        init: Value,
        depth: Option<u64>,
    ) -> Result<Value> {
        let Self { levels, protocols, base, options } = self;
        let name = &levels[0].config().rel;
        // The step bound is spec-k's nat?
        let value_depth = depth
            .map(|depth| value::make::nat(arena, Natural::from(depth), Span::default()))
            .transpose()
            .map_err(|error| error::input_invalid(error.to_string()))?;
        let typ_depth = typ::make::opt(typ::make::nat());
        let value_depth =
            value::make::opt(arena, typ_depth.node.into(), value_depth, Span::default())
                .map_err(|error| error::input_invalid(error.to_string()))?;
        // spec-k is the base, which hosts the program's empty level above it
        let Some(LentBase { mut engine, spec_base, upper, effects }) = base.lend() else {
            return Err(error::input_invalid("a KORE input requires spec-k as the base"));
        };
        let env = TowerEnv { spec_base, levels, protocols, options: *options };
        let mut upper = upper.lend(&env);
        let values = [definition, init, value_depth];
        let outputs = engine
            .call_rel(arena, &mut upper, effects, name, &values)
            .map_err(InterpreterError::into_report)?;
        value::get::one(&outputs)
            .copied()
            .map_err(|error| error::input_invalid(error.to_string()))
    }
}

impl<Ext: Extern> Tower<Ext> {
    /// Returns the base's interface if the base is spec-k,
    /// which parses a KORE input, answers spec-k's hooks, and prints its run.
    pub(super) fn kore_interface(&mut self) -> Option<&mut KoreInterface> {
        match &mut self.base {
            BaseRunner::Meta(Runner { interface: InterfaceKind::Kore(interface), .. }) => {
                Some(interface)
            }
            _ => None,
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
            Self::K(base) => {
                base.effects = 0;
                KEngine::lend(&base.runtime, &mut base.state).reset();
                base.upper.clear();
            }
        }
    }

    /// Lends depth 0's engine and the levels above the base,
    /// or nothing for a P4 base, which hosts no levels.
    pub(super) fn lend(&mut self) -> Option<LentBase<'_, Ext>> {
        match self {
            Self::P4(_) => None,
            Self::Meta(runner) => {
                let Runner { backend, interface, external: upper, effects } = runner;
                let (engine, spec_base) = SpecTecEngine::lend_base(backend, interface);
                let engine = EngineKind::SpecTec(engine);
                let spec_base = BaseSpec::SpecTec(spec_base);
                Some(LentBase { engine, spec_base, upper, effects })
            }
            Self::K(KBase { runtime, state, upper, effects }) => {
                let engine = EngineKind::K(KEngine::lend(runtime, state));
                let spec_base = BaseSpec::K(runtime);
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
