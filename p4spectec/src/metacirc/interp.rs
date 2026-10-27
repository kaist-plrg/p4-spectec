//! The P4 level's interpreter at one reentry depth, run through the tower
//!
//! For a P4 call `eval_rel(V1Model_parser, values)`,
//! `TowerInterp` calls the relation at the top `HostLevel`:
//! each level runs through the `Protocol` below it, lowered to a call
//! on the level below, down to the depth's `Engine` at `LevelAt::Base`;
//! C, a level loaded into the level below, is made once per program.
//! A reentry from the P4 host runs on the next `Depth`, which shares
//! the base's definitions, C, and the codec caches with this one.

use super::{
    engine::{Engine, EngineKind, SpecTecEngine, SpecTecSpec, SpecTecState},
    protocol::{Below, Call, Output, Protocol, ProtocolKind, Values},
    tower::{HostLevel, LevelAt, LevelSpec, TopMut, UpperMut},
};
use crate::{
    interface::{
        self, Lang,
        meta::{MetaInterface, shared::Cache},
        p4::P4Interface,
    },
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::{
        self, Extern, Interface, Interpreter, InterpreterError, RunnerContext, RunnerContextParts,
    },
};
use std::borrow::Cow;

// == The P4 level's interpreter

/// Definitions shared by every depth.
pub(in crate::metacirc) struct TowerEnv<'s> {
    /// The base specification, executed natively.
    pub spec_base: SpecTecSpec<'s>,
    /// Every level of the tower, from the base to the top.
    pub specs: &'s [LevelSpec],
    /// The protocol of the boundary below each level above the base.
    pub protocols: &'s [ProtocolKind],
    /// Options for the engines of deeper depths.
    pub options: runner::Config,
}

impl TowerEnv<'_> {
    /// Returns the specification of `level`, in the form the level below loads.
    fn spec(&self, level: HostLevel) -> &LevelSpec {
        &self.specs[level.number()]
    }

    /// Returns the protocol of the boundary below `level`.
    pub(in crate::metacirc) fn protocol(&self, level: HostLevel) -> &ProtocolKind {
        &self.protocols[level.index()]
    }
}

/// The P4 level's interpreter at one depth, borrowing that depth's state.
pub(in crate::metacirc) struct TowerInterp<'s> {
    pub engine: EngineKind<'s>,
    /// The meta builtins of each level between the base and the top.
    pub hosts: &'s mut [MetaInterface],
    /// The depths beneath this one, which answer reentry.
    pub depth_next: &'s mut Option<Box<Depth>>,
    /// The codec cache of each level boundary, shared by every depth.
    pub caches: &'s mut [Cache],
    /// C of each level above the base, shared by every depth.
    pub ctxs: &'s mut [Option<Value>],
}

impl<'s, Ext: Extern> Interpreter<P4Interface, Ext> for TowerInterp<'s> {
    type SpecEnv = TowerEnv<'s>;

    fn clear(&mut self) {
        self.engine.clear();
        *self.depth_next = None;
        self.caches.iter_mut().for_each(Cache::clear);
    }

    fn reset(&mut self) {
        self.engine.reset();
        self.hosts.iter_mut().for_each(Interface::clear);
        *self.depth_next = None;
        self.ctxs.fill(None);
        self.caches.iter_mut().for_each(Cache::clear);
    }

    fn eval_program(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        name: &str,
        program: Value,
    ) -> Result<Vec<Value>, InterpreterError> {
        Self::eval_rel(ctx, name, &[program])
    }

    fn eval_rel(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        Self::with_tower(ctx, |tower| {
            let level_top = tower.upper.level_top();
            let call = Call::Rel { name, values: Values::Borrowed(values) };
            tower
                .call(LevelAt::Host(level_top), call)
                .map(Output::into_rel)
        })
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        Self::with_tower(ctx, |tower| {
            let level_top = tower.upper.level_top();
            let call =
                Call::Func { name, targs: Cow::Borrowed(targs), values: Values::Borrowed(values) };
            tower
                .call(LevelAt::Host(level_top), call)
                .map(Output::into_func)
        })
    }
}

impl<'s> TowerInterp<'s> {
    /// Runs `body` on the tower at this depth,
    /// sending the base's host calls up to the P4 host of `ctx`.
    fn with_tower<Ext: Extern, T>(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        body: impl FnOnce(&mut TowerCall<'_, 's, Ext>) -> Result<T, InterpreterError>,
    ) -> Result<T, InterpreterError> {
        // Split the P4 context: the host goes to the lent levels, the depth runs the base
        let RunnerContextParts { arena, spec_env: env, interp, interface, external, effects } =
            ctx.parts_mut();
        let TowerInterp { engine, hosts, depth_next, caches, ctxs } = interp;
        let upper = UpperMut {
            env,
            caches,
            ctxs,
            hosts,
            top: TopMut::P4 { interface, external },
            depth_next,
        };
        // Run the base in the same arena and effect count
        body(&mut TowerCall { arena, engine, upper, effects })
    }
}

// == Calls down the tower
//
//   eval_rel(ctx, "V1Model_parser", vals)        ctx.interp: depth d
//     with_tower                                 P4 host -> UpperMut
//       call(top, Rel V1Model_parser vals)       on the top level
//         load_ctx(top) -> C                     once per program
//         lower -> Rel Call_rel [C, name, vals]  one level down, until
//         call(top.below(), ..)                  the base runs natively
//           Call_extern_rel(..) -> Boundary      up each boundary
//             Ext::eval_rel(ctx', ..)            ctx'.interp: depth d+1
//               ctx'.call_func("find_var_e")     reentry with the same C
//         raise relRes -> Ok | Mismatch | Fatal

/// The tower at one depth during one P4 call, with the levels lent to it.
struct TowerCall<'c, 's, Ext: Extern> {
    arena: &'c mut Arena,
    engine: &'c mut EngineKind<'s>,
    upper: UpperMut<'c, Ext>,
    effects: &'c mut u64,
}

impl<Ext: Extern> TowerCall<'_, '_, Ext> {
    /// Calls a relation or function of the specification at `floor`.
    fn call(&mut self, floor: LevelAt, call: Call<'_>) -> Result<Output, InterpreterError> {
        let level = match floor {
            // The base runs natively
            LevelAt::Base => {
                let Self { arena, engine, upper, effects } = self;
                return match call {
                    Call::Rel { name, values } => engine
                        .call_rel(arena, upper, effects, name, &values)
                        .map(Output::Rel),
                    Call::Func { name, targs, values } => engine
                        .call_func(arena, upper, effects, name, &targs, &values)
                        .map(Output::Func),
                };
            }
            // A host level runs through the protocol of the level below
            LevelAt::Host(level) => level,
        };
        let value_ctx = self.load_ctx(level)?;
        let env = self.upper.env;
        let protocol = env.protocol(level);
        let call_below = protocol.lower(self.arena, self.upper.cache(level), value_ctx, &call)?;
        let output = self.call(level.below(), call_below)?;
        protocol.raise(self.arena, self.upper.cache(level), output, &call)
    }

    /// Returns C of `level`, loading it into the level below on first use.
    fn load_ctx(&mut self, level: HostLevel) -> Result<Value, InterpreterError> {
        // A level's C lives until the program is reset
        if let Some(value_ctx) = self.upper.ctx(level) {
            return Ok(value_ctx);
        }
        // Load the specification through the protocol, on the level below
        let env = self.upper.env;
        let mut below = BelowFloor { tower: self, floor: level.below() };
        let value_ctx = env.protocol(level).load(&mut below, env.spec(level))?;
        self.upper.set_ctx(level, value_ctx);
        Ok(value_ctx)
    }
}

/// The level below a boundary, on which a protocol loads C.
struct BelowFloor<'b, 'c, 's, Ext: Extern> {
    tower: &'b mut TowerCall<'c, 's, Ext>,
    floor: LevelAt,
}

impl<Ext: Extern> Below for BelowFloor<'_, '_, '_, Ext> {
    fn arena(&mut self) -> &mut Arena {
        self.tower.arena
    }

    fn call(&mut self, call: Call<'_>) -> Result<Output, InterpreterError> {
        self.tower.call(self.floor, call)
    }
}

// == Depth state

/// The state of one reentry depth beneath the base.
pub(in crate::metacirc) struct Depth {
    engine: SpecTecState,
    /// The meta builtins of each level between the base and the top.
    hosts: Vec<MetaInterface>,
    next: Option<Box<Depth>>,
}

impl Depth {
    /// Makes a depth executing `spec_base`, with builtins for `count` levels
    /// between base and top.
    pub(in crate::metacirc) fn new(
        options: runner::Config,
        spec_base: SpecTecSpec<'_>,
        count: usize,
    ) -> Self {
        Self {
            engine: SpecTecState::new(options, spec_base),
            hosts: (0..count).map(|_| interface::meta(Lang::Al)).collect(),
            next: None,
        }
    }

    /// Lends this depth, executing `spec_base`, as the P4 level's interpreter.
    pub(in crate::metacirc) fn lend<'d>(
        &'d mut self,
        spec_base: SpecTecSpec<'d>,
        caches: &'d mut [Cache],
        ctxs: &'d mut [Option<Value>],
    ) -> TowerInterp<'d> {
        TowerInterp {
            engine: EngineKind::SpecTec(SpecTecEngine::lend_depth(&mut self.engine, spec_base)),
            hosts: &mut self.hosts,
            depth_next: &mut self.next,
            caches,
            ctxs,
        }
    }
}
