//! Native execution of the base specification at one reentry depth
//!
//! The base is the only level that runs natively;
//! an `Engine` calls its relations and functions,
//! sending the host calls it makes to the levels lent above it.
//! Depth 0 borrows the base runner's interpreter; each deeper depth owns one.
//! `EngineKind` lends the engines, `EngineStateKind` stores a deeper depth's,
//! and `BaseSpec` is the base's definitions they share;
//! `spectec` executes AL or SL, `k` a library compiled from a K base,
//! and `redex` a `racket` process running a PLT Redex base.
//! `InterfaceKind` answers the builtins of a base executed by `spectec`,
//! spec-meta or spec-k.

mod k;
mod redex;
mod spectec;

pub(super) use k::{KEngine, KEngineState, KRuntime};
pub(super) use redex::{RedexEngine, RedexEngineState, RedexRuntime};
pub(super) use spectec::SpecTecEngine;
use spectec::{SpecTecEngineState, SpecTecSpec};

use super::{Result as MetacircResult, tower::UpperMut};
use crate::{
    interface::{kore::KoreInterface, meta::MetaInterface},
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{self, Eval, Extern, Interface, InterfaceError, InterpreterError},
};
use std::path::PathBuf;

/// A native executor of the base specification at one depth.
pub(in crate::metacirc) trait Engine {
    /// Runs the program entry `name`, sending host calls to `levels`.
    fn call_program<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError>;

    /// Calls relation `name`, sending host calls to `levels`.
    fn call_rel<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError>;

    /// Calls function `name`, sending host calls to `levels`.
    fn call_func<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError>;

    /// Clears cached results without invalidating arena values.
    fn clear(&mut self);

    /// Clears cached results and builtin state between programs.
    fn reset(&mut self);
}

/// A lent engine.
pub(in crate::metacirc) enum EngineKind<'s> {
    /// The AL or SL interpreter of a P4-SpecTec specification.
    SpecTec(SpecTecEngine<'s>),
    /// The library compiled from a K specification.
    K(KEngine<'s>),
    /// The `racket` process running a PLT Redex specification.
    Redex(RedexEngine<'s>),
}

/// The engine state a depth beneath the base owns.
pub(in crate::metacirc) enum EngineStateKind {
    SpecTec(SpecTecEngineState),
    K(KEngineState),
    Redex(RedexEngineState),
}

/// The base's definitions, shared by every depth's engine.
#[derive(Clone, Copy)]
pub(in crate::metacirc) enum BaseSpec<'s> {
    SpecTec(SpecTecSpec<'s>),
    K(&'s KRuntime),
    Redex(&'s RedexRuntime),
}

impl EngineStateKind {
    /// Makes the state of a depth executing `spec`.
    pub(in crate::metacirc) fn new(
        options: runner::Config,
        spec: BaseSpec<'_>,
    ) -> MetacircResult<Self> {
        match spec {
            BaseSpec::SpecTec(spec) => Ok(Self::SpecTec(SpecTecEngineState::new(options, spec))),
            BaseSpec::K(runtime) => KEngineState::new(runtime).map(Self::K),
            BaseSpec::Redex(runtime) => RedexEngineState::new(runtime).map(Self::Redex),
        }
    }

    /// Lends this depth's engine, executing `spec`.
    pub(in crate::metacirc) fn lend<'s>(&'s mut self, spec: BaseSpec<'s>) -> EngineKind<'s> {
        match (self, spec) {
            (Self::SpecTec(state), BaseSpec::SpecTec(spec)) => {
                EngineKind::SpecTec(SpecTecEngine::lend_depth(state, spec))
            }
            (Self::K(state), BaseSpec::K(runtime)) => EngineKind::K(KEngine::lend(runtime, state)),
            (Self::Redex(state), BaseSpec::Redex(runtime)) => {
                EngineKind::Redex(RedexEngine::lend(runtime, state))
            }
            _ => unreachable!("a depth runs the engine of the base it was made for"),
        }
    }
}

impl Engine for EngineKind<'_> {
    fn call_program<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        match self {
            Self::SpecTec(engine) => engine.call_program(arena, upper, effects, name, program),
            Self::K(engine) => engine.call_program(arena, upper, effects, name, program),
            Self::Redex(engine) => engine.call_program(arena, upper, effects, name, program),
        }
    }

    fn call_rel<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        match self {
            Self::SpecTec(engine) => engine.call_rel(arena, upper, effects, name, values),
            Self::K(engine) => engine.call_rel(arena, upper, effects, name, values),
            Self::Redex(engine) => engine.call_rel(arena, upper, effects, name, values),
        }
    }

    fn call_func<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        match self {
            Self::SpecTec(engine) => engine.call_func(arena, upper, effects, name, targs, values),
            Self::K(engine) => engine.call_func(arena, upper, effects, name, targs, values),
            Self::Redex(engine) => engine.call_func(arena, upper, effects, name, targs, values),
        }
    }

    fn clear(&mut self) {
        match self {
            Self::SpecTec(engine) => engine.clear(),
            Self::K(engine) => engine.clear(),
            Self::Redex(engine) => engine.clear(),
        }
    }

    fn reset(&mut self) {
        match self {
            Self::SpecTec(engine) => engine.reset(),
            Self::K(engine) => engine.reset(),
            Self::Redex(engine) => engine.reset(),
        }
    }
}

// == Base builtins

/// The builtins of a base specification that `SpecTecEngine` executes,
/// by the language the base interprets.
pub(in crate::metacirc) enum InterfaceKind {
    /// spec-meta, interpreting AL or IL.
    Meta(MetaInterface),
    /// spec-k, interpreting KORE.
    Kore(KoreInterface),
}

impl Interface for InterfaceKind {
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        match self {
            Self::Meta(interface) => interface.parse_program(arena, includes, paths),
            Self::Kore(interface) => interface.parse_program(arena, includes, paths),
        }
    }

    fn call_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterfaceError> {
        match self {
            Self::Meta(interface) => interface.call_builtin(arena, id, targs, values),
            Self::Kore(interface) => interface.call_builtin(arena, id, targs, values),
        }
    }

    fn clear(&mut self) {
        match self {
            Self::Meta(interface) => interface.clear(),
            Self::Kore(interface) => interface.clear(),
        }
    }
}
