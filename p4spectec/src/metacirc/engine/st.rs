//! The AL or SL interpreter of a P4-SpecTec base specification
//!
//! `StEngine` pairs the base's definitions in the representation it executes
//! with one depth's interpreter of them and that depth's meta builtins.
//! Depth 0 borrows the base runner's (`StEngine::lend_base`);
//! a deeper depth owns an `StState` interpreting the same representation.

use super::super::boundary::Levels;
use super::Engine;
use crate::{
    interface::{self, Lang, meta::MetaInterface},
    interp::{
        al::{AlInterp, Config as AlConfig, context::SpecEnv as AlSpecEnv},
        sl::{Config as SlConfig, SlInterp, context::SpecEnv as SlSpecEnv},
    },
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::{
        self, Backend, Eval, Extern, Interface, Interpreter, InterpreterError, NullExtern,
        RunnerContext,
    },
};

/// The base's definitions, in the representation it executes.
#[derive(Clone, Copy)]
pub(in crate::metacirc) enum StSpec<'s> {
    Al(&'s AlSpecEnv),
    Sl(&'s SlSpecEnv),
}

/// An interpreter of the base's definitions, owned by a depth beneath the base.
enum StInterp {
    Al(AlInterp),
    Sl(SlInterp),
}

/// The engine state a depth beneath the base owns.
pub(in crate::metacirc) struct StState {
    interp: StInterp,
    interface: MetaInterface,
}

/// The base's definitions and a depth's interpreter of them.
enum StBackend<'s> {
    Al { spec_env: &'s AlSpecEnv, interp: &'s mut AlInterp },
    Sl { spec_env: &'s SlSpecEnv, interp: &'s mut SlInterp },
}

/// The base's native interpreter at one depth, with that depth's meta builtins.
pub(in crate::metacirc) struct StEngine<'s> {
    backend: StBackend<'s>,
    interface: &'s mut MetaInterface,
}

impl StState {
    /// Makes the state of a depth interpreting `spec`.
    pub(in crate::metacirc) fn new(options: runner::Config, spec: StSpec<'_>) -> Self {
        let interp = match spec {
            StSpec::Al(_) => StInterp::Al(AlInterp::new(AlConfig::new(
                options.cache,
                options.det,
                options.guard,
            ))),
            StSpec::Sl(_) => StInterp::Sl(SlInterp::new(SlConfig::new(
                options.cache,
                options.det,
                options.guard,
            ))),
        };
        // Only the builtins are used; the base never parses at a depth
        Self { interp, interface: interface::meta(Lang::Al) }
    }

    /// Lends this state as an engine interpreting `spec`.
    pub(in crate::metacirc) fn lend<'s>(&'s mut self, spec: StSpec<'s>) -> StEngine<'s> {
        let backend = match (spec, &mut self.interp) {
            (StSpec::Al(spec_env), StInterp::Al(interp)) => StBackend::Al { spec_env, interp },
            (StSpec::Sl(spec_env), StInterp::Sl(interp)) => StBackend::Sl { spec_env, interp },
            _ => unreachable!("a depth interprets the representation it was made for"),
        };
        StEngine { backend, interface: &mut self.interface }
    }
}

/// Runs `$body` with `$ctx` bound to a context of `$engine`'s backend.
macro_rules! with_backend {
    ($engine:expr, $arena:expr, $levels:expr, $effects:expr, |$ctx:ident| $body:expr) => {{
        let engine = &mut *$engine;
        match &mut engine.backend {
            StBackend::Al { spec_env, interp } => {
                let mut $ctx = RunnerContext::new(
                    $arena,
                    *spec_env,
                    &mut **interp,
                    &mut *engine.interface,
                    $levels,
                    $effects,
                );
                $body
            }
            StBackend::Sl { spec_env, interp } => {
                let mut $ctx = RunnerContext::new(
                    $arena,
                    *spec_env,
                    &mut **interp,
                    &mut *engine.interface,
                    $levels,
                    $effects,
                );
                $body
            }
        }
    }};
}

impl<'s> StEngine<'s> {
    /// Lends the base runner's backend and interface as depth 0.
    pub(in crate::metacirc) fn lend_base(
        backend: &'s mut Backend,
        interface: &'s mut MetaInterface,
    ) -> (Self, StSpec<'s>) {
        let (backend, spec) = match backend {
            Backend::Al { spec_env, interp } => {
                (StBackend::Al { spec_env, interp }, StSpec::Al(spec_env))
            }
            Backend::Sl { spec_env, interp } => {
                (StBackend::Sl { spec_env, interp }, StSpec::Sl(spec_env))
            }
            Backend::Pl { .. } => unreachable!("a tower executes AL or SL natively"),
        };
        (Self { backend, interface }, spec)
    }
}

// The interpreters are generic in their extern; clearing uses none
impl Engine for StEngine<'_> {
    fn call_program<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        levels: &mut Levels<'_, Ext>,
        effects: &mut u64,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        with_backend!(self, arena, levels, effects, |ctx| ctx.call_program(name, program))
    }

    fn call_rel<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        levels: &mut Levels<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        with_backend!(self, arena, levels, effects, |ctx| ctx.call_rel(name, values).result)
    }

    fn call_func<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        levels: &mut Levels<'_, Ext>,
        effects: &mut u64,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        with_backend!(self, arena, levels, effects, |ctx| ctx.call_func(name, targs, values).result)
    }

    fn clear(&mut self) {
        match &mut self.backend {
            StBackend::Al { interp, .. } => {
                Interpreter::<MetaInterface, NullExtern>::clear(&mut **interp)
            }
            StBackend::Sl { interp, .. } => {
                Interpreter::<MetaInterface, NullExtern>::clear(&mut **interp)
            }
        }
    }

    fn reset(&mut self) {
        match &mut self.backend {
            StBackend::Al { interp, .. } => {
                Interpreter::<MetaInterface, NullExtern>::reset(&mut **interp)
            }
            StBackend::Sl { interp, .. } => {
                Interpreter::<MetaInterface, NullExtern>::reset(&mut **interp)
            }
        }
        self.interface.clear();
    }
}
