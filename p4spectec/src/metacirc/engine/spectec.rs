//! The AL or SL interpreter of a P4-SpecTec base specification
//!
//! `SpecTecEngine` pairs the base's definitions, in the representation it executes,
//! with one depth's interpreter of them and that depth's meta builtins.
//! Depth 0 borrows the base runner's (`SpecTecEngine::lend_base`);
//! a deeper depth owns a `SpecTecEngineState` interpreting the same representation
//! and lends it with `SpecTecEngine::lend_depth`.

use super::super::tower::UpperMut;
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

// == Engine

/// The base's native interpreter at one depth, with that depth's meta builtins.
pub(in crate::metacirc) struct SpecTecEngine<'s> {
    backend: SpecTecBackend<'s>,
    interface: &'s mut MetaInterface,
}

/// The base's definitions and a depth's interpreter of them.
enum SpecTecBackend<'s> {
    Al { spec_env: &'s AlSpecEnv, interp: &'s mut AlInterp },
    Sl { spec_env: &'s SlSpecEnv, interp: &'s mut SlInterp },
}

impl<'s> SpecTecEngine<'s> {
    /// Lends the base runner's backend and interface as depth 0.
    pub(in crate::metacirc) fn lend_base(
        backend: &'s mut Backend,
        interface: &'s mut MetaInterface,
    ) -> (Self, SpecTecSpec<'s>) {
        let (backend, spec) = match backend {
            Backend::Al { spec_env, interp } => {
                (SpecTecBackend::Al { spec_env, interp }, SpecTecSpec::Al(spec_env))
            }
            Backend::Sl { spec_env, interp } => {
                (SpecTecBackend::Sl { spec_env, interp }, SpecTecSpec::Sl(spec_env))
            }
            Backend::Pl { .. } => unreachable!("a tower executes AL or SL natively"),
        };
        (Self { backend, interface }, spec)
    }

    /// Lends a deeper depth's state, interpreting the base's `spec`.
    pub(in crate::metacirc) fn lend_depth(
        state: &'s mut SpecTecEngineState,
        spec: SpecTecSpec<'s>,
    ) -> Self {
        let backend = match (spec, &mut state.interp) {
            (SpecTecSpec::Al(spec_env), SpecTecInterp::Al(interp)) => {
                SpecTecBackend::Al { spec_env, interp }
            }
            (SpecTecSpec::Sl(spec_env), SpecTecInterp::Sl(interp)) => {
                SpecTecBackend::Sl { spec_env, interp }
            }
            _ => unreachable!("a depth interprets the representation it was made for"),
        };
        Self { backend, interface: &mut state.interface }
    }
}

/// Runs `$body` with `$ctx` bound to a context of `$engine`'s backend.
macro_rules! with_backend {
    ($engine:expr, $arena:expr, $upper:expr, $effects:expr, |$ctx:ident| $body:expr) => {{
        let engine = &mut *$engine;
        match &mut engine.backend {
            SpecTecBackend::Al { spec_env, interp } => {
                let mut $ctx = RunnerContext::new(
                    $arena,
                    *spec_env,
                    &mut **interp,
                    &mut *engine.interface,
                    $upper,
                    $effects,
                );
                $body
            }
            SpecTecBackend::Sl { spec_env, interp } => {
                let mut $ctx = RunnerContext::new(
                    $arena,
                    *spec_env,
                    &mut **interp,
                    &mut *engine.interface,
                    $upper,
                    $effects,
                );
                $body
            }
        }
    }};
}

// The interpreters are generic in their extern; clearing uses none
impl Engine for SpecTecEngine<'_> {
    fn call_program<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        with_backend!(self, arena, upper, effects, |ctx| ctx.call_program(name, program))
    }

    fn call_rel<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        with_backend!(self, arena, upper, effects, |ctx| ctx.call_rel(name, values).result)
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
        with_backend!(self, arena, upper, effects, |ctx| ctx.call_func(name, targs, values).result)
    }

    fn clear(&mut self) {
        match &mut self.backend {
            SpecTecBackend::Al { interp, .. } => {
                Interpreter::<MetaInterface, NullExtern>::clear(&mut **interp)
            }
            SpecTecBackend::Sl { interp, .. } => {
                Interpreter::<MetaInterface, NullExtern>::clear(&mut **interp)
            }
        }
    }

    fn reset(&mut self) {
        match &mut self.backend {
            SpecTecBackend::Al { interp, .. } => {
                Interpreter::<MetaInterface, NullExtern>::reset(&mut **interp)
            }
            SpecTecBackend::Sl { interp, .. } => {
                Interpreter::<MetaInterface, NullExtern>::reset(&mut **interp)
            }
        }
        self.interface.clear();
    }
}

// == Shared definitions

/// The base's definitions, in the representation it executes.
#[derive(Clone, Copy)]
pub(in crate::metacirc) enum SpecTecSpec<'s> {
    Al(&'s AlSpecEnv),
    Sl(&'s SlSpecEnv),
}

// == Depth state

/// The engine state a depth beneath the base owns.
pub(in crate::metacirc) struct SpecTecEngineState {
    interp: SpecTecInterp,
    interface: MetaInterface,
}

/// An interpreter of the base's definitions, owned by a depth beneath the base.
enum SpecTecInterp {
    Al(AlInterp),
    Sl(SlInterp),
}

impl SpecTecEngineState {
    /// Makes the state of a depth interpreting `spec`.
    pub(in crate::metacirc) fn new(options: runner::Config, spec: SpecTecSpec<'_>) -> Self {
        let interp = match spec {
            SpecTecSpec::Al(_) => SpecTecInterp::Al(AlInterp::new(AlConfig::new(
                options.cache,
                options.det,
                options.guard,
            ))),
            SpecTecSpec::Sl(_) => SpecTecInterp::Sl(SlInterp::new(SlConfig::new(
                options.cache,
                options.det,
                options.guard,
            ))),
        };
        // Only the builtins are used; the base never parses at a depth
        Self { interp, interface: interface::meta(Lang::Al) }
    }
}
