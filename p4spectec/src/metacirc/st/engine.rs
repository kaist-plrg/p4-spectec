//! The AL or SL interpreter of a P4-SpecTec base specification
//!
//! `EngineSt` pairs the base's definitions, in the representation it executes,
//! with one depth's interpreter of them and that depth's base builtins.
//! Depth 0 borrows the base runner's, hooks included (`EngineSt::lend_base`);
//! a deeper depth owns a `StateSt` interpreting the same representation,
//! with hooks of its own, and lends it with `EngineSt::lend_depth`.

use crate::metacirc::{
    engine::{Engine, InterfaceSt},
    tower::UpperMut,
};
use crate::{
    interface,
    interp::{
        al::{AlInterp, Config as AlConfig, context::SpecEnv as AlSpecEnv},
        sl::{Config as SlConfig, SlInterp, context::SpecEnv as SlSpecEnv},
    },
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    runner::{
        self, Backend, Extern, Hooks, Interface, Interpreter, InterpreterError, NullExtern,
        RunnerContext,
    },
};

// == Engine

/// The base's native interpreter at one depth, with that depth's base builtins.
pub(in crate::metacirc) struct EngineSt<'s> {
    backend: BackendSt<'s>,
    interface: &'s mut InterfaceSt,
    hooks: &'s mut Hooks,
}

/// The base's definitions and a depth's interpreter of them.
enum BackendSt<'s> {
    Al { spec_env: &'s AlSpecEnv, interp: &'s mut AlInterp },
    Sl { spec_env: &'s SlSpecEnv, interp: &'s mut SlInterp },
}

impl<'s> EngineSt<'s> {
    /// Lends the base runner's backend, interface, and hooks as depth 0.
    pub(in crate::metacirc) fn lend_base(
        backend: &'s mut Backend,
        interface: &'s mut InterfaceSt,
        hooks: &'s mut Hooks,
    ) -> (Self, SpecSt<'s>) {
        let (backend, spec) = match backend {
            Backend::Al { spec_env, interp } => {
                (BackendSt::Al { spec_env, interp }, SpecSt::Al(spec_env))
            }
            Backend::Sl { spec_env, interp } => {
                (BackendSt::Sl { spec_env, interp }, SpecSt::Sl(spec_env))
            }
            Backend::Pl { .. } => unreachable!("a tower executes AL or SL natively"),
        };
        (Self { backend, interface, hooks }, spec)
    }

    /// Lends a deeper depth's state, interpreting the base's `spec`.
    pub(in crate::metacirc) fn lend_depth(state: &'s mut StateSt, spec: SpecSt<'s>) -> Self {
        let backend = match (spec, &mut state.interp) {
            (SpecSt::Al(spec_env), InterpSt::Al(interp)) => BackendSt::Al { spec_env, interp },
            (SpecSt::Sl(spec_env), InterpSt::Sl(interp)) => BackendSt::Sl { spec_env, interp },
            _ => unreachable!("a depth interprets the representation it was made for"),
        };
        Self { backend, interface: &mut state.interface, hooks: &mut state.hooks }
    }
}

/// Runs `$body` with `$ctx` bound to a context of `$engine`'s backend.
macro_rules! with_backend {
    ($engine:expr, $arena:expr, $upper:expr, $effects:expr, |$ctx:ident| $body:expr) => {{
        let engine = &mut *$engine;
        match &mut engine.backend {
            BackendSt::Al { spec_env, interp } => {
                let mut $ctx = RunnerContext::new(
                    $arena,
                    *spec_env,
                    &mut **interp,
                    &mut *engine.interface,
                    $upper,
                    $effects,
                    &mut *engine.hooks,
                );
                $body
            }
            BackendSt::Sl { spec_env, interp } => {
                let mut $ctx = RunnerContext::new(
                    $arena,
                    *spec_env,
                    &mut **interp,
                    &mut *engine.interface,
                    $upper,
                    $effects,
                    &mut *engine.hooks,
                );
                $body
            }
        }
    }};
}

// The interpreters are generic in their extern; clearing uses none
impl Engine for EngineSt<'_> {
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
            BackendSt::Al { interp, .. } => {
                Interpreter::<InterfaceSt, NullExtern>::clear(&mut **interp)
            }
            BackendSt::Sl { interp, .. } => {
                Interpreter::<InterfaceSt, NullExtern>::clear(&mut **interp)
            }
        }
    }

    fn reset(&mut self) {
        match &mut self.backend {
            BackendSt::Al { interp, .. } => {
                Interpreter::<InterfaceSt, NullExtern>::reset(&mut **interp)
            }
            BackendSt::Sl { interp, .. } => {
                Interpreter::<InterfaceSt, NullExtern>::reset(&mut **interp)
            }
        }
        self.interface.clear();
    }
}

// == Shared definitions

/// The base's definitions, in the representation it executes.
#[derive(Clone, Copy)]
pub(in crate::metacirc) enum SpecSt<'s> {
    Al(&'s AlSpecEnv),
    Sl(&'s SlSpecEnv),
}

// == Depth state

/// The engine state a depth beneath the base owns.
pub(in crate::metacirc) struct StateSt {
    interp: InterpSt,
    interface: InterfaceSt,
    hooks: Hooks,
}

/// An interpreter of the base's definitions, owned by a depth beneath the base.
enum InterpSt {
    Al(AlInterp),
    Sl(SlInterp),
}

impl StateSt {
    /// Makes the state of a depth interpreting `spec`, dispatching to `hooks`.
    pub(in crate::metacirc) fn new(
        options: runner::Config,
        spec: SpecSt<'_>,
        hooks: Hooks,
    ) -> Self {
        let interp = match spec {
            SpecSt::Al(_) => InterpSt::Al(AlInterp::new(AlConfig::new(
                options.cache,
                options.det,
                options.guard,
            ))),
            SpecSt::Sl(_) => InterpSt::Sl(SlInterp::new(SlConfig::new(
                options.cache,
                options.det,
                options.guard,
                options.tco,
            ))),
        };
        // Only the builtins are used; the base never parses at a depth.
        // A depth beneath the base exists only under a P4 top, over spec-meta
        Self { interp, interface: InterfaceSt::StSt(interface::meta()), hooks }
    }
}
