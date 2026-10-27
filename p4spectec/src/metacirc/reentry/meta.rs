//! The P4 level's interpreter at one reentry depth, run through spec-meta
//!
//! For a P4 call `eval_rel(V1Model_parser, values)` on a tower of n levels,
//! `MetaInterp` calls the relation at the top `HostLevel`:
//! each level above the base runs as `Call_rel(C, name, values)`
//! on spec-meta at the level below, down to the depth's native interpreter,
//! where C loads that level's specification once per program.
//! spec-meta's host calls go up through the levels the caller lent
//! (`boundary::Levels`); a reentry from the P4 host runs on the next `MetaDepth`.
//! Depths share spec-meta, each level's C, and the codec caches;
//! each depth owns its AL or SL interpreter and its meta builtins.

use super::super::{
    boundary::{Levels, TopMut},
    relay::codec,
    tower::{Floor, HostLevel, Level},
};
use crate::{
    interface::{
        self, Lang,
        meta::{
            MetaInterface,
            shared::{Cache, encode::Encode},
        },
        p4::P4Interface,
    },
    interp::{
        al::{AlInterp, Config as AlConfig, context::SpecEnv as AlSpecEnv},
        sl::{Config as SlConfig, SlInterp, context::SpecEnv as SlSpecEnv},
    },
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{self as value, Value},
        },
        il::ast::Typ,
    },
    phrase,
    runner::{
        self, Backend, Eval, Extern, ExternError, Interface, Interpreter, InterpreterError,
        RunnerContext, RunnerContextParts,
    },
};

// == Native execution of spec-meta

/// spec-meta's definitions, in the representation the base executes.
#[derive(Clone, Copy)]
pub(in crate::metacirc) enum SpecMeta<'s> {
    Al(&'s AlSpecEnv),
    Sl(&'s SlSpecEnv),
}

/// A native interpreter of spec-meta, owned by a depth beneath the base.
enum InterpMeta {
    Al(AlInterp),
    Sl(SlInterp),
}

/// spec-meta's definitions and a depth's interpreter of them.
enum BackendMut<'s> {
    Al { spec_env: &'s AlSpecEnv, interp: &'s mut AlInterp },
    Sl { spec_env: &'s SlSpecEnv, interp: &'s mut SlInterp },
}

/// spec-meta's native interpreter at one depth, with that depth's meta builtins.
pub(in crate::metacirc) struct Native<'s> {
    backend: BackendMut<'s>,
    interface: &'s mut MetaInterface,
}

/// Runs `$body` with `$ctx` bound to a spec-meta context of `$native`'s backend.
macro_rules! with_native {
    ($native:expr, $arena:expr, $levels:expr, $effects:expr, |$ctx:ident| $body:expr) => {{
        let native = &mut *$native;
        match &mut native.backend {
            BackendMut::Al { spec_env, interp } => {
                let mut $ctx = RunnerContext::new(
                    $arena,
                    *spec_env,
                    &mut **interp,
                    &mut *native.interface,
                    $levels,
                    $effects,
                );
                $body
            }
            BackendMut::Sl { spec_env, interp } => {
                let mut $ctx = RunnerContext::new(
                    $arena,
                    *spec_env,
                    &mut **interp,
                    &mut *native.interface,
                    $levels,
                    $effects,
                );
                $body
            }
        }
    }};
}

impl<'s> Native<'s> {
    /// Lends the base runner's backend and interface as depth 0.
    pub(in crate::metacirc) fn lend(
        backend: &'s mut Backend,
        interface: &'s mut MetaInterface,
    ) -> (Self, SpecMeta<'s>) {
        let (backend, spec_meta) = match backend {
            Backend::Al { spec_env, interp } => {
                (BackendMut::Al { spec_env, interp }, SpecMeta::Al(spec_env))
            }
            Backend::Sl { spec_env, interp } => {
                (BackendMut::Sl { spec_env, interp }, SpecMeta::Sl(spec_env))
            }
            Backend::Pl { .. } => unreachable!("a tower executes AL or SL natively"),
        };
        (Self { backend, interface }, spec_meta)
    }

    /// Runs spec-meta's program entry, sending host calls to `levels`.
    pub(in crate::metacirc) fn call_program<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        levels: &mut Levels<'_, Ext>,
        effects: &mut u64,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        with_native!(self, arena, levels, effects, |ctx| ctx.call_program(name, program))
    }

    /// Calls a relation of spec-meta, sending host calls to `levels`.
    fn call_rel<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        levels: &mut Levels<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, InterpreterError> {
        with_native!(self, arena, levels, effects, |ctx| ctx.call_rel(name, values))
    }

    /// Calls a function of spec-meta, sending host calls to `levels`.
    fn call_func<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        levels: &mut Levels<'_, Ext>,
        effects: &mut u64,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterpreterError> {
        with_native!(self, arena, levels, effects, |ctx| ctx.call_func(name, targs, values))
    }

    /// Clears the interpreter's cached results.
    fn clear<Ext: Extern>(&mut self) {
        match &mut self.backend {
            BackendMut::Al { interp, .. } => {
                Interpreter::<MetaInterface, Levels<'_, Ext>>::clear(&mut **interp)
            }
            BackendMut::Sl { interp, .. } => {
                Interpreter::<MetaInterface, Levels<'_, Ext>>::clear(&mut **interp)
            }
        }
    }

    /// Resets the interpreter and the meta builtins between programs.
    fn reset<Ext: Extern>(&mut self) {
        match &mut self.backend {
            BackendMut::Al { interp, .. } => {
                Interpreter::<MetaInterface, Levels<'_, Ext>>::reset(&mut **interp)
            }
            BackendMut::Sl { interp, .. } => {
                Interpreter::<MetaInterface, Levels<'_, Ext>>::reset(&mut **interp)
            }
        }
        self.interface.clear();
    }
}

// == Depth state

/// The interpreter state of one reentry depth beneath the base.
pub(in crate::metacirc) struct MetaDepth {
    interp: InterpMeta,
    interface: MetaInterface,
    /// The meta builtins of each level between the base and the top.
    hosts: Vec<MetaInterface>,
    next: Option<Box<MetaDepth>>,
}

impl MetaDepth {
    /// Makes a depth interpreting `spec_meta`, with builtins for `count` levels
    /// between base and top.
    pub(in crate::metacirc) fn new(
        options: runner::Config,
        spec_meta: SpecMeta<'_>,
        count: usize,
    ) -> Self {
        let interp = match spec_meta {
            SpecMeta::Al(_) => InterpMeta::Al(AlInterp::new(AlConfig::new(
                options.cache,
                options.det,
                options.guard,
            ))),
            SpecMeta::Sl(_) => InterpMeta::Sl(SlInterp::new(SlConfig::new(
                options.cache,
                options.det,
                options.guard,
            ))),
        };
        Self {
            interp,
            // Only the builtins are used; spec-meta never parses at a depth
            interface: interface::meta(Lang::Al),
            hosts: (0..count).map(|_| interface::meta(Lang::Al)).collect(),
            next: None,
        }
    }

    /// Lends this depth, interpreting `spec_meta`, as the P4 level's interpreter.
    pub(in crate::metacirc) fn lend<'d>(
        &'d mut self,
        spec_meta: SpecMeta<'d>,
        caches: &'d mut [Cache],
        ctxs: &'d mut [Option<Value>],
    ) -> MetaInterp<'d> {
        let backend = match (spec_meta, &mut self.interp) {
            (SpecMeta::Al(spec_env), InterpMeta::Al(interp)) => BackendMut::Al { spec_env, interp },
            (SpecMeta::Sl(spec_env), InterpMeta::Sl(interp)) => BackendMut::Sl { spec_env, interp },
            _ => unreachable!("a depth interprets the representation it was made for"),
        };
        MetaInterp {
            native: Native { backend, interface: &mut self.interface },
            hosts: &mut self.hosts,
            depth_next: &mut self.next,
            caches,
            ctxs,
        }
    }
}

/// Definitions shared by every depth.
pub(in crate::metacirc) struct MetaEnv<'s> {
    /// spec-meta, executed natively.
    pub spec_meta: SpecMeta<'s>,
    /// Every level of the tower, from the base to the top.
    pub levels: &'s [Level],
    /// Options for the interpreters of deeper levels.
    pub options: runner::Config,
}

/// The P4 level's interpreter at one depth, borrowing that depth's state.
pub(in crate::metacirc) struct MetaInterp<'s> {
    pub native: Native<'s>,
    /// The meta builtins of each level between the base and the top.
    pub hosts: &'s mut [MetaInterface],
    /// The depths beneath this one, which answer reentry.
    pub depth_next: &'s mut Option<Box<MetaDepth>>,
    /// The codec cache of each level boundary, shared by every depth.
    pub caches: &'s mut [Cache],
    /// C of each level above the base, shared by every depth.
    pub ctxs: &'s mut [Option<Value>],
}

// == Calls into spec-meta
//
//   eval_rel(ctx, "V1Model_parser", vals)        ctx.interp: depth d
//     with_meta                                  P4 host -> Levels
//       call_rel(top, "V1Model_parser", vals)    on the top level
//         load_ctx(top) -> C                     once per program
//         call_rel(top.below(), "Call_rel",      one level down, until
//                  [C, "V1Model_parser", vals])  the base runs natively
//           Call_extern_rel(..) -> Boundary      up each boundary
//             Ext::eval_rel(ctx', ..)            ctx'.interp: depth d+1
//               ctx'.call_func("find_var_e")     reentry with the same C
//         relRes -> Ok | Mismatch | Fatal

/// spec-meta at one depth during one P4 call, with the levels lent to it.
struct MetaCall<'c, 's, Ext: Extern> {
    arena: &'c mut Arena,
    native: &'c mut Native<'s>,
    levels: Levels<'c, Ext>,
    effects: &'c mut u64,
}

impl<'s> MetaInterp<'s> {
    /// Runs `call` on spec-meta at this depth,
    /// relaying its host calls up to the P4 host of `ctx`.
    fn with_meta<Ext: Extern, T>(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        call: impl FnOnce(&mut MetaCall<'_, 's, Ext>) -> Result<T, InterpreterError>,
    ) -> Result<T, InterpreterError> {
        // Split the P4 context: the host goes to the lent levels, the depth runs spec-meta
        let RunnerContextParts { arena, spec_env: env, interp: meta, interface, external, effects } =
            ctx.parts_mut();
        let MetaInterp { native, hosts, depth_next, caches, ctxs } = meta;
        let levels = Levels {
            env,
            caches,
            ctxs,
            hosts,
            top: TopMut::P4 { interface, external },
            depth_next,
        };
        // Run spec-meta in the same arena and effect count
        call(&mut MetaCall { arena, native, levels, effects })
    }
}

impl<Ext: Extern> MetaCall<'_, '_, Ext> {
    /// Calls relation `name` of the specification at `floor`.
    fn call_rel(
        &mut self,
        floor: Floor,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        let level = match floor {
            // The base runs spec-meta natively
            Floor::Base => {
                let Self { arena, native, levels, effects } = self;
                return native.call_rel(arena, levels, effects, name, values).result;
            }
            // A host level runs through spec-meta at the level below
            Floor::Host(level) => level,
        };
        // Call_rel(C, name, values) : relRes, on the level below
        let value_ctx = self.load_ctx(level)?;
        let id = phrase!(node: name.into(), span: Span::default());
        let [value_id, value_args] =
            codec::encode::rel_args(self.arena, self.levels.cache(level), &id, values)?;
        let values_res =
            self.call_rel(level.below(), "Call_rel", &[value_ctx, value_id, value_args])?;
        let value_res = *value::get::one(&values_res).map_err(ExternError::from)?;
        // OK outputs, UNMATCH a mismatch, ERROR a fatal failure
        codec::decode::rel_result(self.arena, self.levels.cache(level), value_res, name)
    }

    /// Calls function `name` of the specification at `floor`.
    fn call_func(
        &mut self,
        floor: Floor,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        let level = match floor {
            // The base runs spec-meta natively
            Floor::Base => {
                let Self { arena, native, levels, effects } = self;
                return native
                    .call_func(arena, levels, effects, name, targs, values)
                    .result;
            }
            // A host level runs through spec-meta at the level below
            Floor::Host(level) => level,
        };
        // Call_func(C, name, targs, values) : funcRes, on the level below
        let value_ctx = self.load_ctx(level)?;
        let id = phrase!(node: name.into(), span: Span::default());
        let [value_id, value_targs, value_args] =
            codec::encode::func_args(self.arena, self.levels.cache(level), &id, targs, values)?;
        let values_res = self.call_rel(
            level.below(),
            "Call_func",
            &[value_ctx, value_id, value_targs, value_args],
        )?;
        let value_res = *value::get::one(&values_res).map_err(ExternError::from)?;
        // OK the result, UNMATCH a mismatch, ERROR a fatal failure
        codec::decode::func_result(self.arena, self.levels.cache(level), value_res, name)
    }

    /// Returns C of `level`, loading it into the level below on first use.
    fn load_ctx(&mut self, level: HostLevel) -> Result<Value, InterpreterError> {
        if let Some(value_ctx) = self.levels.ctx(level) {
            return Ok(value_ctx);
        }
        // Encode the specification as AL, converting IL through spec-meta below
        let value_spec_al = match self.levels.env.spec(level) {
            Level::AboveAl { spec_al, .. } => spec_al
                .encode(self.arena)
                .map_err(|error| InterpreterError::Fatal(error.into_report()))?,
            Level::AboveIl { spec_il, .. } => {
                let value_spec_il = spec_il
                    .encode(self.arena)
                    .map_err(|error| InterpreterError::Fatal(error.into_report()))?;
                let values = self.call_rel(level.below(), "Algo_IL", &[value_spec_il])?;
                *value::get::one(&values).map_err(ExternError::from)?
            }
            Level::Base { .. } => unreachable!("the base is never loaded by a level below"),
        };
        // C = $load_ctx($empty_ctx, spec), on the level below
        let value_ctx_empty = self.call_func(level.below(), "empty_ctx", &[], &[])?;
        let value_ctx =
            self.call_func(level.below(), "load_ctx", &[], &[value_ctx_empty, value_spec_al])?;
        self.levels.set_ctx(level, value_ctx);
        Ok(value_ctx)
    }
}

impl MetaEnv<'_> {
    /// Returns the specification of `level`, in the form the level below loads.
    fn spec(&self, level: HostLevel) -> &Level {
        &self.levels[level.number()]
    }
}

impl<'s, Ext: Extern> Interpreter<P4Interface, Ext> for MetaInterp<'s> {
    type SpecEnv = MetaEnv<'s>;

    fn clear(&mut self) {
        self.native.clear::<Ext>();
        *self.depth_next = None;
        self.caches.iter_mut().for_each(Cache::clear);
    }

    fn reset(&mut self) {
        self.native.reset::<Ext>();
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
        Self::with_meta(ctx, |call| {
            let level_top = HostLevel::top(call.levels.env.levels);
            call.call_rel(Floor::Host(level_top), name, values)
        })
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        Self::with_meta(ctx, |call| {
            let level_top = HostLevel::top(call.levels.env.levels);
            call.call_func(Floor::Host(level_top), name, targs, values)
        })
    }
}
