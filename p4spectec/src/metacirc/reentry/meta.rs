//! The P4 level's interpreter at one reentry depth, run through spec-meta
//!
//! For a P4 call `eval_rel(V1Model_parser, values)` on a tower of n levels,
//! `MetaInterp` calls the relation at the top `HostLevel`:
//! each level above the base runs as `Call_rel(C, name, values)`
//! on spec-meta at the level below, down to the depth's SL interpreter,
//! where C loads that level's specification once per program.
//! spec-meta's host calls relay up through the levels the caller lent
//! (`relay::lent`); a reentry from the P4 host runs on the next `MetaDepth`.
//! Depths share spec-meta, each level's C, and the codec caches;
//! each depth owns its SL memo and its meta builtins.

use super::super::{
    relay::{codec, lent::LentLevels},
    tower::{HostLevel, Level},
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
    interp::sl::{Config as SlConfig, SlInterp, context::SpecEnv as SlSpecEnv},
    lang::{
        common::source::Span,
        data::value::flat::{self as value, Value},
        il::ast::Typ,
    },
    phrase,
    runner::{
        self, Extern, ExternError, Interface, Interpreter, InterpreterError, RunnerContext,
        RunnerContextParts,
    },
};

// == Depth state

/// The interpreter state of one reentry depth beneath the base.
pub(in crate::metacirc) struct MetaDepth {
    interp: SlInterp,
    interface: MetaInterface,
    /// The meta builtins of each level between the base and the top.
    interfaces: Vec<MetaInterface>,
    next: Option<Box<MetaDepth>>,
}

impl MetaDepth {
    /// Makes a depth with builtins for `count` levels between base and top.
    pub(in crate::metacirc) fn new(options: runner::Config, count: usize) -> Self {
        Self {
            interp: SlInterp::new(SlConfig::new(options.cache, options.det, options.guard)),
            // Only the builtins are used; spec-meta never parses at a depth
            interface: interface::meta(Lang::Al),
            interfaces: (0..count).map(|_| interface::meta(Lang::Al)).collect(),
            next: None,
        }
    }

    /// Lends this depth as the P4 level's interpreter.
    pub(in crate::metacirc) fn lend<'d>(
        &'d mut self,
        ctx_targets: &'d mut [Option<Value>],
        caches: &'d mut [Cache],
    ) -> MetaInterp<'d> {
        MetaInterp {
            interp: &mut self.interp,
            interface: &mut self.interface,
            interfaces: self.interfaces.iter_mut().collect(),
            depth_next: &mut self.next,
            ctx_targets,
            caches,
        }
    }
}

/// Definitions shared by every depth.
pub(in crate::metacirc) struct MetaEnv<'s> {
    /// spec-meta, executed natively.
    pub spec_meta: &'s SlSpecEnv,
    /// Every level of the tower, from the base to the P4 top.
    pub levels: &'s [Level],
    /// Options for the interpreters of deeper levels.
    pub options: runner::Config,
}

/// The P4 level's interpreter at one depth, borrowing that depth's state.
pub(in crate::metacirc) struct MetaInterp<'s> {
    pub interp: &'s mut SlInterp,
    pub interface: &'s mut MetaInterface,
    /// The meta builtins of each level between the base and the top.
    pub interfaces: Vec<&'s mut MetaInterface>,
    /// The depths beneath this one, which answer reentry.
    pub depth_next: &'s mut Option<Box<MetaDepth>>,
    /// C of each level above the base, shared by every depth.
    pub ctx_targets: &'s mut [Option<Value>],
    /// The codec cache of each level boundary, shared by every depth.
    pub caches: &'s mut [Cache],
}

// == Calls into spec-meta
//
//   eval_rel(ctx, "V1Model_parser", vals)        ctx.interp: depth d
//     with_meta                                  P4 host -> LentLevels
//       call_rel(top, "V1Model_parser", vals)    on the top level
//         load_ctx(top) -> C                     once per program
//         call_rel(top.below(), "Call_rel",      one level down, until
//                  [C, "V1Model_parser", vals])  the base runs natively
//           Call_extern_rel(..) -> LentLevels    up each boundary
//             Ext::eval_rel(ctx', ..)            ctx'.interp: depth d+1
//               ctx'.call_func("find_var_e")     reentry with the same C
//         relRes -> Ok | Mismatch | Fatal

/// The spec-meta context of one P4 call.
type MetaContext<'r, 'h, 's, Ext> =
    RunnerContext<'r, SlInterp, MetaInterface, LentLevels<'h, 's, Ext>>;

impl<'s> MetaInterp<'s> {
    /// Runs `call` on spec-meta at this depth,
    /// relaying its host calls up to the P4 host of `ctx`.
    fn with_meta<Ext: Extern, T>(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        call: impl FnOnce(&mut MetaContext<'_, '_, 's, Ext>) -> Result<T, InterpreterError>,
    ) -> Result<T, InterpreterError> {
        // Split the P4 context: the host goes to the lent levels, the depth runs spec-meta
        let RunnerContextParts { arena, spec_env: env, interp: meta, interface, external, effects } =
            ctx.parts_mut();
        let MetaInterp {
            interp,
            interface: interface_meta,
            interfaces,
            depth_next,
            ctx_targets,
            caches,
        } = meta;
        let mut levels = LentLevels {
            env,
            interfaces,
            interface_top: interface,
            external_top: external,
            depth_next,
            ctx_targets,
            caches,
        };
        // Run spec-meta in the same arena and effect count
        let mut ctx_meta = RunnerContext::new(
            arena,
            env.spec_meta,
            &mut **interp,
            &mut **interface_meta,
            &mut levels,
            effects,
        );
        call(&mut ctx_meta)
    }

    /// Calls relation `name` of `level`, or of the base if `None`.
    fn call_rel<Ext: Extern>(
        ctx_meta: &mut MetaContext<'_, '_, '_, Ext>,
        level: Option<HostLevel>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        // The base runs natively
        let Some(level) = level else {
            return ctx_meta.call_rel(name, values).result;
        };
        // Call_rel(C, name, values) : relRes, on the level below
        let value_ctx = Self::load_ctx(ctx_meta, level)?;
        let id = phrase!(node: name.into(), span: Span::default());
        let (arena, levels) = ctx_meta.arena_and_external_mut();
        let [value_id, value_args] =
            codec::encode::rel_args(arena, levels.cache(level), &id, values)?;
        let values_res = Self::call_rel(
            ctx_meta,
            level.below(),
            "Call_rel",
            &[value_ctx, value_id, value_args],
        )?;
        let value_res = *value::get::one(&values_res).map_err(ExternError::from)?;
        // OK outputs, UNMATCH a mismatch, ERROR a fatal failure
        let (arena, levels) = ctx_meta.arena_and_external_mut();
        codec::decode::rel_result(arena, levels.cache(level), value_res, name)
    }

    /// Calls function `name` of `level`, or of the base if `None`.
    fn call_func<Ext: Extern>(
        ctx_meta: &mut MetaContext<'_, '_, '_, Ext>,
        level: Option<HostLevel>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        // The base runs natively
        let Some(level) = level else {
            return ctx_meta.call_func(name, targs, values).result;
        };
        // Call_func(C, name, targs, values) : funcRes, on the level below
        let value_ctx = Self::load_ctx(ctx_meta, level)?;
        let id = phrase!(node: name.into(), span: Span::default());
        let (arena, levels) = ctx_meta.arena_and_external_mut();
        let [value_id, value_targs, value_args] =
            codec::encode::func_args(arena, levels.cache(level), &id, targs, values)?;
        let values_res = Self::call_rel(
            ctx_meta,
            level.below(),
            "Call_func",
            &[value_ctx, value_id, value_targs, value_args],
        )?;
        let value_res = *value::get::one(&values_res).map_err(ExternError::from)?;
        // OK the result, UNMATCH a mismatch, ERROR a fatal failure
        let (arena, levels) = ctx_meta.arena_and_external_mut();
        codec::decode::func_result(arena, levels.cache(level), value_res, name)
    }

    /// Returns C of `level`, loading it into the level below on first use.
    fn load_ctx<Ext: Extern>(
        ctx_meta: &mut MetaContext<'_, '_, '_, Ext>,
        level: HostLevel,
    ) -> Result<Value, InterpreterError> {
        if let Some(value_ctx) = ctx_meta.external().ctx(level) {
            return Ok(value_ctx);
        }
        // Encode the specification as AL, converting IL through spec-meta below
        let env = ctx_meta.external().env;
        let value_spec_al = match env.spec(level) {
            Level::AboveAl { spec_al, .. } => spec_al
                .encode(ctx_meta.arena_mut())
                .map_err(|error| InterpreterError::Fatal(error.into_report()))?,
            Level::AboveIl { spec_il, .. } => {
                let value_spec_il = spec_il
                    .encode(ctx_meta.arena_mut())
                    .map_err(|error| InterpreterError::Fatal(error.into_report()))?;
                let values = Self::call_rel(ctx_meta, level.below(), "Algo_IL", &[value_spec_il])?;
                *value::get::one(&values).map_err(ExternError::from)?
            }
            Level::Base { .. } => unreachable!("the base is never loaded by a level below"),
        };
        // C = $load_ctx($empty_ctx, spec), on the level below
        let value_ctx_empty = Self::call_func(ctx_meta, level.below(), "empty_ctx", &[], &[])?;
        let value_ctx = Self::call_func(
            ctx_meta,
            level.below(),
            "load_ctx",
            &[],
            &[value_ctx_empty, value_spec_al],
        )?;
        ctx_meta.external_mut().set_ctx(level, value_ctx);
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
        Interpreter::<MetaInterface, LentLevels<'_, 's, Ext>>::clear(&mut *self.interp);
        *self.depth_next = None;
        self.caches.iter_mut().for_each(Cache::clear);
    }

    fn reset(&mut self) {
        Interpreter::<MetaInterface, LentLevels<'_, 's, Ext>>::reset(&mut *self.interp);
        self.interface.clear();
        self.interfaces
            .iter_mut()
            .for_each(|interface| interface.clear());
        *self.depth_next = None;
        self.ctx_targets.fill(None);
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
        Self::with_meta(ctx, |ctx_meta| {
            let level_top = HostLevel::top(ctx_meta.external().env.levels);
            Self::call_rel(ctx_meta, Some(level_top), name, values)
        })
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        Self::with_meta(ctx, |ctx_meta| {
            let level_top = HostLevel::top(ctx_meta.external().env.levels);
            Self::call_func(ctx_meta, Some(level_top), name, targs, values)
        })
    }
}
