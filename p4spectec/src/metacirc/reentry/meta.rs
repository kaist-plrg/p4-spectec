//! The P4 level's interpreter at one reentry depth, run through spec-meta
//!
//! For a P4 call `eval_rel(V1Model_parser, values)`, `MetaInterp` loads
//! `C = $load_ctx($empty_ctx, <P4 spec>)` once per program
//! and runs `Call_rel(C, "V1Model_parser", values)` on its depth's SL interpreter.
//! spec-meta's host calls relay to the P4 host the caller lent (`relay::lent`);
//! a reentry from that host runs on the next `MetaDepth`.
//! Depths share spec-meta, C, and the codec cache;
//! each depth owns its SL memo and its meta builtins.

use super::super::{
    relay::{RelayExtern, codec, lent::LentHost},
    tower::Level,
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
    next: Option<Box<MetaDepth>>,
}

impl MetaDepth {
    pub(in crate::metacirc) fn new(options: runner::Config) -> Self {
        Self {
            interp: SlInterp::new(SlConfig::new(options.cache, options.det, options.guard)),
            // Only the builtins are used; spec-meta never parses at a depth
            interface: interface::meta(Lang::Al),
            next: None,
        }
    }

    /// Lends this depth as the P4 level's interpreter.
    pub(in crate::metacirc) fn lend<'d>(
        &'d mut self,
        ctx_target: &'d mut Option<Value>,
        cache: &'d mut Cache,
    ) -> MetaInterp<'d> {
        MetaInterp {
            interp: &mut self.interp,
            interface: &mut self.interface,
            depth_next: &mut self.next,
            ctx_target,
            cache,
        }
    }
}

/// Definitions shared by every depth.
pub(in crate::metacirc) struct MetaEnv<'s> {
    /// spec-meta, executed natively.
    pub spec_meta: &'s SlSpecEnv,
    /// The P4 specification, in the form spec-meta loads.
    pub target: &'s Level,
    /// Options for the interpreters of deeper levels.
    pub options: runner::Config,
}

/// The P4 level's interpreter at one depth, borrowing that depth's state.
pub(in crate::metacirc) struct MetaInterp<'s> {
    pub interp: &'s mut SlInterp,
    pub interface: &'s mut MetaInterface,
    /// The depths beneath this one, which answer reentry.
    pub depth_next: &'s mut Option<Box<MetaDepth>>,
    /// C for the current program, shared by every depth.
    pub ctx_target: &'s mut Option<Value>,
    /// The codec cache, shared by every depth.
    pub cache: &'s mut Cache,
}

// == Calls into spec-meta
//
//   eval_rel(ctx, "V1Model_parser", vals)        ctx.interp: depth d
//     with_meta                                  P4 host -> LentHost relay
//       load_ctx_target -> C                     once per program
//         $load_ctx($empty_ctx, specAL)          specAL = Algo_IL(specIL)
//                                                on an IL level
//       Call_rel(C, "V1Model_parser", vals)      on depth d's SL interp
//         Call_extern_rel(..) -> LentHost
//           Ext::eval_rel(ctx', ..)              ctx'.interp: depth d+1
//             ctx'.call_func("find_var_e", ..)   reentry with the same C
//       relRes -> Ok | Mismatch | Fatal

/// spec-meta's extern while it runs one P4 call.
type LentRelay<'h, 's, Ext> = RelayExtern<&'h mut Cache, LentHost<'h, 's, Ext>>;

/// The spec-meta context of one P4 call.
type MetaContext<'r, 'h, 's, Ext> =
    RunnerContext<'r, SlInterp, MetaInterface, LentRelay<'h, 's, Ext>>;

impl<'s> MetaInterp<'s> {
    /// Runs `call` on spec-meta at this depth with C,
    /// relaying its host calls to the P4 host of `ctx`.
    fn with_meta<Ext: Extern, T>(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        call: impl FnOnce(&mut MetaContext<'_, '_, 's, Ext>, Value) -> Result<T, InterpreterError>,
    ) -> Result<T, InterpreterError> {
        // Split the P4 context: the host goes to the relay, the depth runs spec-meta
        let RunnerContextParts { arena, spec_env: env, interp: meta, interface, external, effects } =
            ctx.parts_mut();
        let MetaInterp { interp, interface: interface_meta, depth_next, ctx_target, cache } = meta;
        let mut relay = RelayExtern {
            cache: &mut **cache,
            layer_next: LentHost { env, interface, external, depth_next, ctx_target },
        };
        // Run spec-meta in the same arena and effect count
        let mut ctx_meta = RunnerContext::new(
            arena,
            env.spec_meta,
            &mut **interp,
            &mut **interface_meta,
            &mut relay,
            effects,
        );
        let value_ctx = Self::load_ctx_target(&mut ctx_meta)?;
        call(&mut ctx_meta, value_ctx)
    }

    /// Returns C, loading the P4 specification into spec-meta on first use.
    fn load_ctx_target<Ext: Extern>(
        ctx_meta: &mut MetaContext<'_, '_, '_, Ext>,
    ) -> Result<Value, InterpreterError> {
        if let Some(value_ctx) = *ctx_meta.external().layer_next.ctx_target {
            return Ok(value_ctx);
        }
        // Encode the specification as AL, converting IL through spec-meta
        let env = ctx_meta.external().layer_next.env;
        let value_spec_al = match env.target {
            Level::AboveAl { spec_al, .. } => spec_al
                .encode(ctx_meta.arena_mut())
                .map_err(|error| InterpreterError::Fatal(error.into_report()))?,
            Level::AboveIl { spec_il, .. } => {
                let value_spec_il = spec_il
                    .encode(ctx_meta.arena_mut())
                    .map_err(|error| InterpreterError::Fatal(error.into_report()))?;
                let values = ctx_meta.call_rel("Algo_IL", &[value_spec_il]).result?;
                *value::get::one(&values).map_err(ExternError::from)?
            }
            Level::Base { .. } => unreachable!("the base is never loaded by a level below"),
        };
        // C = $load_ctx($empty_ctx, spec)
        let value_ctx_empty = ctx_meta.call_func("empty_ctx", &[], &[]).result?;
        let value_ctx = ctx_meta
            .call_func("load_ctx", &[], &[value_ctx_empty, value_spec_al])
            .result?;
        *ctx_meta.external_mut().layer_next.ctx_target = Some(value_ctx);
        Ok(value_ctx)
    }
}

impl<'s, Ext: Extern> Interpreter<P4Interface, Ext> for MetaInterp<'s> {
    type SpecEnv = MetaEnv<'s>;

    fn clear(&mut self) {
        Interpreter::<MetaInterface, LentRelay<'_, 's, Ext>>::clear(&mut *self.interp);
        *self.depth_next = None;
        self.cache.clear();
    }

    fn reset(&mut self) {
        Interpreter::<MetaInterface, LentRelay<'_, 's, Ext>>::reset(&mut *self.interp);
        self.interface.clear();
        *self.depth_next = None;
        *self.ctx_target = None;
        self.cache.clear();
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
        Self::with_meta(ctx, |ctx_meta, value_ctx| {
            // Call_rel(C, name, values) : relRes
            let id = phrase!(node: name.into(), span: Span::default());
            let (arena, relay) = ctx_meta.arena_and_external_mut();
            let [value_id, value_args] = codec::encode::rel_args(arena, relay.cache, &id, values)?;
            let values_res = ctx_meta
                .call_rel("Call_rel", &[value_ctx, value_id, value_args])
                .result?;
            let value_res = *value::get::one(&values_res).map_err(ExternError::from)?;
            // OK outputs, UNMATCH a mismatch, ERROR a fatal failure
            let (arena, relay) = ctx_meta.arena_and_external_mut();
            codec::decode::rel_result(arena, relay.cache, value_res, name)
        })
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, P4Interface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        Self::with_meta(ctx, |ctx_meta, value_ctx| {
            // Call_func(C, name, targs, values) : funcRes
            let id = phrase!(node: name.into(), span: Span::default());
            let (arena, relay) = ctx_meta.arena_and_external_mut();
            let [value_id, value_targs, value_args] =
                codec::encode::func_args(arena, relay.cache, &id, targs, values)?;
            let values_res = ctx_meta
                .call_rel("Call_func", &[value_ctx, value_id, value_targs, value_args])
                .result?;
            let value_res = *value::get::one(&values_res).map_err(ExternError::from)?;
            // OK the result, UNMATCH a mismatch, ERROR a fatal failure
            let (arena, relay) = ctx_meta.arena_and_external_mut();
            codec::decode::func_result(arena, relay.cache, value_res, name)
        })
    }
}
