//! A runner backend that executes the target through the meta specification
//!
//! For a P4 call `eval_rel(V1Model_parser, values)`, `MetaInterp` loads
//! `C = $load($empty_ctx, <P4 spec>)` once per program and runs
//! `Call_rel(C, "V1Model_parser", values)` on the SL executor of spec-meta.
//! When spec-meta reaches `Call_extern_rel` (e.g. a packet extract),
//! `MetaBridge` decodes it and calls the outer simulator with a P4-level context;
//! a reentry from the simulator runs `Call_rel` on the next `MetaLevel`.
//! Levels share spec-meta, C, the codec cache, and the effect count;
//! each level owns its executor memo and its AL builtin state.

use super::codec;
use crate::{
    interface::{
        self as builtin,
        al::{AlInterface, Cache, encode::Encode},
    },
    interp::sl::{Config as SlConfig, SlInterp, context::SpecEnv as SlSpecEnv},
    lang::{
        al,
        common::source::Span,
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
        sl,
    },
    phrase,
    runner::{
        self, BuildError, Eval, Extern, ExternError, Interface, Interpreter, InterpreterError,
        RunnerContext, SideEffect,
    },
};

// == Backend state

/// The loaded meta specification and the target it interprets.
pub struct MetaEnv {
    /// spec-meta, executed natively and shared by every level.
    base: SlSpecEnv,
    /// The target specification, encoded into C on the first call of a program.
    target: al::ast::Spec,
    /// Options for executors created at reentry.
    config: runner::Config,
}

/// Program-owned state of the meta backend.
pub struct MetaState {
    /// The executor for calls from the outer runner.
    level: MetaLevel,
    /// Conversions across the meta boundary, shared by every level.
    cache: Cache,
    /// C for the current program; arena values do not survive a reset.
    ctx_target: Option<Value>,
}

/// The executor for one reentry depth; a deeper reentry uses `child`.
struct MetaLevel {
    interp: SlInterp,
    interface: AlInterface,
    child: Option<Box<MetaLevel>>,
}

/// A borrowed view of the backend state at one level.
pub struct MetaInterp<'s> {
    level: &'s mut MetaLevel,
    cache: &'s mut Cache,
    ctx_target: &'s mut Option<Value>,
}

/// The extern of spec-meta's executor, answering through the outer runner.
struct MetaBridge<'b, Iface, Ext> {
    env: &'b MetaEnv,
    interface: &'b mut Iface,
    external: &'b mut Ext,
    /// The executor that runs a reentry from `external`.
    child: &'b mut Option<Box<MetaLevel>>,
    cache: &'b mut Cache,
    ctx_target: &'b mut Option<Value>,
}

impl MetaEnv {
    /// Loads spec-meta for native execution next to the target it interprets.
    pub(crate) fn new(
        arena: &mut Arena,
        base: sl::ast::Spec,
        target: al::ast::Spec,
        config: runner::Config,
    ) -> Result<Self, BuildError> {
        let base = SlSpecEnv::load(&mut arena.mixop, config.det, base)?;
        Ok(Self { base, target, config })
    }
}

impl MetaState {
    pub(crate) fn new(config: runner::Config) -> Self {
        Self { level: MetaLevel::new(config), cache: Cache::new(config.cache), ctx_target: None }
    }

    pub(crate) fn view(&mut self) -> MetaInterp<'_> {
        MetaInterp {
            level: &mut self.level,
            cache: &mut self.cache,
            ctx_target: &mut self.ctx_target,
        }
    }
}

impl MetaLevel {
    fn new(config: runner::Config) -> Self {
        Self {
            interp: SlInterp::new(SlConfig::new(config.cache, config.det, config.guard)),
            interface: builtin::al(),
            child: None,
        }
    }
}

// == Calls into the meta specification

/// The spec-meta context of one call: the executor at one level and its bridge.
type BaseContext<'r, 'b, Iface, Ext> =
    RunnerContext<'r, SlInterp, AlInterface, MetaBridge<'b, Iface, Ext>>;

impl<'s> MetaInterp<'s> {
    /// Runs one spec-meta call on this view's level, bridging its host calls.
    ///
    /// `call` receives C and the spec-meta context; effects stay in `effects`.
    fn with_base<Iface, Ext, T>(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        call: impl FnOnce(&mut BaseContext<'_, '_, Iface, Ext>, Value) -> Result<T, InterpreterError>,
    ) -> Result<T, InterpreterError>
    where
        Iface: Interface,
        Ext: Extern,
    {
        // Split the outer context; the bridge takes its interface and extern
        let parts = ctx.parts_mut();
        let env = parts.spec_env;
        let MetaInterp { level, cache, ctx_target: ctx_target_cached } = parts.interp;
        let MetaLevel { interp, interface, child } = &mut **level;
        let mut bridge = MetaBridge {
            env,
            interface: parts.interface,
            external: parts.external,
            child,
            cache,
            ctx_target: ctx_target_cached,
        };
        // Run spec-meta on this level's executor with the same arena and effects
        let mut ctx_base = RunnerContext::new(
            parts.arena,
            &env.base,
            interp,
            interface,
            &mut bridge,
            parts.effects,
        );
        let value_ctx = ctx_target(&mut ctx_base)?;
        call(&mut ctx_base, value_ctx)
    }
}

/// Returns C, loading the target into an empty spec-meta context on first use.
fn ctx_target<Iface: Interface, Ext: Extern>(
    ctx_base: &mut BaseContext<'_, '_, Iface, Ext>,
) -> Result<Value, InterpreterError> {
    if let Some(value_ctx) = *ctx_base.external().ctx_target {
        return Ok(value_ctx);
    }
    // Encode the target in the arena of the current program
    let (arena, bridge) = ctx_base.arena_and_external_mut();
    let value_spec = bridge
        .env
        .target
        .encode(arena)
        .map_err(|error| InterpreterError::Fatal(error.into_report()))?;
    // C = $load($empty_ctx, spec)
    let value_ctx_empty = ctx_base.call_func("empty_ctx", &[], &[]).result?;
    let value_ctx = ctx_base
        .call_func("load", &[], &[value_ctx_empty, value_spec])
        .result?;
    *ctx_base.external_mut().ctx_target = Some(value_ctx);
    Ok(value_ctx)
}

/// Takes the single relRes or funcRes output of a spec-meta call relation.
fn output_one(values: Vec<Value>) -> Result<Value, InterpreterError> {
    match values[..] {
        [value] => Ok(value),
        _ => Err(ExternError::diagnostic_message("meta call relation must have one output").into()),
    }
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for MetaInterp<'_> {
    type SpecEnv = MetaEnv;

    fn clear(&mut self) {
        Interpreter::<AlInterface, MetaBridge<'_, Iface, Ext>>::clear(&mut self.level.interp);
        self.level.child = None;
        self.cache.clear();
    }

    fn reset(&mut self) {
        Interpreter::<AlInterface, MetaBridge<'_, Iface, Ext>>::reset(&mut self.level.interp);
        self.level.interface.clear();
        self.level.child = None;
        self.cache.clear();
        *self.ctx_target = None;
    }

    fn eval_program(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        program: Value,
    ) -> Result<Vec<Value>, InterpreterError> {
        Self::eval_rel(ctx, name, &[program])
    }

    fn eval_rel(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        Self::with_base(ctx, |ctx_base, value_ctx| {
            // Call_rel(C, name, values) : relRes
            let id = phrase!(node: name.into(), span: Span::default());
            let (arena, bridge) = ctx_base.arena_and_external_mut();
            let [value_id, value_args] = codec::encode::rel_args(arena, bridge.cache, &id, values)?;
            let values_res = ctx_base
                .call_rel("Call_rel", &[value_ctx, value_id, value_args])
                .result?;
            let value_res = output_one(values_res)?;
            let (arena, bridge) = ctx_base.arena_and_external_mut();
            codec::decode::rel_result(arena, bridge.cache, value_res, name)
        })
    }

    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        Self::with_base(ctx, |ctx_base, value_ctx| {
            // Call_func(C, name, targs, values) : funcRes
            let id = phrase!(node: name.into(), span: Span::default());
            let (arena, bridge) = ctx_base.arena_and_external_mut();
            let [value_id, value_targs, value_args] =
                codec::encode::func_args(arena, bridge.cache, &id, targs, values)?;
            let values_res = ctx_base
                .call_rel("Call_func", &[value_ctx, value_id, value_targs, value_args])
                .result?;
            let value_res = output_one(values_res)?;
            let (arena, bridge) = ctx_base.arena_and_external_mut();
            codec::decode::func_result(arena, bridge.cache, value_res, name)
        })
    }
}

// == Host calls from the meta specification

/// Rejects an operation outside the host boundary's contract.
fn unsupported<T>(name: &str) -> Eval<T, ExternError> {
    Eval::new(
        Err(ExternError::diagnostic_message(format!("unsupported meta extern: {name}"))),
        SideEffect::Pure,
    )
}

impl<Iface: Interface, Ext: Extern> Extern for MetaBridge<'_, Iface, Ext> {
    /// Answers spec-meta's host relations through the outer interface and extern.
    ///
    /// The calling executor is not used: a reentry from the outer extern
    /// runs on the next level, so this works under any base interpreter.
    fn eval_rel<Interp, IfaceBase>(
        ctx: &mut RunnerContext<'_, Interp, IfaceBase, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        IfaceBase: Interface,
        Interp: Interpreter<IfaceBase, Self>,
    {
        let parts = ctx.parts_mut();
        let bridge = parts.external;
        // The next level runs reentries; it lives until the program is reset
        let config = bridge.env.config;
        let level = bridge
            .child
            .get_or_insert_with(|| Box::new(MetaLevel::new(config)));
        let mut view =
            MetaInterp { level, cache: &mut *bridge.cache, ctx_target: &mut *bridge.ctx_target };
        // Each call answers through a P4-level context over the outer components
        match name {
            // Extern relations of the target, such as packet extraction
            "Call_extern_rel" => {
                let (id, values) = match codec::decode::rel_args(parts.arena, view.cache, values) {
                    Ok(args) => args,
                    Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                };
                let mut ctx_outer = RunnerContext::new(
                    parts.arena,
                    bridge.env,
                    &mut view,
                    bridge.interface,
                    bridge.external,
                    parts.effects,
                );
                let eval = Ext::eval_rel(&mut ctx_outer, &id.node, &values)
                    .map_err(InterpreterError::from);
                codec::encode::rel_result(parts.arena, view.cache, eval)
            }
            // Extern functions of the target, such as object initialization
            "Call_extern_func" => {
                let (id, targs, values) =
                    match codec::decode::func_args(parts.arena, view.cache, values) {
                        Ok(args) => args,
                        Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                    };
                let mut ctx_outer = RunnerContext::new(
                    parts.arena,
                    bridge.env,
                    &mut view,
                    bridge.interface,
                    bridge.external,
                    parts.effects,
                );
                let eval = Ext::eval_func(&mut ctx_outer, &id.node, &targs, &values)
                    .map_err(InterpreterError::from);
                codec::encode::func_result(parts.arena, view.cache, eval)
            }
            // Builtins of the target, such as print_ and fresh_typeId
            "Call_builtin_func" => {
                let (id, targs, values) =
                    match codec::decode::func_args(parts.arena, view.cache, values) {
                        Ok(args) => args,
                        Err(error) => return Eval::new(Err(error), SideEffect::Pure),
                    };
                let mut ctx_outer = RunnerContext::new(
                    parts.arena,
                    bridge.env,
                    &mut view,
                    bridge.interface,
                    bridge.external,
                    parts.effects,
                );
                let eval = Iface::call_builtin(&mut ctx_outer, &id, &targs, &values)
                    .map_err(InterpreterError::from);
                codec::encode::func_result(parts.arena, view.cache, eval)
            }
            // spec-meta declares no other host relations
            _ => unsupported(name),
        }
    }

    fn eval_func<Interp, IfaceBase>(
        _ctx: &mut RunnerContext<'_, Interp, IfaceBase, Self>,
        name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        IfaceBase: Interface,
        Interp: Interpreter<IfaceBase, Self>,
    {
        unsupported(name)
    }

    fn clear(&mut self) {}
}
