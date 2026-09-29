//! Structured-language execution over the composed runner context
//!
//! `SlInterp` runs SL definitions:
//! a call binds its inputs into a frame and evaluates the body block;
//! instructions run in order and a continuing one falls through to the next,
//! and an otherwise block catches a body that fell through entirely.
//! Tail calls loop inside `call` instead of recursing.
//! `Config` toggles memoization, determinism checks, and call-boundary guards.

pub mod context;
pub mod flow;

pub mod eval;

use crate::interp::shared::{backtrack::Failure, cache::Cache, eval::Invoker};
use crate::{
    lang::{common::source::Span, data::value::Value, sl::ast},
    runner::{Extern, Interface, Interpreter, RunnerContext},
};
use context::{Context, Global};

/// Configuration for the SL interpreter.
pub struct Config {
    /// Memoize pure calls.
    cache: bool,
    /// Run every instruction of a block and reject two terminating ones.
    det: bool,
    /// Check argument and result types at call boundaries.
    guard: bool,
}

impl Config {
    pub fn new(cache: bool, det: bool, guard: bool) -> Self {
        Self { cache, det, guard }
    }
}

/// The SL interpreter: configuration plus the call cache.
pub struct SlInterp {
    config: Config,
    cache: Cache,
}

impl SlInterp {
    pub fn new(config: Config) -> Self {
        Self { config, cache: Cache::default() }
    }
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for SlInterp {
    type Spec = Global;
    type Error = Failure;

    fn clear(&mut self) {
        self.cache.clear();
    }

    fn reset(&mut self) {
        self.cache = Cache::default();
    }

    fn eval_program(
        runner_ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        program: Value,
    ) -> Result<Vec<Value>, Failure> {
        runner_ctx.call_rel(name, &[program])
    }

    fn eval_rel(
        runner_ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, Failure> {
        // Public entries start from a fresh cache
        runner_ctx.interp_mut().cache.clear();
        let id = crate::phrase!(node: name.to_owned(), span: Span::default());
        let ctx = Context::new(runner_ctx.spec());
        // Guard the inputs unless the call would be served from the cache
        if runner_ctx.interp().config.guard && !eval::call::cache_rel(runner_ctx, &ctx, &id) {
            eval::call::check_rel_inputs(runner_ctx.arena(), &ctx, &id, values)?;
        }
        Self::invoke_rel(runner_ctx, &ctx, &id, values)
    }

    fn eval_func(
        runner_ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        targs: &[ast::Typ],
        values: &[Value],
    ) -> Result<Value, Failure> {
        // Public entries start from a fresh cache
        runner_ctx.interp_mut().cache.clear();
        let id = crate::phrase!(node: name.to_owned(), span: Span::default());
        let ctx = Context::new(runner_ctx.spec());
        // Guard the inputs unless the call would be served from the cache
        if runner_ctx.interp().config.guard
            && !eval::call::cache_func(runner_ctx, &ctx, &id, values)
        {
            eval::call::check_func_inputs(runner_ctx.arena(), &ctx, &id, targs, values)?;
        }
        Self::invoke_func(runner_ctx, &ctx, &id, targs, values)
    }
}
