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

use crate::lang::{common::source::Span, data::value::Value};

use crate::lang::sl::ast;

use crate::runner::{Extern, Interface, Interpreter, InterpreterError, RunnerContext};

use crate::interp::shared::{cache::Cache, eval::Invoker};

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
    ) -> Result<Vec<Value>, InterpreterError> {
        runner_ctx.call_rel(name, &[program])
    }

    fn eval_rel(
        runner_ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        // Public entries start from a fresh cache
        runner_ctx.interp_mut().cache.clear();
        let id = crate::phrase!(node: name.to_owned(), span: Span::default());
        let ctx = Context::new(runner_ctx.spec());
        // Check the caller's inputs before running the definition
        eval::call::check_rel_inputs(
            runner_ctx.arena(),
            &ctx,
            &id,
            values,
            runner_ctx.interp().config.guard,
        )?;
        Self::invoke_rel(runner_ctx, &ctx, &id, values)
    }

    fn eval_func(
        runner_ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        targs: &[ast::Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        // Public entries start from a fresh cache
        runner_ctx.interp_mut().cache.clear();
        let id = crate::phrase!(node: name.to_owned(), span: Span::default());
        let ctx = Context::new(runner_ctx.spec());
        // Check the caller's inputs before running the definition
        eval::call::check_func_inputs(
            runner_ctx.arena(),
            &ctx,
            &id,
            targs,
            values,
            runner_ctx.interp().config.guard,
        )?;
        Self::invoke_func(runner_ctx, &ctx, &id, targs, values)
    }
}
