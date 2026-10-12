//! Prose-language execution over annotated PL definitions
//!
//! `PlInterp` runs prepared group and dispatch blocks through `eval`.
//! Blocks isolate local bindings; alternatives share their enclosing scope.
//! An otherwise block handles a body that produces no conclusion.
//! `Config` controls memoization, alternative determinism, and type guards.

pub mod context;
mod eval;
pub mod flow;
mod prepare;

use crate::lang::{common::source::Span, data::value::flat::Value};

use crate::lang::pl::ast;

use crate::runner::{Extern, Interface, Interpreter, InterpreterError, RunnerContext};

use crate::interp::shared::{cache::Cache, eval::Invoker};

/// Configures the PL interpreter.
pub struct Config {
    /// Memoize pure calls.
    pub(crate) cache: bool,
    /// Evaluate every alternative and reject multiple conclusions.
    pub(crate) det: bool,
    /// Check argument and result types at call boundaries.
    pub(crate) guard: bool,
}

impl Config {
    pub fn new(cache: bool, det: bool, guard: bool) -> Self {
        Self { cache, det, guard }
    }
}

/// Stores interpreter configuration and the call cache.
pub struct PlInterp {
    pub(crate) config: Config,
    pub(crate) cache: Cache,
}

impl PlInterp {
    pub fn new(config: Config) -> Self {
        Self { config, cache: Cache::default() }
    }
}

impl<Iface: Interface, Ext: Extern> Interpreter<Iface, Ext> for PlInterp {
    type Spec = context::Global;

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
        let ctx = context::Context::new(runner_ctx.spec());
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
        let ctx = context::Context::new(runner_ctx.spec());
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
