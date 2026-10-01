//! AL assignment, expression, premise, and invocation evaluation
//!
//! `Invoker` for `AlInterp` delegates to `call`;
//! `prem` evaluates premises into extended contexts;
//! `assign` and `expr` re-export the shared evaluation.

pub mod assign;
pub mod call;
pub mod expr;
pub mod prem;

use crate::lang::data::value::Value;

use crate::runtime::envs::interp::al::ast_prepared as ast;

use crate::runner::{Extern, Interface, RunnerContext};

use crate::interp::shared::{backtrack::Backtrack, eval::Invoker};

use super::{AlInterp, context::Context};

impl<Iface: Interface, Ext: Extern> Invoker<Iface, Ext> for AlInterp {
    type Context<'global> = Context<'global>;

    fn invoke_func<'global>(
        runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
        ctx: &Context<'global>,
        id: &ast::Id,
        targs: &[ast::Typ],
        values: &[Value],
    ) -> Backtrack<Value> {
        call::invoke_func(runner_ctx, ctx, id, targs, values)
    }

    fn invoke_rel<'global>(
        runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
        ctx: &Context<'global>,
        id: &ast::Id,
        values: &[Value],
    ) -> Backtrack<Vec<Value>> {
        call::invoke_rel(runner_ctx, ctx, id, values)
    }
}
