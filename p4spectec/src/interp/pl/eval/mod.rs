//! PL assignment, expression, instruction, and invocation evaluation
//!
//! `Invoker` for `PlInterp` delegates to `call`;
//! `instr` evaluates group and dispatch blocks into flows;
//! `assign` binds parameters; `expr` delegates to shared expression evaluation
//! after `strip` removes prose hints.

mod assign;
pub(super) mod call;
mod expr;
mod instr;
mod strip;

use crate::lang::data::value::flat::Value;

use crate::lang::il::prepared as ast;

use crate::runner::{Extern, Interface, RunnerContext};

use crate::interp::shared::{backtrack::Backtrack, eval::Invoker};

use crate::interp::pl::{PlInterp, context};

impl<Iface: Interface, Ext: Extern> Invoker<Iface, Ext> for PlInterp {
    type Context<'global> = context::Context<'global>;

    fn invoke_func<'global>(
        runner_ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        ctx: &Self::Context<'global>,
        id: &ast::Id,
        targs: &[ast::Typ],
        values: &[Value],
    ) -> Backtrack<Value> {
        call::invoke_func(runner_ctx, ctx, id, targs, values)
    }

    fn invoke_rel<'global>(
        runner_ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        ctx: &Self::Context<'global>,
        id: &ast::Id,
        values: &[Value],
    ) -> Backtrack<Vec<Value>> {
        call::invoke_rel(runner_ctx, ctx, id, values)
    }
}
