//! Shared tail-call evaluation
//!
//! A conclusion that preparation made a tail call evaluates the call's arguments here;
//! the invoker loop then makes the call in place of the current one.
//! `eval_tail_func` also resolves a call through a function parameter
//! to the global definition its value names; `eval_tail_rel` evaluates the inputs.

use crate::lang::data::value::flat::Value;

use crate::lang::il::prepared as ast;

use crate::runner::{Extern, Interface, RunnerContext};

use crate::interp::shared::backtrack::{Backtrack, ok, unwrap, unwrap_from_result};

use super::super::context::ReadContext;

use super::{
    Invoker,
    arg::eval_args,
    expr::{eval_exps, resolve_targs},
};

// = Tail calls

/// Evaluates a function tail call's arguments and resolves the global definition it names.
pub(crate) fn eval_tail_func<
    'global,
    Interp: Invoker<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    tail: &ast::TailFunc,
) -> Backtrack<(ast::Id, Vec<ast::Typ>, Vec<Value>)> {
    let targs = unwrap_from_result!(resolve_targs(ctx, &tail.targs), &tail.id.span);
    let values = unwrap!(eval_args(runner_ctx, ctx, &tail.args));
    // A call through a function parameter is made to the global definition its value names
    let id = ctx.resolve_func_id(runner_ctx.arena(), &tail.id);
    ok!((id, targs.into_owned(), values))
}

/// Evaluates a relation tail call's inputs.
pub(crate) fn eval_tail_rel<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    tail: &ast::TailRel,
) -> Backtrack<(ast::Id, Vec<Value>)> {
    let values = unwrap!(eval_exps(runner_ctx, ctx, &tail.exps_input));
    ok!((tail.id.clone(), values))
}
