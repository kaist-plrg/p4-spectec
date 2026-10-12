//! PL expression evaluation through the shared evaluator
//!
//! `eval_exp` and `eval_exps` use `strip` to remove prose hints,
//! then delegate to shared expression evaluation with the same slots.
//! Types and spans survive while the annotated PL tree stays intact.

use std::borrow::Borrow;

use crate::lang::data::value::flat::Value;

use crate::lang::pl::prepared as ast;

use crate::runner::{Extern, Interface, RunnerContext};

use crate::interp::shared::{backtrack::Backtrack, eval::expr as shared};

use crate::interp::pl::{PlInterp, context::Context};

use super::strip::strip_exp;

// = Expression evaluation

/// Evaluates a PL expression after removing its prose hints.
pub(super) fn eval_exp<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    exp: &ast::Exp,
) -> Backtrack<Value> {
    let exp_shared = strip_exp(exp);
    shared::eval_exp(runner_ctx, ctx, &exp_shared)
}

/// Evaluates PL expressions in order through the shared evaluator.
pub(super) fn eval_exps<Iface: Interface, Ext: Extern, T: Borrow<ast::Exp>>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    exps: &[T],
) -> Backtrack<Vec<Value>> {
    let exps_shared = exps
        .iter()
        .map(|exp| strip_exp(exp.borrow()))
        .collect::<Vec<_>>();
    shared::eval_exps(runner_ctx, ctx, &exps_shared)
}
