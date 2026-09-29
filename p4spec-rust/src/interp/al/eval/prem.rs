//! AL premise evaluation and iterative bindings
//!
//! A premise extends the context or fails:
//! rule premises bind outputs, `if` premises test a boolean,
//! hold premises test whether a relation applies, let premises assign,
//! iteration premises repeat under `iter::yield`.
//! A failed premise is an `Unmatch`, so the enclosing candidate is skipped.

use super::super::{AlInterp, context::Context};
use super::{assign, expr};
use crate::interp::shared::error;
use crate::interp::shared::{
    backtrack::{Backtrack, err, ok, unmatch, unwrap, unwrap_from_result},
    eval::{Invoker, iter},
};
use crate::runtime::envs::interp::al::ast_prepared as ast;
use crate::{
    lang::{data::value::get, hints::input, traits::print::Print},
    runner::{Extern, Interface, RunnerContext},
};

// = Premise evaluation

/// Evaluates a premise, propagating its classified failure unchanged.
pub fn eval_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::Prem,
) -> Backtrack<Context<'global>> {
    match &prem.node {
        ast::PremKind::Rule(prem) => eval_rule_prem(runner_ctx, ctx, prem),
        ast::PremKind::If(prem) => eval_if_prem(runner_ctx, ctx, prem),
        ast::PremKind::IfHold(prem) => eval_if_hold_prem(runner_ctx, ctx, prem),
        ast::PremKind::IfNotHold(prem) => eval_if_not_hold_prem(runner_ctx, ctx, prem),
        ast::PremKind::Let(prem) => eval_let_prem(runner_ctx, ctx, prem),
        ast::PremKind::Iter(prem) => eval_iter_prem(runner_ctx, ctx, prem),
        ast::PremKind::Debug(prem) => eval_debug_prem(runner_ctx, ctx, prem),
    }
}

/// Evaluates premises in order, threading the context.
pub fn eval_prems<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    mut ctx: Context<'global>,
    prems: &[ast::Prem],
) -> Backtrack<Context<'global>> {
    for prem in prems {
        ctx = unwrap!(eval_prem(runner_ctx, ctx, prem));
    }
    ok!(ctx)
}

// - Rule premise

/// Calls the relation on the input positions and binds the outputs.
fn eval_rule_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::RulePrem,
) -> Backtrack<Context<'global>> {
    // Split by the input hint, evaluate inputs, bind outputs
    let exps = prem.not_exp.args();
    let (exps_input, exps_output) =
        unwrap_from_result!(input::split(&prem.input_hint, exps), &prem.id.span);
    let values_input = unwrap!(expr::eval_exps(runner_ctx, &ctx, &exps_input));
    let values_output = unwrap!(AlInterp::invoke_rel(runner_ctx, &ctx, &prem.id, &values_input));
    assign::assign_exps(runner_ctx.arena_mut(), ctx, &exps_output, &values_output)
}

// - If premise

/// Passes when the condition holds; otherwise a mismatch.
fn eval_if_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::IfPrem,
) -> Backtrack<Context<'global>> {
    let value = unwrap!(expr::eval_exp(runner_ctx, &ctx, &prem.exp));
    if unwrap_from_result!(get::bool(runner_ctx.arena(), &value), &prem.exp.span) {
        ok!(ctx)
    } else {
        unmatch!(prem.exp.span.clone(), error::prem::condition_unmet(Print::to_string(&prem.exp)),)
    }
}

// - Hold premise

/// Passes when the relation applies; its mismatch becomes the premise's.
fn eval_if_hold_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::IfHoldPrem,
) -> Backtrack<Context<'global>> {
    let exps: Vec<_> = prem.not_exp.args();
    let values = unwrap!(expr::eval_exps(runner_ctx, &ctx, &exps));
    match AlInterp::invoke_rel(runner_ctx, &ctx, &prem.id, &values) {
        // The relation applied: the premise passes
        ok!(_) => ok!(ctx),
        // Fatal errors propagate
        err!(errors) => err!(errors),
        // It did not apply: the premise fails, naming the relation
        unmatch!(errors) => {
            let mut report = error::at(
                error::prem::hold_condition_unmet(prem.id.node.clone()),
                prem.id.span.clone(),
            );
            report.children = errors;
            unmatch!(vec![*report])
        }
    }
}

// - Not-hold premise

/// Passes when the relation does not apply; a match is the mismatch.
fn eval_if_not_hold_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::IfNotHoldPrem,
) -> Backtrack<Context<'global>> {
    let exps: Vec<_> = prem.not_exp.args();
    let values = unwrap!(expr::eval_exps(runner_ctx, &ctx, &exps));
    match AlInterp::invoke_rel(runner_ctx, &ctx, &prem.id, &values) {
        // The relation applied: the premise fails
        ok!(_) => unmatch!(
            prem.id.span.clone(),
            error::prem::not_hold_condition_unmet(prem.id.node.clone()),
        ),
        // Fatal errors propagate
        err!(errors) => err!(errors),
        // It did not apply: the premise passes
        unmatch!(_) => ok!(ctx),
    }
}

// - Let premise

/// Evaluates the right side and assigns it to the pattern.
fn eval_let_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::LetPrem,
) -> Backtrack<Context<'global>> {
    let value = unwrap!(expr::eval_exp(runner_ctx, &ctx, &prem.exp_r));
    assign::assign_exp(runner_ctx.arena_mut(), ctx, &prem.exp_l, value)
}

// - Iteration premise

/// Repeats the inner premise per element, gathering its bindings.
fn eval_iter_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::IterPrem,
) -> Backtrack<Context<'global>> {
    iter::r#yield(runner_ctx, ctx, &prem.prem.span, &prem.prem_iter, |runner_ctx, ctx_sub| {
        eval_prem(runner_ctx, ctx_sub, &prem.prem)
    })
}

// - Debug premise

/// Prints the expression and its value, then continues.
fn eval_debug_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::DebugPrem,
) -> Backtrack<Context<'global>> {
    let value = unwrap!(expr::eval_exp(runner_ctx, &ctx, &prem.exp));
    let exp_text = Print::to_string(&prem.exp);
    println!("{}: {}", prem.exp.span, exp_text);
    // Print the value's source span when it has one
    let span_text = runner_ctx.arena().span(&value).to_string();
    if span_text.is_empty() {
        println!("{}", runner_ctx.arena().to_string(&value));
    } else {
        println!("{span_text}: {}", runner_ctx.arena().to_string(&value));
    }
    ok!(ctx)
}
