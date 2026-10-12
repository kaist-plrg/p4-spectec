//! AL premise evaluation and iterative bindings
//!
//! A premise extends the context or fails:
//! rule premises bind outputs, `if` premises test a boolean,
//! hold premises test whether a relation applies, let premises assign,
//! iteration premises repeat under `iter::yield`.
//! A failed premise is an `Unmatch`, so the enclosing candidate is skipped.

use crate::lang::{data::value::flat as value, hints::input, traits::print::Print};

use crate::lang::al::prepared as ast;

use crate::diagnostic::Report;

use crate::runner::{Eval, Extern, Interface, RunnerContext};

use crate::interp::shared::{
    backtrack::{fatal, ok, unmatch, unwrap},
    error,
    eval::{Invoker, iter},
};

use super::super::{AlInterp, context::Context};

use super::{assign, expr};

// = Premise evaluation

/// Evaluates a premise, returning any failure unchanged.
pub fn eval_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::Prem,
) -> Eval<Context<'global>> {
    Eval::with(|effect| match &prem.node {
        ast::PremKind::Rule(prem) => effect.include(eval_rule_prem(runner_ctx, ctx, prem)),
        ast::PremKind::If(prem) => effect.include(eval_if_prem(runner_ctx, ctx, prem)),
        ast::PremKind::IfHold(prem) => effect.include(eval_if_hold_prem(runner_ctx, ctx, prem)),
        ast::PremKind::IfNotHold(prem) => {
            effect.include(eval_if_not_hold_prem(runner_ctx, ctx, prem))
        }
        ast::PremKind::Let(prem) => effect.include(eval_let_prem(runner_ctx, ctx, prem)),
        ast::PremKind::Iter(prem) => effect.include(eval_iter_prem(runner_ctx, ctx, prem)),
        ast::PremKind::Debug(prem) => effect.include(eval_debug_prem(runner_ctx, ctx, prem)),
    })
}

/// Evaluates premises in order, threading the context.
pub fn eval_prems<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    mut ctx: Context<'global>,
    prems: &[ast::Prem],
) -> Eval<Context<'global>> {
    Eval::with(|effect| {
        for prem in prems {
            ctx = unwrap!(effect.include(eval_prem(runner_ctx, ctx, prem)));
        }
        ok!(ctx)
    })
}

// - Rule premise

/// Calls the relation on the input positions and binds the outputs.
fn eval_rule_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::RulePrem,
) -> Eval<Context<'global>> {
    Eval::with(|effect| {
        // Split by the input hint, evaluate inputs, bind outputs
        let exps = prem.not_exp.args().iter().collect();
        let (exps_input, exps_output) =
            input::split(&prem.input_hint, exps).expect("input hint must fit relation");
        let values_input = unwrap!(effect.include(expr::eval_exps(runner_ctx, &ctx, &exps_input)));
        let values_output = unwrap!(effect.include(AlInterp::invoke_rel(
            runner_ctx,
            &ctx,
            &prem.id,
            &values_input
        )));
        assign::assign_exps(runner_ctx.arena_mut(), ctx, &exps_output, &values_output)
    })
}

// - If premise

/// Passes when the condition holds; otherwise a mismatch.
fn eval_if_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::IfPrem,
) -> Eval<Context<'global>> {
    Eval::with(|effect| {
        let value = unwrap!(effect.include(expr::eval_exp(runner_ctx, &ctx, &prem.exp)));
        if value::get::bool(runner_ctx.arena(), &value).expect("condition must be a boolean") {
            ok!(ctx)
        } else {
            unmatch!(
                prem.exp.span.clone(),
                error::prem::condition_unmet(prem.exp.view(&runner_ctx.arena().mixop).to_string()),
            )
        }
    })
}

// - Hold premise

/// Passes when the relation applies; its mismatch becomes the premise's.
fn eval_if_hold_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::IfHoldPrem,
) -> Eval<Context<'global>> {
    Eval::with(|effect| {
        let values =
            unwrap!(effect.include(expr::eval_exps(runner_ctx, &ctx, prem.not_exp.args())));
        match effect.include(AlInterp::invoke_rel(runner_ctx, &ctx, &prem.id, &values)) {
            // The relation applied: the premise passes
            ok!(_) => ok!(ctx),
            // Fatal errors propagate
            fatal!(errors) => fatal!(errors),
            // It did not apply: the premise fails, naming the relation
            unmatch!(errors) => {
                let diagnostic = error::prem::hold_condition_unmet(prem.id.node.to_string());
                let report = Report::from(diagnostic)
                    .with_span(&prem.id.span)
                    .with_children(errors);
                unmatch!(vec![report])
            }
        }
    })
}

// - Not-hold premise

/// Passes when the relation does not apply; a match is the mismatch.
fn eval_if_not_hold_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::IfNotHoldPrem,
) -> Eval<Context<'global>> {
    Eval::with(|effect| {
        let values =
            unwrap!(effect.include(expr::eval_exps(runner_ctx, &ctx, prem.not_exp.args())));
        match effect.include(AlInterp::invoke_rel(runner_ctx, &ctx, &prem.id, &values)) {
            // The relation applied: the premise fails
            ok!(_) => unmatch!(
                prem.id.span.clone(),
                error::prem::not_hold_condition_unmet(prem.id.node.to_string()),
            ),
            // Fatal errors propagate
            fatal!(errors) => fatal!(errors),
            // It did not apply: the premise passes
            unmatch!(_) => ok!(ctx),
        }
    })
}

// - Let premise

/// Evaluates the right side and assigns it to the pattern.
fn eval_let_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::LetPrem,
) -> Eval<Context<'global>> {
    Eval::with(|effect| {
        let value = unwrap!(effect.include(expr::eval_exp(runner_ctx, &ctx, &prem.exp_r)));
        assign::assign_exp(runner_ctx.arena_mut(), ctx, &prem.exp_l, value)
    })
}

// - Iteration premise

/// Repeats the inner premise per element, gathering its bindings.
fn eval_iter_prem<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: Context<'global>,
    prem: &ast::IterPrem,
) -> Eval<Context<'global>> {
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
) -> Eval<Context<'global>> {
    Eval::with(|effect| {
        let value = unwrap!(effect.include(expr::eval_exp(runner_ctx, &ctx, &prem.exp)));
        let exp_text = prem.exp.view(&runner_ctx.arena().mixop).to_string();
        *effect = crate::runner::SideEffect::Impure;
        println!("{}: {}", prem.exp.span, exp_text);
        // Print the value's source span when it has one
        let span_text = runner_ctx.arena().value.span(value.span).to_string();
        if span_text.is_empty() {
            println!("{}", value.view(runner_ctx.arena()).to_string());
        } else {
            println!("{span_text}: {}", value.view(runner_ctx.arena()).to_string());
        }
        ok!(ctx)
    })
}
