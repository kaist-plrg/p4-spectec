//! Structured branch selection and explicit tail-call flow
//!
//! `eval_block` runs instructions in order (or all of them under `det`),
//! each yielding a `Flow`.
//! Guards and conditions evaluate under their iterations (`eval_cond_iter`),
//! bindings under theirs (`eval_instr_iter`).
//! The `tail` flag marks the last instruction of a callee body,
//! so a return or rule call there can become a tail call.

use super::super::{
    SlInterp,
    context::{Context, Scope},
    flow::{self, Flow},
};
use super::{
    assign,
    expr::{self, eval_exp, eval_exps},
};
use crate::diagnostic::Report;
use crate::interp::shared::context::{IterContext, WriteContext};
use crate::interp::shared::error;
use crate::interp::shared::eval::{Invoker, iter, ops};
use crate::interp::shared::util::iterate_vars;
use crate::lang::hints::input;
use crate::phrase;
use crate::runtime::envs::interp::sl::ast_prepared as ast;
use crate::{
    interp::shared::backtrack::{Backtrack, fatal, ok, unmatch, unwrap, unwrap_from_result},
    lang::{
        common::source::Span,
        data::value::{Value, ValueKind, get},
        traits::{eq::SyntaxEq, print::Print},
    },
    runner::{Extern, Interface, RunnerContext},
};
use std::borrow::Cow;

// = Block evaluation

/// Runs a block sequentially or, under `det`, deterministically.
pub fn eval_block<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    block: &[ast::Instr],
    tail: bool,
) -> Backtrack<Flow> {
    if runner_ctx.interp().config.det {
        eval_block_deterministic(runner_ctx, ctx.as_ref(), block, tail)
    } else {
        eval_block_sequential(runner_ctx, ctx, block.iter(), tail)
    }
}

/// Runs the block; if it falls through, runs the otherwise block instead.
pub(crate) fn eval_block_with_else<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Context<'_>,
    block: &[ast::Instr],
    block_else: Option<&[ast::Instr]>,
) -> Backtrack<Flow> {
    // Without an otherwise block the body itself is in tail position
    let Some(block_else) = block_else else {
        return eval_block(runner_ctx, Cow::Owned(ctx), block, true);
    };
    // The otherwise block catches a body that fell through
    let flow = unwrap!(eval_block(runner_ctx, Cow::Borrowed(&ctx), block, false));
    if matches!(flow, Flow::Cont(_)) {
        eval_block(runner_ctx, Cow::Owned(ctx), block_else, true)
    } else {
        ok!(flow)
    }
}

/// Runs every instruction and merges their flows.
fn eval_block_deterministic<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    block: &[ast::Instr],
    tail: bool,
) -> Backtrack<Flow> {
    flow::choose_deterministic(block, |instr| {
        eval_instr(runner_ctx, Cow::Borrowed(ctx), instr, tail)
    })
}

/// Runs instructions in order; the last one gets the context and the tail flag.
pub(crate) fn eval_block_sequential<'instr, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    instrs: impl DoubleEndedIterator<Item = &'instr ast::Instr>,
    tail: bool,
) -> Backtrack<Flow> {
    // Hand the context over exactly once
    let mut ctx = Some(ctx);
    flow::choose_sequential(instrs, |instr, is_last| {
        // The last instruction owns the context
        if is_last {
            eval_instr(
                runner_ctx,
                ctx.take().expect("last instruction evaluated once"),
                instr,
                tail,
            )
        // Earlier ones borrow it and never run in tail position
        } else {
            eval_instr(
                runner_ctx,
                Cow::Borrowed(
                    ctx.as_ref()
                        .expect("last instruction not yet evaluated")
                        .as_ref(),
                ),
                instr,
                false,
            )
        }
    })
}

// = Instruction evaluation

/// Evaluates one instruction.
pub fn eval_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    instr: &ast::Instr,
    tail: bool,
) -> Backtrack<Flow> {
    let span = &instr.span;
    // Grow the stack for deep blocks
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
        match &instr.node {
            ast::InstrKind::If(instr) => eval_if_instr(runner_ctx, ctx, instr, tail),
            ast::InstrKind::Hold(instr) => eval_hold_instr(runner_ctx, ctx, instr, tail),
            ast::InstrKind::Case(instr) => eval_case_instr(runner_ctx, ctx, instr, tail),
            ast::InstrKind::Group(instr) => eval_group_instr(runner_ctx, ctx, instr, tail),
            // Binding and terminal instructions fall through on a mismatch
            ast::InstrKind::Let(instr) => {
                Flow::cont_from_unmatch(eval_let_instr(runner_ctx, ctx, instr, tail))
            }
            ast::InstrKind::Rule(instr) => {
                Flow::cont_from_unmatch(eval_rule_instr(runner_ctx, ctx, span, instr, tail))
            }
            ast::InstrKind::Result(instr) => {
                Flow::cont_from_unmatch(eval_result_instr(runner_ctx, ctx, span, instr))
            }
            ast::InstrKind::Return(instr) => {
                Flow::cont_from_unmatch(eval_return_instr(runner_ctx, ctx, span, instr, tail))
            }
            ast::InstrKind::Debug(instr) => {
                Flow::cont_from_unmatch(eval_debug_instr(runner_ctx, ctx, instr, tail))
            }
        }
    })
}

// - If instruction

/// Runs the block when the condition holds under its iterations.
fn eval_if_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    instr: &ast::IfInstr,
    tail: bool,
) -> Backtrack<Flow> {
    // The condition must hold for every iteration element
    let cond = unwrap!(eval_cond_iter(
        runner_ctx,
        ctx.as_ref(),
        &instr.iter_exps,
        &mut |runner_ctx, ctx| {
            let value = unwrap!(eval_exp(runner_ctx, ctx, &instr.exp));
            ok!(get::bool(runner_ctx.arena(), &value).expect("condition must be a boolean"))
        }
    ));
    // Run the block, or fall through recording the failed condition
    if cond {
        eval_block(runner_ctx, ctx, &instr.block, tail)
    } else {
        ok!(Flow::cont(
            instr.exp.span.clone(),
            error::prem::condition_unmet(Print::to_string(&instr.exp)),
        ))
    }
}

// - Hold instruction

/// Runs the branch for whether the relation applies; a missing one continues.
fn eval_hold_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    instr: &ast::HoldInstr,
    tail: bool,
) -> Backtrack<Flow> {
    // Whether the relation applies to the arguments, for every element
    let mut errors = Vec::new();
    let cond = unwrap!(eval_cond_iter(
        runner_ctx,
        ctx.as_ref(),
        &instr.iter_exps,
        &mut |runner_ctx, ctx| {
            let values = unwrap!(eval_exps(runner_ctx, ctx, &instr.not_exp.args()));
            match SlInterp::invoke_rel(runner_ctx, ctx, &instr.id, &values) {
                // A match means it holds
                ok!(_) => ok!(true),
                // Keep the reason if this hold condition fails
                unmatch!(reports) => {
                    errors = reports;
                    ok!(false)
                }
                // Fatal errors propagate
                fatal!(errors) => fatal!(errors),
            }
        }
    ));
    match &instr.hold_case {
        // Both branches present: pick by the outcome
        ast::HoldCase::Both(block_hold, block_not) => {
            eval_block(runner_ctx, ctx, if cond { block_hold } else { block_not }, tail)
        }
        // Only the matching branch present: run it
        ast::HoldCase::Hold(block, _) if cond => eval_block(runner_ctx, ctx, block, tail),
        // Likewise for the not-hold branch
        ast::HoldCase::NotHold(block, _) if !cond => eval_block(runner_ctx, ctx, block, tail),
        // Only the other branch present: fall through
        ast::HoldCase::Hold(..) => {
            let diagnostic = error::prem::hold_condition_unmet(instr.id.node.clone());
            let report = Report::from(diagnostic)
                .with_span(&instr.id.span)
                .with_children(errors);
            ok!(Flow::Cont(vec![report]))
        }
        // Likewise, recording the failed not-hold condition
        ast::HoldCase::NotHold(..) => ok!(Flow::cont(
            instr.id.span.clone(),
            error::prem::not_hold_condition_unmet(instr.id.node.clone()),
        )),
    }
}

// - Case instruction

/// Runs the first case whose guard accepts the value; none continues.
fn eval_case_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    instr: &ast::CaseInstr,
    tail: bool,
) -> Backtrack<Flow> {
    // Evaluate the scrutinee once
    let value = unwrap!(eval_exp(runner_ctx, ctx.as_ref(), &instr.exp));
    // The first accepting guard runs its block
    for case in &instr.cases {
        if unwrap!(eval_guard(runner_ctx, ctx.as_ref(), &instr.exp.span, value, &case.guard)) {
            return eval_block(runner_ctx, ctx, &case.block, tail);
        }
    }
    // No guard accepted: fall through
    ok!(Flow::cont(
        instr.exp.span.clone(),
        error::prem::condition_unmet(format!("case {}", Print::to_string(&instr.exp))),
    ))
}

/// Tests a guard against the scrutinee value.
fn eval_guard<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    span: &Span,
    value: Value,
    guard: &ast::Guard,
) -> Backtrack<bool> {
    // The trivial guard reads the boolean itself
    if matches!(guard, ast::Guard::Bool(true)) {
        return ok!(
            get::bool(runner_ctx.arena(), &value).expect("boolean guard value must be a boolean")
        );
    }
    (|| match guard {
        // Negation
        ast::Guard::Bool(_) => {
            ok!(!get::bool(runner_ctx.arena(), &value)
                .expect("boolean guard value must be a boolean"))
        }
        // Comparison against the evaluated right side
        ast::Guard::Cmp(op, _, exp_r) => {
            let value_r = unwrap!(eval_exp(runner_ctx, ctx, exp_r));
            ops::cmpop(runner_ctx.arena(), span, op, value, value_r)
        }
        // Subtype check
        ast::Guard::Sub(_, check) => ops::sub(runner_ctx.arena(), ctx, span, check, value),
        // Pattern match
        ast::Guard::Match(pattern) => {
            ok!(ops::r#match(runner_ctx.arena(), pattern, value))
        }
        // List membership
        ast::Guard::Mem(exp_list) => {
            let value_list = unwrap!(eval_exp(runner_ctx, ctx, exp_list));
            ops::mem(runner_ctx.arena(), span, value, value_list)
        }
    })()
}

// - Group instruction

/// Runs the group's block; groups only structure the source.
fn eval_group_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    instr: &ast::GroupInstr,
    tail: bool,
) -> Backtrack<Flow> {
    eval_block(runner_ctx, ctx, &instr.block, tail)
}

// - Let instruction

/// Assigns under the binding iterators, then runs the block.
fn eval_let_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    instr: &ast::LetInstr,
    tail: bool,
) -> Backtrack<Flow> {
    // Evaluate the right side and bind the left pattern, per element
    let ctx = unwrap!(eval_instr_iter(
        runner_ctx,
        ctx.into_owned(),
        &instr.iter_instrs,
        &mut |runner_ctx, ctx| {
            let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp_r));
            assign::assign_exp(runner_ctx.arena_mut(), ctx, &instr.exp_l, value)
        }
    ));
    // The block sees the new bindings
    eval_block(runner_ctx, Cow::Owned(ctx), &instr.block, tail)
}

// - Rule instruction

/// Calls the relation and binds its outputs, or becomes a tail call.
fn eval_rule_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    span: &Span,
    instr: &ast::RuleInstr,
    tail: bool,
) -> Backtrack<Flow> {
    // Split the notation arguments by the input hint
    let (exps_input, exps_output) = input::split_validated(&instr.input_hint, instr.not_exp.args());
    // A tail-position call whose block just returns its outputs is a tail call
    if tail
        && instr.iter_instrs.is_empty()
        && let [instr_result] = instr.block.as_slice()
        && let ast::InstrKind::Result(instr_result) = &instr_result.node
        && exps_output.len() == instr_result.exps.len()
        && exps_output
            .iter()
            .zip(&instr_result.exps)
            .all(|(exp_l, exp_r)| exp_l.syntax_eq(exp_r))
    {
        let values = unwrap!(eval_exps(runner_ctx, ctx.as_ref(), &exps_input));
        return ok!(Flow::TailRel(phrase!(
            node: (instr.id.clone(), values),
            span: span.clone(),
        )));
    }
    // Otherwise call, bind the outputs under the iterators, and run the block
    let ctx = unwrap!(eval_instr_iter(
        runner_ctx,
        ctx.into_owned(),
        &instr.iter_instrs,
        &mut |runner_ctx, ctx| {
            let values = unwrap!(eval_exps(runner_ctx, &ctx, &exps_input));
            let values = unwrap!(SlInterp::invoke_rel(runner_ctx, &ctx, &instr.id, &values));
            assign::assign_exps(runner_ctx.arena_mut(), ctx, &exps_output, &values)
        }
    ));
    // The block sees the bound outputs
    eval_block(runner_ctx, Cow::Owned(ctx), &instr.block, tail)
}

// - Result instruction

/// Evaluates the outputs and finishes the relation.
fn eval_result_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    span: &Span,
    instr: &ast::ResultInstr,
) -> Backtrack<Flow> {
    let values = unwrap!(eval_exps(runner_ctx, ctx.as_ref(), &instr.exps));
    ok!(Flow::Result(phrase!(node: values, span: span.clone())))
}

// - Return instruction

/// Returns the value, or turns a tail-position call into a tail call.
fn eval_return_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    span: &Span,
    instr: &ast::ReturnInstr,
    tail: bool,
) -> Backtrack<Flow> {
    // Only a call in tail position can become a tail call
    if tail && let ast::ExpKind::Call(id, targs, args) = &instr.exp.node {
        // Resolve type arguments and evaluate arguments before deciding
        let targs = unwrap_from_result!(expr::resolve_targs(ctx.as_ref(), targs), &id.span);
        let values = unwrap!(expr::eval_args(runner_ctx, ctx.as_ref(), args));
        let (scope, _) = unwrap_from_result!(ctx.find_func_with_scope(id), &id.span);
        // Local functions and function-valued arguments must be called here
        if scope == Scope::Local
            || values
                .iter()
                .any(|value| matches!(runner_ctx.arena().kind(value), ValueKind::Func(_)))
        {
            let value =
                unwrap!(SlInterp::invoke_func(runner_ctx, ctx.as_ref(), id, &targs, &values));
            ok!(Flow::Return(phrase!(node: value, span: span.clone())))
        // Global calls become tail calls for the invoker loop
        } else {
            ok!(Flow::TailFunc(phrase!(node: (id.clone(), targs, values), span: span.clone())))
        }
    // Any other expression is evaluated and returned
    } else {
        let value = unwrap!(eval_exp(runner_ctx, ctx.as_ref(), &instr.exp));
        ok!(Flow::Return(phrase!(node: value, span: span.clone())))
    }
}

// - Debug instruction

/// Prints the expression and its value, then runs the wrapped instruction.
fn eval_debug_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    instr: &ast::DebugInstr,
    tail: bool,
) -> Backtrack<Flow> {
    let value = unwrap!(eval_exp(runner_ctx, ctx.as_ref(), &instr.exp));
    println!("{}: {}", instr.exp.span, Print::to_string(&instr.exp));
    // Print the value's source span when it has one
    let span = runner_ctx.arena().span(&value).to_string();
    if span.is_empty() {
        println!("{}", runner_ctx.arena().to_string(&value));
    } else {
        println!("{span}: {}", runner_ctx.arena().to_string(&value));
    }
    eval_instr(runner_ctx, ctx, &instr.instr, tail)
}

// = Iteration

// - Condition iteration

/// Evaluates a condition under nested iterations; every element must hold.
fn eval_cond_iter<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    iters: &[ast::ExpIter],
    eval: &mut impl FnMut(&mut RunnerContext<'_, SlInterp, Iface, Ext>, &Context<'_>) -> Backtrack<bool>,
) -> Backtrack<bool> {
    // Recurse from the outermost iteration inward
    let Some((exp_iter, iters_tail)) = iters.split_last() else {
        return eval(runner_ctx, ctx);
    };
    let ast::ExpIter { iter, vars } = exp_iter;
    let vars_outer = iterate_vars(ctx, vars, *iter);
    match iter {
        // An option iterates zero or one time
        ast::Iter::Opt => {
            let values = unwrap_from_result!(
                ctx.find_opt_values_by_var(runner_ctx.arena(), &vars_outer),
                &Span::default()
            );
            // An absent option makes the condition false
            let Some(values) = values else {
                return ok!(false);
            };
            // Bind the inner variables for the nested check
            let mut ctx_sub = ctx.clone();
            for (var, value) in vars.iter().zip(values) {
                ctx_sub.add_value_at_slot(var.slot, value);
            }
            eval_cond_iter(runner_ctx, &ctx_sub, iters_tail, eval)
        }
        // A list iterates over its elements in lockstep
        ast::Iter::List => {
            let values_by_var = unwrap_from_result!(
                ctx.find_list_values_by_var(runner_ctx.arena(), &vars_outer),
                &Span::default()
            );
            // Copy handles before the callback can allocate in the arena
            let values_by_var: Vec<_> = values_by_var.into_iter().map(<[Value]>::to_vec).collect();
            let len = values_by_var.first().map_or(0, Vec::len);
            let mut ctx_sub = ctx.clone();
            for idx in 0..len {
                for (var, values) in vars.iter().zip(&values_by_var) {
                    ctx_sub.add_value_at_slot(var.slot, values[idx]);
                }
                // Every element must satisfy the condition
                if !unwrap!(eval_cond_iter(runner_ctx, &ctx_sub, iters_tail, eval)) {
                    return ok!(false);
                }
            }
            ok!(true)
        }
    }
}

// - Binding iteration

/// Runs a binding action under nested iterations, gathering its bindings.
fn eval_instr_iter<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Context<'global>,
    iters: &[ast::InstrIter],
    eval: &mut impl FnMut(
        &mut RunnerContext<'_, SlInterp, Iface, Ext>,
        Context<'global>,
    ) -> Backtrack<Context<'global>>,
) -> Backtrack<Context<'global>> {
    // Outermost iteration first, yielding through the rest
    let Some((iter, iters_tail)) = iters.split_last() else {
        return eval(runner_ctx, ctx);
    };
    iter::r#yield(runner_ctx, ctx, &Span::default(), iter, |runner_ctx, ctx_sub| {
        eval_instr_iter(runner_ctx, ctx_sub, iters_tail, eval)
    })
}
