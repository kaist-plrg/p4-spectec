//! Structured branch selection and explicit tail-call flow
//!
//! `eval_block` runs instructions in order (or all of them under `det`),
//! each yielding a `Flow`.
//! Guards and conditions evaluate under their iterations (`eval_cond_iter`),
//! bindings under theirs (`eval_instr_iter`).
//! The `tail` flag marks the last instruction of a callee body,
//! so a function return there can become a tail call.
//! Relation tail calls are selected when the specification is prepared.

use std::borrow::Cow;

use crate::lang::{
    common::source::Span,
    data::value::flat::{self as value, Value},
    traits::print::Print,
};

use crate::lang::sl::prepared as ast;

use crate::diagnostic::Report;

use crate::runner::{Extern, Interface, RunnerContext};

use crate::interp::shared::{
    backtrack::{Backtrack, fatal, ok, unmatch, unwrap, unwrap_from_result},
    context::{IterContext, ReadContext, WriteContext},
    error,
    eval::{Invoker, iter, ops},
    util::iterate_vars,
};

use crate::phrase;

use super::super::{
    SlInterp,
    context::Context,
    flow::{self, Flow},
};

use super::{
    assign,
    expr::{self, eval_exp, eval_exps},
};

// = Block evaluation

/// Runs a block sequentially or, under `det`, deterministically.
pub fn eval_block<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    mut ctx: Cow<'_, Context<'_>>,
    mut block: &ast::Block,
    tail: bool,
) -> Backtrack<Flow> {
    if runner_ctx.interp().config.det {
        return eval_block_deterministic(runner_ctx, ctx.as_ref(), &block.instrs, tail);
    }
    // Earlier candidates borrow the context; the last transfers it forward
    let mut errors = Vec::new();
    let result = (|| loop {
        let Some((instr, instrs)) = block.instrs.split_last() else {
            return ok!(Flow::Cont(vec![]));
        };
        for instr in instrs {
            match unwrap!(eval_instr(runner_ctx, Cow::Borrowed(ctx.as_ref()), instr, false)) {
                Flow::Cont(errors_post) => flow::retain_deepest_errors(&mut errors, errors_post),
                flow => return ok!(flow),
            }
        }
        // The last instruction hands its continuation block back to this loop
        match unwrap!(enter_instr(runner_ctx, ctx, instr, tail)) {
            Next::Block(ctx_next, block_next) => {
                ctx = ctx_next;
                block = block_next;
            }
            Next::Flow(flow) => return ok!(flow),
        }
    })();
    // Nested final candidates contribute to the same deepest failure choice
    match result {
        // A mismatch of the last instruction ends only that alternative
        ok!(Flow::Cont(errors_post)) | unmatch!(errors_post) => {
            flow::retain_deepest_errors(&mut errors, errors_post);
            ok!(Flow::Cont(errors))
        }
        result => result,
    }
}

/// Runs the block; if it falls through, runs the otherwise block instead.
pub(crate) fn eval_block_with_else<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Context<'_>,
    block: &ast::Block,
    block_else: Option<&ast::ElseBlock>,
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

/// Evaluates one instruction; a mismatch ends only this alternative.
pub fn eval_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    instr: &ast::Instr,
    tail: bool,
) -> Backtrack<Flow> {
    // Grow the stack for deep blocks
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
        // Every instruction falls through on a mismatch, as its block's alternative
        Flow::cont_from_unmatch((|| match unwrap!(enter_instr(runner_ctx, ctx, instr, tail)) {
            Next::Block(ctx, block) => eval_block(runner_ctx, ctx, block, tail),
            Next::Flow(flow) => ok!(flow),
        })())
    })
}

/// An instruction's continuation once its own premise is decided.
enum Next<'instr, 'ctx, 'global> {
    /// Continues with this block under the context.
    Block(Cow<'ctx, Context<'global>>, &'instr ast::Block),
    /// Finishes with this flow.
    Flow(Flow),
}

/// Decides an instruction's premise, leaving its continuation block to the caller.
fn enter_instr<'instr, 'ctx, 'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'ctx, Context<'global>>,
    instr: &'instr ast::Instr,
    tail: bool,
) -> Backtrack<Next<'instr, 'ctx, 'global>> {
    let span = &instr.span;
    match &instr.node {
        ast::InstrKind::If(instr) => eval_if_instr(runner_ctx, ctx, instr),
        ast::InstrKind::Hold(instr) => eval_hold_instr(runner_ctx, ctx, instr),
        ast::InstrKind::Case(instr) => eval_case_instr(runner_ctx, ctx, instr),
        ast::InstrKind::Group(instr) => eval_group_instr(ctx, instr),
        ast::InstrKind::Let(instr) => eval_let_instr(runner_ctx, ctx, instr),
        ast::InstrKind::Rule(ast::RuleInstr::Call(instr)) => {
            eval_rule_instr(runner_ctx, ctx, instr)
        }
        // Terminal instructions finish with their own flow
        ast::InstrKind::Rule(ast::RuleInstr::Tail(instr)) => {
            eval_rule_tail_instr(runner_ctx, ctx, span, instr).map(Next::Flow)
        }
        ast::InstrKind::Result(instr) => {
            eval_result_instr(runner_ctx, ctx, span, instr).map(Next::Flow)
        }
        ast::InstrKind::Return(instr) => {
            eval_return_instr(runner_ctx, ctx, span, instr, tail).map(Next::Flow)
        }
        ast::InstrKind::Debug(instr) => {
            eval_debug_instr(runner_ctx, ctx, instr, tail).map(Next::Flow)
        }
    }
}

// - If instruction

/// Enters the block when the condition holds under its iterations.
fn eval_if_instr<'instr, 'ctx, 'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'ctx, Context<'global>>,
    instr: &'instr ast::IfInstr,
) -> Backtrack<Next<'instr, 'ctx, 'global>> {
    // The condition must hold for every iteration element
    let cond = unwrap!(eval_cond_iter(
        runner_ctx,
        ctx.as_ref(),
        &instr.iter_exps,
        &mut |runner_ctx, ctx| {
            let value = unwrap!(eval_exp(runner_ctx, ctx, &instr.exp));
            ok!(value::get::bool(runner_ctx.arena(), &value).expect("condition must be a boolean"))
        }
    ));
    // Enter the block, or fall through recording the failed condition
    if cond {
        ok!(Next::Block(ctx, &instr.block))
    } else {
        ok!(Next::Flow(Flow::cont(instr.exp.span, || error::prem::condition_unmet(
            instr.exp.view(&runner_ctx.arena().mixop).to_string()
        ),)))
    }
}

// - Hold instruction

/// Enters the branch for whether the relation applies; a missing one continues.
fn eval_hold_instr<'instr, 'ctx, 'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'ctx, Context<'global>>,
    instr: &'instr ast::HoldInstr,
) -> Backtrack<Next<'instr, 'ctx, 'global>> {
    // Whether the relation applies to the arguments, for every element
    let mut errors = Vec::new();
    let cond = unwrap!(eval_cond_iter(
        runner_ctx,
        ctx.as_ref(),
        &instr.iter_exps,
        &mut |runner_ctx, ctx| {
            let values = unwrap!(eval_exps(runner_ctx, ctx, instr.not_exp.args()));
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
            ok!(Next::Block(ctx, if cond { block_hold } else { block_not }))
        }
        // Only the matching branch present: enter it
        ast::HoldCase::Hold(block, _) if cond => ok!(Next::Block(ctx, block)),
        // Likewise for the not-hold branch
        ast::HoldCase::NotHold(block, _) if !cond => ok!(Next::Block(ctx, block)),
        // Only the other branch present: fall through
        ast::HoldCase::Hold(..) => {
            let diagnostic = error::prem::hold_condition_unmet(instr.id.node.to_string());
            let report = Report::from(diagnostic)
                .with_span(&instr.id.span)
                .with_children(errors);
            ok!(Next::Flow(Flow::Cont(vec![report])))
        }
        // Likewise, recording the failed not-hold condition
        ast::HoldCase::NotHold(..) => {
            ok!(Next::Flow(Flow::cont(instr.id.span, || error::prem::not_hold_condition_unmet(
                instr.id.node.to_string()
            ),)))
        }
    }
}

// - Case instruction

/// Enters the first case whose guard accepts the value; none continues.
fn eval_case_instr<'instr, 'ctx, 'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'ctx, Context<'global>>,
    instr: &'instr ast::CaseInstr,
) -> Backtrack<Next<'instr, 'ctx, 'global>> {
    // Evaluate the scrutinee once
    let value = unwrap!(eval_exp(runner_ctx, ctx.as_ref(), &instr.exp));
    // The first accepting guard enters its block
    for case in &instr.cases {
        if unwrap!(eval_guard(runner_ctx, ctx.as_ref(), &instr.exp.span, value, &case.guard)) {
            return ok!(Next::Block(ctx, &case.block));
        }
    }
    // No guard accepted: fall through
    ok!(Next::Flow(Flow::cont(instr.exp.span, || error::prem::condition_unmet(format!(
        "case {}",
        instr.exp.view(&runner_ctx.arena().mixop).to_string()
    )),)))
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
        return ok!(value::get::bool(runner_ctx.arena(), &value)
            .expect("boolean guard value must be a boolean"));
    }
    (|| match guard {
        // Negation
        ast::Guard::Bool(_) => {
            ok!(!value::get::bool(runner_ctx.arena(), &value)
                .expect("boolean guard value must be a boolean"))
        }
        // Comparison against the evaluated right side
        ast::Guard::Cmp(op, _, exp_r) => {
            let value_r = unwrap!(eval_exp(runner_ctx, ctx, exp_r));
            ops::cmpop(runner_ctx.arena(), span, op, value, value_r)
        }
        // Subtype check
        ast::Guard::Sub(subcheck) => ops::sub(runner_ctx.arena(), ctx, span, subcheck, value),
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

/// Enters the group's block; groups only structure the source.
fn eval_group_instr<'instr, 'ctx, 'global>(
    ctx: Cow<'ctx, Context<'global>>,
    instr: &'instr ast::GroupInstr,
) -> Backtrack<Next<'instr, 'ctx, 'global>> {
    ok!(Next::Block(ctx, &instr.block))
}

// - Let instruction

/// Assigns under the binding iterators, then enters the block.
fn eval_let_instr<'instr, 'ctx, 'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    mut ctx: Cow<'ctx, Context<'global>>,
    instr: &'instr ast::LetInstr,
) -> Backtrack<Next<'instr, 'ctx, 'global>> {
    // Without iterations, bind straight into the context
    if instr.iter_instrs.is_empty() {
        let value = unwrap!(eval_exp(runner_ctx, ctx.as_ref(), &instr.exp_r));
        unwrap!(assign::assign_in_place(runner_ctx.arena_mut(), ctx.to_mut(), &instr.exp_l, value));
    // Otherwise bind per element and gather the iterated bindings
    } else {
        let ctx_post = eval_instr_iter(
            runner_ctx,
            ctx.into_owned(),
            &instr.iter_instrs,
            &mut |runner_ctx, ctx| {
                let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp_r));
                assign::assign_exp(runner_ctx.arena_mut(), ctx, &instr.exp_l, value)
            },
        );
        ctx = Cow::Owned(unwrap!(ctx_post));
    }
    ok!(Next::Block(ctx, &instr.block))
}

// - Rule instruction

/// Calls the relation, binds its outputs, and enters the continuation.
fn eval_rule_instr<'instr, 'ctx, 'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'ctx, Context<'global>>,
    instr: &'instr ast::RuleCallInstr,
) -> Backtrack<Next<'instr, 'ctx, 'global>> {
    // Call and bind the outputs under the iterators
    let ctx_post = eval_instr_iter(
        runner_ctx,
        ctx.into_owned(),
        &instr.iter_instrs,
        &mut |runner_ctx, ctx| {
            let values_input = unwrap!(eval_exps(runner_ctx, &ctx, &instr.exps_input));
            let values = unwrap!(SlInterp::invoke_rel(runner_ctx, &ctx, &instr.id, &values_input));
            assign::assign_exps(runner_ctx.arena_mut(), ctx, &instr.exps_output, &values)
        },
    );
    ok!(Next::Block(Cow::Owned(unwrap!(ctx_post)), &instr.block))
}

/// Evaluates a prepared tail call's inputs for the invocation loop.
fn eval_rule_tail_instr<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'_>>,
    span: &Span,
    instr: &ast::RuleTailInstr,
) -> Backtrack<Flow> {
    let values = unwrap!(eval_exps(runner_ctx, ctx.as_ref(), &instr.exps_input));
    ok!(Flow::TailRel(phrase!(
        node: (instr.id.clone(), values),
        span: *span,
    )))
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
    ok!(Flow::Result(phrase!(node: values, span: *span)))
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
        // Every call becomes a tail call to the global definition it names
        let id = ctx.resolve_func_id(runner_ctx.arena(), id);
        ok!(Flow::TailFunc(phrase!(node: (id, targs.into_owned(), values), span: *span)))
    // Any other expression is evaluated and returned
    } else {
        let value = unwrap!(eval_exp(runner_ctx, ctx.as_ref(), &instr.exp));
        ok!(Flow::Return(phrase!(node: value, span: *span)))
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
    runner_ctx.record_effect(crate::runner::SideEffect::Impure);
    println!("{}: {}", instr.exp.span, instr.exp.view(&runner_ctx.arena().mixop).to_string());
    // Print the value's source span when it has one
    let span = runner_ctx.arena().value.span(value.span).to_string();
    if span.is_empty() {
        println!("{}", value.view(runner_ctx.arena()).to_string());
    } else {
        println!("{span}: {}", value.view(runner_ctx.arena()).to_string());
    }
    eval_instr(runner_ctx, ctx, &instr.instr, tail)
}

// = Iteration

// - Condition iteration

/// Evaluates a condition under nested iterations; every element must hold.
#[inline(always)]
fn eval_cond_iter<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    iters: &[ast::ExpIter],
    eval: &mut impl FnMut(&mut RunnerContext<'_, SlInterp, Iface, Ext>, &Context<'_>) -> Backtrack<bool>,
) -> Backtrack<bool> {
    if iters.is_empty() {
        return eval(runner_ctx, ctx);
    }
    eval_cond_iter_inner(runner_ctx, ctx, iters, eval)
}

/// Evaluates the nonempty nested iteration.
fn eval_cond_iter_inner<Iface: Interface, Ext: Extern>(
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
    match iter {
        // An option iterates zero or one time
        ast::Iter::Opt => {
            let vars_outer = iterate_vars(ctx, vars, *iter);
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
            let inputs = unwrap_from_result!(
                iter::ListInputs::new(runner_ctx.arena(), ctx, vars),
                &Span::default()
            );
            let mut ctx_sub = ctx.clone();
            for idx in 0..inputs.len() {
                inputs.bind(runner_ctx.arena(), &mut ctx_sub, vars, idx);
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
#[inline(always)]
fn eval_instr_iter<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: Context<'global>,
    iters: &[ast::InstrIter],
    eval: &mut impl FnMut(
        &mut RunnerContext<'_, SlInterp, Iface, Ext>,
        Context<'global>,
    ) -> Backtrack<Context<'global>>,
) -> Backtrack<Context<'global>> {
    if iters.is_empty() {
        return eval(runner_ctx, ctx);
    }
    eval_instr_iter_inner(runner_ctx, ctx, iters, eval)
}

/// Evaluates the nonempty nested iteration.
fn eval_instr_iter_inner<'global, Iface: Interface, Ext: Extern>(
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
