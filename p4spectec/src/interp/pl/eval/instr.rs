//! PL instruction, block, and alternative evaluation
//!
//! `eval_group_block` executes a rule group or function body;
//! `eval_dispatch_block` selects relation groups through routing instructions.
//! `eval_block` threads bindings; `eval_alternatives` selects conclusions.
//! Blocks run sequentially; alternatives delegate selection to `pl::flow`.
//!
//! A nested block borrows the enclosing bindings and copies them only to bind,
//! unless nothing reads them after it: the last instruction of a block,
//! the last alternative, and a callable body without an otherwise block
//! own the bindings and extend them in place.

use std::borrow::Cow;

use crate::lang::{
    common::source::Span,
    data::value::flat::{self as value, Value},
    traits::{eq::SyntaxEq, print::Print},
};

use crate::lang::pl::prepared as ast;

use crate::diagnostic::Report;

use crate::runner::{Extern, Interface, RunnerContext};

use crate::interp::shared::{
    backtrack::{Backtrack, fatal, ok, unmatch, unwrap, unwrap_from_result},
    context::{IterContext, WriteContext},
    error,
    eval::{Invoker, iter, ops},
    util::iterate_vars,
};

use crate::interp::pl::{
    PlInterp,
    context::Context,
    flow::{self, Flow},
};

use crate::phrase;

use super::{
    assign,
    expr::{eval_exp, eval_exps},
};

// = Block bindings

/// The bindings of a running block, as one of its instructions sees them.
///
/// A binding instruction takes them and extends them for the rest of the block.
/// Any other instruction reads them, and lends them to its nested blocks;
/// the last instruction hands them over instead, since the block ends with it.
pub(super) struct BlockContext<'ctx, 'global> {
    /// The bindings, absent once handed over.
    ctx: Option<Cow<'ctx, Context<'global>>>,
    /// Whether the instruction is the last of its block.
    last: bool,
}

impl<'global> BlockContext<'_, 'global> {
    fn get(&self) -> &Context<'global> {
        self.ctx
            .as_deref()
            .expect("block bindings are present until handed over")
    }

    /// Lends the bindings to a nested block; the last instruction hands them over.
    fn pass(&mut self) -> Cow<'_, Context<'global>> {
        if self.last {
            self.ctx
                .take()
                .expect("block bindings are handed over once")
        } else {
            Cow::Borrowed(self.get())
        }
    }

    /// Extends the bindings for the rest of the block, copying borrowed ones first.
    fn bind(
        &mut self,
        bind: impl FnOnce(Context<'global>) -> Backtrack<Context<'global>>,
    ) -> Backtrack<()> {
        let ctx = self
            .ctx
            .take()
            .expect("block bindings are present until handed over");
        let ctx = unwrap!(bind(ctx.into_owned()));
        self.ctx = Some(Cow::Owned(ctx));
        ok!(())
    }
}

// = Block evaluation

/// Runs instructions sequentially, threading each one's bindings to the next.
pub(super) fn eval_block<'global, 'instr, Tier: 'instr, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'global>>,
    mut evaluate: impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        &mut BlockContext<'_, 'global>,
        &ast::Instr<Tier>,
    ) -> Backtrack<Flow>,
    instrs: impl IntoIterator<Item = &'instr ast::Instr<Tier>>,
) -> Backtrack<Flow> {
    let mut ctx = BlockContext { ctx: Some(ctx), last: false };
    let mut errors = Vec::new();
    let mut instrs = instrs.into_iter().peekable();
    while let Some(instr) = instrs.next() {
        // The last instruction may take the bindings
        ctx.last = instrs.peek().is_none();
        match unwrap!(evaluate(runner_ctx, &mut ctx, instr)) {
            // Continue with the most specific failure so far
            Flow::Cont(errors_post) => {
                flow::retain_deepest_errors(&mut errors, errors_post);
            }
            // A conclusion ends this execution path
            flow => return ok!(flow),
        }
    }
    ok!(Flow::Cont(errors))
}

/// Runs a group body.
pub(super) fn eval_group_block<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'global>>,
    block: &ast::GroupBlock,
) -> Backtrack<Flow> {
    eval_block(runner_ctx, ctx, eval_group_instr, &block.instrs)
}

/// Runs relation routing.
pub(super) fn eval_dispatch_block<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'global>>,
    block: &ast::DispatchBlock,
) -> Backtrack<Flow> {
    eval_block(runner_ctx, ctx, eval_dispatch_instr, &block.instrs)
}

/// Runs a callable body; if it falls through, runs the otherwise block instead.
///
/// The otherwise block starts from the same input bindings,
/// so the body borrows them only when an otherwise block follows.
pub(super) fn eval_block_with_else<'global, Block, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    block: &Block,
    block_else: Option<&Block>,
    mut evaluate: impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &Block,
    ) -> Backtrack<Flow>,
) -> Backtrack<Flow> {
    // Without an otherwise block the body owns the bindings
    let Some(block_else) = block_else else {
        return Flow::cont_from_unmatch(evaluate(runner_ctx, Cow::Owned(ctx), block));
    };
    // A recoverable mismatch in the body permits the otherwise block
    let flow = unwrap!(Flow::cont_from_unmatch(evaluate(runner_ctx, Cow::Borrowed(&ctx), block)));
    if matches!(flow, Flow::Cont(_)) {
        evaluate(runner_ctx, Cow::Owned(ctx), block_else)
    } else {
        ok!(flow)
    }
}

// - Alternative selection

/// Chooses between isolated alternatives in either execution mode.
fn eval_alternatives<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Cow<'_, Context<'global>>,
    mut evaluate: impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &ast::Block<Tier>,
    ) -> Backtrack<Flow>,
    blocks: &[ast::Block<Tier>],
) -> Backtrack<Flow> {
    let det = runner_ctx.interp().config.det;
    // Every alternative starts from the enclosing bindings; the last one takes them
    let mut ctx = Some(ctx);
    let mut eval = |(idx, block): (usize, &ast::Block<Tier>)| {
        let ctx = if idx + 1 == blocks.len() {
            ctx.take()
                .expect("the last alternative takes the bindings once")
        } else {
            Cow::Borrowed(
                ctx.as_deref()
                    .expect("bindings are present until the last alternative"),
            )
        };
        evaluate(runner_ctx, ctx, block)
    };
    if det {
        // Deterministic choice checks alternatives for conflicting conclusions
        flow::choose_deterministic(blocks.iter().enumerate(), eval)
    } else {
        // A mismatch ends this alternative, but permits trying the next one
        flow::choose_sequential(blocks.iter().enumerate(), |candidate| {
            Flow::cont_from_unmatch(eval(candidate))
        })
    }
}

// = Instruction evaluation

/// Evaluates one instruction, returning any failure unchanged.
fn eval_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    eval_tier: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        &mut BlockContext<'_, 'global>,
        &Tier,
    ) -> Backtrack<Flow>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &ast::Block<Tier>,
    ) -> Backtrack<Flow>,
    instr: &ast::Instr<Tier>,
) -> Backtrack<Flow> {
    // Grow the stack for deep blocks
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || match &instr.node.node {
        ast::InstrKind::If(instr) => eval_if_instr(runner_ctx, ctx, evaluate_block, instr),
        ast::InstrKind::IfLetSub(instr) => {
            eval_if_let_sub_instr(runner_ctx, ctx, evaluate_block, instr)
        }
        ast::InstrKind::IfLetMatch(instr) => {
            eval_if_let_match_instr(runner_ctx, ctx, evaluate_block, instr)
        }
        ast::InstrKind::Check(instr) => eval_check_instr(runner_ctx, ctx, evaluate_block, instr),
        ast::InstrKind::Hold(instr) => eval_hold_instr(runner_ctx, ctx, evaluate_block, instr),
        ast::InstrKind::Case(instr) => eval_case_instr(runner_ctx, ctx, evaluate_block, instr),
        ast::InstrKind::Let(instr) => eval_let_instr(runner_ctx, ctx, instr),
        ast::InstrKind::Debug(instr) => eval_debug_instr(runner_ctx, ctx, instr),
        ast::InstrKind::Destruct(instr) => eval_destruct_instr(runner_ctx, ctx, instr),
        ast::InstrKind::Tier(instr) => eval_tier(runner_ctx, ctx, &instr.tier),
    })
}

/// Evaluates one group instruction.
pub(super) fn eval_group_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    instr: &ast::Instr<ast::GroupInstr>,
) -> Backtrack<Flow> {
    eval_instr(
        runner_ctx,
        ctx,
        &mut |runner_ctx, ctx, tier| eval_group_tier(runner_ctx, ctx, &instr.node.span, tier),
        &mut eval_group_block,
        instr,
    )
}

fn eval_group_tier<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    span: &Span,
    tier: &ast::GroupInstr,
) -> Backtrack<Flow> {
    match tier {
        ast::GroupInstr::Result(instr) => eval_result_instr(runner_ctx, ctx, span, instr),
        ast::GroupInstr::Return(instr) => eval_return_instr(runner_ctx, ctx, span, instr),
        ast::GroupInstr::Rule(instr) => eval_rule_instr(runner_ctx, ctx, instr),
        ast::GroupInstr::Backtrack(instr) => eval_backtrack_instr(runner_ctx, ctx, instr),
    }
}

/// Evaluates one dispatch instruction.
fn eval_dispatch_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    instr: &ast::Instr<ast::DispatchInstr>,
) -> Backtrack<Flow> {
    eval_instr(runner_ctx, ctx, &mut eval_dispatch_tier, &mut eval_dispatch_block, instr)
}

fn eval_dispatch_tier<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    tier: &ast::DispatchInstr,
) -> Backtrack<Flow> {
    match tier {
        ast::DispatchInstr::Group(instr) => eval_rule_group_instr(runner_ctx, ctx, instr),
        ast::DispatchInstr::Route(instr) => eval_route_instr(runner_ctx, ctx, instr),
    }
}

// - If instruction

/// Evaluates the if condition and its nested block.
fn eval_if_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &ast::Block<Tier>,
    ) -> Backtrack<Flow>,
    instr: &ast::IfInstr<Tier>,
) -> Backtrack<Flow> {
    eval_condition(runner_ctx, ctx, evaluate_block, &instr.exp, &instr.iter_exps, &instr.block)
}

// - Subtype binding instruction

/// Tests and narrows the scrutinee once, keeping its binding inside the body.
fn eval_if_let_sub_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &ast::Block<Tier>,
    ) -> Backtrack<Flow>,
    instr: &ast::IfLetSubInstr<Tier>,
) -> Backtrack<Flow> {
    // Test the original value before narrowing it for the binding
    let value = unwrap!(eval_exp(runner_ctx, ctx.get(), &instr.exp_r));
    let ast::SubBind { typ, subcheck } = &instr.target;
    if !unwrap!(ops::sub(runner_ctx.arena(), ctx.get(), &instr.exp_r.span, subcheck, value)) {
        let text = format!(
            "({} has type {})",
            instr.exp_r.view(&runner_ctx.arena().mixop).to_string(),
            typ.to_string()
        );
        return ok!(Flow::cont(instr.exp_r.span, error::prem::condition_unmet(text)));
    }
    // Same-type bindings do not introduce an otherwise absent cast
    let value = if instr.exp_r.note.as_ref().syntax_eq(&typ.node) {
        value
    } else {
        unwrap!(ops::cast_down(runner_ctx.arena_mut(), ctx.get(), typ, value))
    };
    // The binding stays inside the body
    let mut ctx_local = ctx.pass();
    unwrap!(assign::assign_exp(runner_ctx.arena_mut(), ctx_local.to_mut(), &instr.exp_l, value));
    evaluate_block(runner_ctx, ctx_local, &instr.block)
}

// - Pattern binding instruction

/// Tests the scrutinee once and runs the body with its pattern bindings.
fn eval_if_let_match_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &ast::Block<Tier>,
    ) -> Backtrack<Flow>,
    instr: &ast::IfLetMatchInstr<Tier>,
) -> Backtrack<Flow> {
    // Evaluate and test before introducing local bindings
    let value = unwrap!(eval_exp(runner_ctx, ctx.get(), &instr.exp_r));
    if !ops::r#match(runner_ctx.arena(), &instr.pattern, value) {
        let text = format!(
            "({} matches pattern {})",
            instr.exp_r.view(&runner_ctx.arena().mixop).to_string(),
            instr.pattern.view(&runner_ctx.arena().mixop).to_string()
        );
        return ok!(Flow::cont(instr.exp_r.span, error::prem::condition_unmet(text)));
    }
    // Binding and body failures propagate after the condition succeeds
    let mut ctx_local = ctx.pass();
    unwrap!(assign::assign_exp(runner_ctx.arena_mut(), ctx_local.to_mut(), &instr.exp_l, value));
    evaluate_block(runner_ctx, ctx_local, &instr.block)
}

// - Check instruction

/// Evaluates the check condition and its nested block.
fn eval_check_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &ast::Block<Tier>,
    ) -> Backtrack<Flow>,
    instr: &ast::CheckInstr<Tier>,
) -> Backtrack<Flow> {
    eval_condition(runner_ctx, ctx, evaluate_block, &instr.exp, &instr.iter_exps, &instr.block)
}

// - Conditions

/// Runs the block when the condition holds under its iterations.
fn eval_condition<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &ast::Block<Tier>,
    ) -> Backtrack<Flow>,
    exp: &ast::Exp,
    iter_exps: &[ast::CondIter],
    block: &ast::Block<Tier>,
) -> Backtrack<Flow> {
    // Test the condition across all enclosing iterations
    let cond = unwrap!(eval_cond_iter(runner_ctx, ctx.get(), iter_exps, &mut |runner_ctx, ctx| {
        let value = unwrap!(eval_exp(runner_ctx, ctx, exp));
        ok!(value::get::bool(runner_ctx.arena(), &value).expect("condition must be a boolean"))
    }));
    // Run the block, or fall through recording the failed condition
    if cond {
        evaluate_block(runner_ctx, ctx.pass(), block)
    } else {
        ok!(Flow::cont(
            exp.span,
            error::prem::condition_unmet(exp.view(&runner_ctx.arena().mixop).to_string())
        ))
    }
}

// - Hold instruction

/// Runs the branch for whether the relation applies; a missing one continues.
fn eval_hold_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &ast::Block<Tier>,
    ) -> Backtrack<Flow>,
    instr: &ast::HoldInstr<Tier>,
) -> Backtrack<Flow> {
    // Keep the first failed relation call under the condition
    let mut errors = Vec::new();
    let cond =
        unwrap!(eval_cond_iter(runner_ctx, ctx.get(), &instr.iter_exps, &mut |runner_ctx, ctx| {
            let values = unwrap!(eval_exps(runner_ctx, ctx, instr.not_exp.args()));
            match PlInterp::invoke_rel(runner_ctx, ctx, &instr.id, &values) {
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
        }));
    match &instr.hold_case {
        // Both branches present: pick by the outcome
        ast::HoldCase::Both(block_hold, block_not) => {
            evaluate_block(runner_ctx, ctx.pass(), if cond { block_hold } else { block_not })
        }
        // Only the matching branch present: run it
        ast::HoldCase::Hold(block, _) if cond => evaluate_block(runner_ctx, ctx.pass(), block),
        // Likewise for the not-hold branch
        ast::HoldCase::NotHold(block, _) if !cond => evaluate_block(runner_ctx, ctx.pass(), block),
        // A failed hold condition retains its relation failure
        ast::HoldCase::Hold(..) => {
            let diagnostic = error::prem::hold_condition_unmet(instr.id.node.to_string());
            let report = Report::from(diagnostic)
                .with_span(&instr.id.span)
                .with_children(errors);
            ok!(Flow::Cont(vec![report]))
        }
        // A failed not-hold condition has no inner failure
        ast::HoldCase::NotHold(..) => ok!(Flow::cont(
            instr.id.span,
            error::prem::not_hold_condition_unmet(instr.id.node.to_string()),
        )),
    }
}

// - Case instruction

/// Runs the first matching case with its body bindings kept local.
fn eval_case_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Cow<'_, Context<'global>>,
        &ast::Block<Tier>,
    ) -> Backtrack<Flow>,
    instr: &ast::CaseInstr<Tier>,
) -> Backtrack<Flow> {
    // Evaluate the scrutinee once
    let value = unwrap!(eval_exp(runner_ctx, ctx.get(), &instr.exp));
    // Try guards in source order from the enclosing bindings
    for case in &instr.cases {
        if unwrap!(eval_guard(runner_ctx, ctx.get(), value, &case.guard)) {
            // Commit to the accepted guard before assigning its local binding
            let mut ctx_local = ctx.pass();
            match &case.guard {
                ast::Guard::LetSub(ast::SubBind { typ, .. }, exp_l) => {
                    let value = if instr.exp.note.as_ref().syntax_eq(&typ.node) {
                        value
                    } else {
                        unwrap!(ops::cast_down(
                            runner_ctx.arena_mut(),
                            ctx_local.as_ref(),
                            typ,
                            value
                        ))
                    };
                    unwrap!(assign::assign_exp(
                        runner_ctx.arena_mut(),
                        ctx_local.to_mut(),
                        exp_l,
                        value
                    ));
                }
                ast::Guard::LetMatch(_, exp_l) => {
                    unwrap!(assign::assign_exp(
                        runner_ctx.arena_mut(),
                        ctx_local.to_mut(),
                        exp_l,
                        value
                    ));
                }
                _ => {}
            }
            // Keep body bindings inside the selected case
            return evaluate_block(runner_ctx, ctx_local, &case.block);
        }
    }
    // No guard accepted: fall through
    ok!(Flow::cont(
        instr.exp.span,
        error::prem::condition_unmet(instr.exp.view(&runner_ctx.arena().mixop).to_string())
    ))
}

/// Tests a case guard against the evaluated scrutinee without binding.
fn eval_guard<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'global>,
    value: Value,
    guard: &ast::Guard,
) -> Backtrack<bool> {
    match guard {
        // Compare the scrutinee with the expected boolean
        ast::Guard::Bool(expected) => ok!(value::get::bool(runner_ctx.arena(), &value)
            .expect("boolean guard value must be a boolean")
            == *expected),
        // Compare against the evaluated right side
        ast::Guard::Cmp(op, _, exp_r) => {
            let value_r = unwrap!(eval_exp(runner_ctx, ctx, exp_r));
            ops::cmpop(runner_ctx.arena(), &exp_r.span, op, value, value_r)
        }
        // Check membership before introducing any binding
        ast::Guard::Sub(subcheck) | ast::Guard::LetSub(ast::SubBind { subcheck, .. }, _) => {
            ops::sub(runner_ctx.arena(), ctx, &Span::default(), subcheck, value)
        }
        // Check the pattern before introducing any binding
        ast::Guard::Match(pattern) | ast::Guard::LetMatch(pattern, _) => {
            ok!(ops::r#match(runner_ctx.arena(), pattern, value))
        }
        // Test membership in the evaluated list
        ast::Guard::Mem(exp_list) => {
            let value_list = unwrap!(eval_exp(runner_ctx, ctx, exp_list));
            ops::mem(runner_ctx.arena(), &exp_list.span, value, value_list)
        }
    }
}

// - Let instruction

/// Assigns under the binding iterators and continues with the new bindings.
fn eval_let_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    instr: &ast::LetInstr,
) -> Backtrack<Flow> {
    // Extend the current block with bindings from each iteration
    unwrap!(ctx.bind(|ctx| eval_instr_iter(
        runner_ctx,
        ctx,
        &instr.iter_instrs,
        &mut |runner_ctx, mut ctx| {
            let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp_r));
            unwrap!(assign::assign_exp(runner_ctx.arena_mut(), &mut ctx, &instr.exp_l, value));
            ok!(ctx)
        }
    )));
    ok!(Flow::Cont(vec![]))
}

// - Rule instruction

/// Calls the relation and binds its outputs under the binding iterators.
fn eval_rule_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    instr: &ast::RuleInstr,
) -> Backtrack<Flow> {
    // Invoke the relation at each enclosing iteration
    unwrap!(ctx.bind(|ctx| eval_instr_iter(
        runner_ctx,
        ctx,
        &instr.iter_instrs,
        &mut |runner_ctx, mut ctx| {
            let values = unwrap!(eval_exps(runner_ctx, &ctx, &instr.exps_input));
            let values = unwrap!(PlInterp::invoke_rel(runner_ctx, &ctx, &instr.id, &values));
            // Extend the local bindings with relation outputs
            unwrap!(assign::assign_exps(
                runner_ctx.arena_mut(),
                &mut ctx,
                &instr.exps_output,
                &values
            ));
            ok!(ctx)
        }
    )));
    ok!(Flow::Cont(vec![]))
}

// - Result instruction

/// Evaluates the outputs and finishes the relation.
fn eval_result_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    span: &Span,
    instr: &ast::ResultInstr,
) -> Backtrack<Flow> {
    let values = unwrap!(eval_exps(runner_ctx, ctx.get(), &instr.exps_output));
    ok!(Flow::Result(phrase!(node: values, span: *span)))
}

// - Return instruction

/// Evaluates the value and finishes the function.
fn eval_return_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    span: &Span,
    instr: &ast::ReturnInstr,
) -> Backtrack<Flow> {
    let value = unwrap!(eval_exp(runner_ctx, ctx.get(), &instr.exp));
    ok!(Flow::Return(phrase!(node: value, span: *span)))
}

// - Debug instruction

/// Prints the expression value and continues with the current bindings.
fn eval_debug_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    instr: &ast::DebugInstr,
) -> Backtrack<Flow> {
    // Evaluate before printing so expression failures keep their trace
    let value = unwrap!(eval_exp(runner_ctx, ctx.get(), &instr.exp));
    runner_ctx.record_effect(crate::runner::SideEffect::Impure);
    println!("{}", value.view(runner_ctx.arena()).to_string());
    ok!(Flow::Cont(vec![]))
}

// - Destruct instruction

/// Binds the fields of a case value and continues with the new bindings.
fn eval_destruct_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    instr: &ast::DestructInstr,
) -> Backtrack<Flow> {
    // Extract fields before mutating the arena during assignment
    let value = unwrap!(eval_exp(runner_ctx, ctx.get(), &instr.exp));
    let value_case =
        value::get::case(runner_ctx.arena(), &value).expect("destructuring value must be a case");
    let values = value_case.args().to_vec();
    let exps = instr
        .bindings
        .iter()
        .map(|(_, exp)| exp)
        .collect::<Vec<_>>();
    // Bind each extracted field to its pattern
    unwrap!(ctx.bind(|mut ctx| {
        unwrap!(assign::assign_exps(runner_ctx.arena_mut(), &mut ctx, &exps, &values));
        ok!(ctx)
    }));
    ok!(Flow::Cont(vec![]))
}

// - Backtrack instruction

/// Chooses a conclusion from isolated group-body alternatives.
fn eval_backtrack_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    instr: &ast::BacktrackInstr,
) -> Backtrack<Flow> {
    eval_alternatives(runner_ctx, ctx.pass(), eval_group_block, &instr.blocks)
}

// - Rule group instruction

/// Runs a relation group in its own local scope.
fn eval_rule_group_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    instr: &ast::RuleGroupInstr,
) -> Backtrack<Flow> {
    eval_group_block(runner_ctx, ctx.pass(), &instr.block)
}

// - Route instruction

/// Chooses a conclusion from isolated dispatch alternatives.
fn eval_route_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &mut BlockContext<'_, 'global>,
    instr: &ast::RouteInstr,
) -> Backtrack<Flow> {
    eval_alternatives(runner_ctx, ctx.pass(), eval_dispatch_block, &instr.blocks)
}

// = Iteration

// - Condition iteration

/// Evaluates a condition under nested iterations; every element must hold.
fn eval_cond_iter<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    iters: &[ast::CondIter],
    eval: &mut impl FnMut(&mut RunnerContext<'_, PlInterp, Iface, Ext>, &Context<'_>) -> Backtrack<bool>,
) -> Backtrack<bool> {
    // Recurse from the outermost iteration inward
    let Some((exp_iter, iters_tail)) = iters.split_last() else {
        return eval(runner_ctx, ctx);
    };
    let ast::CondIter { iter, vars } = exp_iter;
    let vars_outer = iterate_vars(vars);
    match iter {
        // An option iterates zero or one time
        ast::Iter::Opt => {
            let values = unwrap_from_result!(
                ctx.find_opt_values_by_var(runner_ctx.arena(), &vars_outer),
                &Span::default()
            );
            // No element can refute a universally iterated condition
            let Some(values) = values else {
                return ok!(true);
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
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    iters: &[ast::InstrIter],
    eval: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
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
