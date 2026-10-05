//! PL instruction, block, and alternative evaluation
//!
//! `eval_group_block` executes a rule group or function body;
//! `eval_dispatch_block` selects relation groups through routing instructions.
//! `eval_block` isolates bindings; `eval_alternatives` selects conclusions.
//! Blocks run sequentially; alternatives delegate selection to `pl::flow`.
//! `eval_instr` propagates failures to the enclosing block or invocation.
//! Expression and assignment adapters remove hints before shared evaluation.

use crate::lang::{
    common::source::Span,
    data::value::{Value, get},
    hints::input,
    traits::print::Print,
};

use crate::diagnostic::Report;

use crate::runtime::envs::interp::pl::ast_prepared as ast;

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

// = Block evaluation

/// Runs instructions sequentially, restoring bindings at the block boundary.
pub(super) fn eval_block<'global, 'instr, Tier: 'instr, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instrs: impl IntoIterator<Item = &'instr ast::Instr<Tier>>,
    mut evaluate: impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &ast::Instr<Tier>,
    ) -> Backtrack<(Context<'global>, Flow)>,
) -> Backtrack<(Context<'global>, Flow)> {
    // Assignments extend this block, not its enclosing scope
    let mut ctx_local = ctx.clone();
    let mut errors = Vec::new();
    // Pass each instruction's bindings to the next
    for instr in instrs {
        let (ctx_post, flow) = unwrap!(evaluate(runner_ctx, ctx_local, instr));
        ctx_local = ctx_post;
        match flow {
            // Continue with the most specific failure so far
            Flow::Cont(errors_post) => {
                flow::retain_deepest_errors(&mut errors, errors_post);
            }
            // A conclusion ends this execution path
            flow => return ok!((ctx, flow)),
        }
    }
    // Leaving the block restores the enclosing bindings
    ok!((ctx, Flow::Cont(errors)))
}

/// Runs a group body in its own local scope.
pub(super) fn eval_group_block<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    block: &ast::GroupBlock,
) -> Backtrack<(Context<'global>, Flow)> {
    eval_block(runner_ctx, ctx, block, eval_group_instr)
}

/// Runs relation routing in its own local scope.
pub(super) fn eval_dispatch_block<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    block: &ast::DispatchBlock,
) -> Backtrack<(Context<'global>, Flow)> {
    eval_block(runner_ctx, ctx, block, eval_dispatch_instr)
}

// - Alternative selection

/// Chooses between isolated alternatives in either execution mode.
fn eval_alternatives<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    blocks: &[ast::Block<Tier>],
    mut evaluate: impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &ast::Block<Tier>,
    ) -> Backtrack<(Context<'global>, Flow)>,
) -> Backtrack<(Context<'global>, Flow)> {
    let det = runner_ctx.interp().config.det;
    // Every alternative starts from the enclosing bindings
    let mut eval = |block| {
        let (_, flow) = unwrap!(evaluate(runner_ctx, ctx.clone(), block));
        ok!(flow)
    };
    let flow = if det {
        // Deterministic choice checks alternatives for conflicting conclusions
        unwrap!(flow::choose_deterministic(blocks, eval))
    } else {
        // A mismatch ends this alternative, but permits trying the next one
        unwrap!(flow::choose_sequential(blocks, |block| { Flow::cont_from_unmatch(eval(block)) }))
    };
    // Local alternative bindings never escape
    ok!((ctx, flow))
}

// = Instruction evaluation

/// Evaluates one instruction, returning any failure unchanged.
fn eval_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::Instr<Tier>,
    eval_tier: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &Tier,
    ) -> Backtrack<(Context<'global>, Flow)>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &ast::Block<Tier>,
    ) -> Backtrack<(Context<'global>, Flow)>,
) -> Backtrack<(Context<'global>, Flow)> {
    // Grow the stack for deep blocks
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || match &instr.node.node {
        ast::InstrKind::If(instr) => eval_if_instr(runner_ctx, ctx, instr, evaluate_block),
        ast::InstrKind::Hold(instr) => eval_hold_instr(runner_ctx, ctx, instr, evaluate_block),
        ast::InstrKind::Case(instr) => eval_case_instr(runner_ctx, ctx, instr, evaluate_block),
        ast::InstrKind::Let(instr) => eval_let_instr(runner_ctx, ctx, instr),
        ast::InstrKind::Debug(instr) => eval_debug_instr(runner_ctx, ctx, instr),
        ast::InstrKind::Destruct(instr) => eval_destruct_instr(runner_ctx, ctx, instr),
        ast::InstrKind::CheckLetSub(instr) => {
            eval_check_let_sub_instr(runner_ctx, ctx, instr, evaluate_block)
        }
        ast::InstrKind::CheckLetMatch(instr) => {
            eval_check_let_match_instr(runner_ctx, ctx, instr, evaluate_block)
        }
        ast::InstrKind::OptionGet(instr) => {
            eval_option_get_instr(runner_ctx, ctx, instr, evaluate_block)
        }
        ast::InstrKind::Tier(instr) => eval_tier(runner_ctx, ctx, &instr.tier),
    })
}

/// Evaluates one group instruction.
pub(super) fn eval_group_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::Instr<ast::GroupInstr>,
) -> Backtrack<(Context<'global>, Flow)> {
    eval_instr(
        runner_ctx,
        ctx,
        instr,
        &mut |runner_ctx, ctx, tier| eval_group_tier(runner_ctx, ctx, &instr.node.span, tier),
        &mut eval_group_block,
    )
}

fn eval_group_tier<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    span: &Span,
    tier: &ast::GroupInstr,
) -> Backtrack<(Context<'global>, Flow)> {
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
    ctx: Context<'global>,
    instr: &ast::Instr<ast::DispatchInstr>,
) -> Backtrack<(Context<'global>, Flow)> {
    eval_instr(runner_ctx, ctx, instr, &mut eval_dispatch_tier, &mut eval_dispatch_block)
}

fn eval_dispatch_tier<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    tier: &ast::DispatchInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    match tier {
        ast::DispatchInstr::Group(instr) => eval_rule_group_instr(runner_ctx, ctx, instr),
        ast::DispatchInstr::Route(instr) => eval_route_instr(runner_ctx, ctx, instr),
    }
}

// - If instruction

/// Runs the block when the condition holds under its iterations.
fn eval_if_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::IfInstr<Tier>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &ast::Block<Tier>,
    ) -> Backtrack<(Context<'global>, Flow)>,
) -> Backtrack<(Context<'global>, Flow)> {
    // Test the condition across all enclosing iterations
    let cond =
        unwrap!(eval_cond_iter(runner_ctx, &ctx, &instr.iter_exps, &mut |runner_ctx, ctx| {
            let value = unwrap!(eval_exp(runner_ctx, ctx, &instr.exp));
            ok!(get::bool(runner_ctx.arena(), &value).expect("condition must be a boolean"))
        }));
    // Run the block, or fall through recording the failed condition
    if cond {
        evaluate_block(runner_ctx, ctx, &instr.block)
    } else {
        ok!((
            ctx,
            Flow::cont(
                instr.exp.node.span.clone(),
                error::prem::condition_unmet(Print::to_string(&instr.exp))
            )
        ))
    }
}

// - Hold instruction

/// Runs the branch for whether the relation applies; a missing one continues.
fn eval_hold_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::HoldInstr<Tier>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &ast::Block<Tier>,
    ) -> Backtrack<(Context<'global>, Flow)>,
) -> Backtrack<(Context<'global>, Flow)> {
    // Keep the first failed relation call under the condition
    let mut errors = Vec::new();
    let cond =
        unwrap!(eval_cond_iter(runner_ctx, &ctx, &instr.iter_exps, &mut |runner_ctx, ctx| {
            let values = unwrap!(eval_exps(runner_ctx, ctx, &instr.not_exp.args()));
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
            evaluate_block(runner_ctx, ctx, if cond { block_hold } else { block_not })
        }
        // Only the matching branch present: run it
        ast::HoldCase::Hold(block, _) if cond => evaluate_block(runner_ctx, ctx, block),
        // Likewise for the not-hold branch
        ast::HoldCase::NotHold(block, _) if !cond => evaluate_block(runner_ctx, ctx, block),
        // A failed hold condition retains its relation failure
        ast::HoldCase::Hold(..) => {
            let diagnostic = error::prem::hold_condition_unmet(instr.id.node.clone());
            let report = Report::from(diagnostic)
                .with_span(&instr.id.span)
                .with_children(errors);
            ok!((ctx, Flow::Cont(vec![report])))
        }
        // A failed not-hold condition has no inner failure
        ast::HoldCase::NotHold(..) => ok!((
            ctx,
            Flow::cont(
                instr.id.span.clone(),
                error::prem::not_hold_condition_unmet(instr.id.node.clone()),
            )
        )),
    }
}

// - Case instruction

/// Runs the first matching case with its checked bindings kept local.
fn eval_case_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::CaseInstr<Tier>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &ast::Block<Tier>,
    ) -> Backtrack<(Context<'global>, Flow)>,
) -> Backtrack<(Context<'global>, Flow)> {
    // Evaluate the scrutinee once
    let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp));
    // Try guards in source order from the enclosing bindings
    for case in &instr.cases {
        if let Some(ctx_arm) = unwrap!(eval_guard(runner_ctx, ctx.clone(), value, &case.guard)) {
            // Keep guard bindings inside the selected case
            let (_, flow) = unwrap!(evaluate_block(runner_ctx, ctx_arm, &case.block));
            return ok!((ctx, flow));
        }
    }
    // No guard accepted: fall through
    ok!((
        ctx,
        Flow::cont(
            instr.exp.node.span.clone(),
            error::prem::condition_unmet(Print::to_string(&instr.exp))
        )
    ))
}

/// Checks a case guard and binds its pattern only when it matches.
fn eval_guard<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    value: Value,
    guard: &ast::Guard,
) -> Backtrack<Option<Context<'global>>> {
    // Test the scrutinee before introducing checked bindings
    let matched = match guard {
        // Compare the scrutinee with the expected boolean
        ast::Guard::Bool(expected) => ok!(get::bool(runner_ctx.arena(), &value)
            .expect("boolean guard value must be a boolean")
            == *expected),
        // Compare against the evaluated right side
        ast::Guard::Cmp(op, _, exp_r) => {
            let value_r = unwrap!(eval_exp(runner_ctx, &ctx, exp_r));
            ops::cmpop(runner_ctx.arena(), &exp_r.node.span, op, value, value_r)
        }
        // Check membership before introducing any binding
        ast::Guard::Sub(_, check) | ast::Guard::CheckLetSub(_, check, _) => {
            ops::sub(runner_ctx.arena(), &ctx, &Span::default(), check, value)
        }
        // Check the pattern before introducing any binding
        ast::Guard::Match(pattern) | ast::Guard::CheckLetMatch(pattern, _) => {
            ok!(ops::r#match(runner_ctx.arena(), pattern, value))
        }
        // Test membership in the evaluated list
        ast::Guard::Mem(exp_list) => {
            let value_list = unwrap!(eval_exp(runner_ctx, &ctx, exp_list));
            ops::mem(runner_ctx.arena(), &exp_list.node.span, value, value_list)
        }
    };
    let matched = unwrap!(matched);
    // A failed guard leaves the caller free to try another case
    if !matched {
        return ok!(None);
    }
    // Only checked guards extend the selected case
    let ctx = match guard {
        // Downcast and bind the checked value
        ast::Guard::CheckLetSub(typ, _, exp) => {
            let value = unwrap!(ops::cast_down(runner_ctx.arena_mut(), &ctx, typ, value));
            unwrap!(assign::assign_exp(runner_ctx.arena_mut(), ctx, exp, value))
        }
        // Bind the value accepted by the pattern
        ast::Guard::CheckLetMatch(_, exp) => {
            unwrap!(assign::assign_exp(runner_ctx.arena_mut(), ctx, exp, value))
        }
        // Ordinary guards do not extend the context
        _ => ctx,
    };
    ok!(Some(ctx))
}

// - Let instruction

/// Assigns under the binding iterators and continues with the new bindings.
fn eval_let_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::LetInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    // Extend the current block with bindings from each iteration
    let ctx =
        unwrap!(eval_instr_iter(runner_ctx, ctx, &instr.iter_instrs, &mut |runner_ctx, ctx| {
            let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp_r));
            assign::assign_exp(runner_ctx.arena_mut(), ctx, &instr.exp_l, value)
        }));
    ok!((ctx, Flow::Cont(vec![])))
}

// - Rule instruction

/// Calls the relation and binds its outputs under the binding iterators.
fn eval_rule_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::RuleInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    // The input hint separates arguments from output patterns
    let (exps_input, exps_output) = input::split(&instr.input_hint, instr.not_exp.args())
        .expect("input hint must fit relation");
    // Invoke the relation at each enclosing iteration
    let ctx =
        unwrap!(eval_instr_iter(runner_ctx, ctx, &instr.iter_instrs, &mut |runner_ctx, ctx| {
            let values = unwrap!(eval_exps(runner_ctx, &ctx, &exps_input));
            let values = unwrap!(PlInterp::invoke_rel(runner_ctx, &ctx, &instr.id, &values));
            // Extend the local bindings with relation outputs
            assign::assign_exps(runner_ctx.arena_mut(), ctx, &exps_output, &values)
        }));
    ok!((ctx, Flow::Cont(vec![])))
}

// - Result instruction

/// Evaluates the outputs and finishes the relation.
fn eval_result_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    span: &Span,
    instr: &ast::ResultInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    let values = unwrap!(eval_exps(runner_ctx, &ctx, &instr.exps_output));
    ok!((ctx, Flow::Result(phrase!(node: values, span: span.clone()))))
}

// - Return instruction

/// Evaluates the value and finishes the function.
fn eval_return_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    span: &Span,
    instr: &ast::ReturnInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp));
    ok!((ctx, Flow::Return(phrase!(node: value, span: span.clone()))))
}

// - Debug instruction

/// Prints the expression value and continues with the current bindings.
fn eval_debug_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::DebugInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    // Evaluate before printing so expression failures keep their trace
    let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp));
    println!("{}", runner_ctx.arena().to_string(&value));
    ok!((ctx, Flow::Cont(vec![])))
}

// - Destruct instruction

/// Binds the fields of a case value and continues with the new bindings.
fn eval_destruct_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::DestructInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    // Extract fields before mutating the arena during assignment
    let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp));
    let values = get::case(runner_ctx.arena(), &value)
        .expect("destructuring value must be a case")
        .args()
        .into_iter()
        .copied()
        .collect::<Vec<_>>();
    let exps = instr
        .bindings
        .iter()
        .map(|(_, exp)| exp)
        .collect::<Vec<_>>();
    // Bind each extracted field to its pattern
    let ctx = unwrap!(assign::assign_exps(runner_ctx.arena_mut(), ctx, &exps, &values));
    ok!((ctx, Flow::Cont(vec![])))
}

// - CheckLetSub instruction

/// Checks and downcasts a value before binding it in the nested block.
fn eval_check_let_sub_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::CheckLetSubInstr<Tier>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &ast::Block<Tier>,
    ) -> Backtrack<(Context<'global>, Flow)>,
) -> Backtrack<(Context<'global>, Flow)> {
    // Check membership before casting and binding the value
    let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp_r));
    let matches =
        unwrap!(ops::sub(runner_ctx.arena(), &ctx, &instr.exp_r.node.span, &instr.subcheck, value));
    // Cast only after the subtype check succeeds
    if matches {
        let value = unwrap!(ops::cast_down(runner_ctx.arena_mut(), &ctx, &instr.typ, value));
        // Propagate binding failures before entering the nested block
        let ctx_bound =
            unwrap!(assign::assign_exp(runner_ctx.arena_mut(), ctx.clone(), &instr.exp_l, value));
        let (_, flow) = unwrap!(evaluate_block(runner_ctx, ctx_bound, &instr.block));
        ok!((ctx, flow))
    } else {
        ok!((
            ctx,
            Flow::cont(
                instr.exp_r.node.span.clone(),
                error::prem::condition_unmet(format!(
                    "{} is not a subtype of {}",
                    Print::to_string(&instr.exp_r),
                    Print::to_string(&instr.typ)
                ))
            )
        ))
    }
}

// - CheckLetMatch instruction

/// Checks a value against its pattern before binding it in the nested block.
fn eval_check_let_match_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::CheckLetMatchInstr<Tier>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &ast::Block<Tier>,
    ) -> Backtrack<(Context<'global>, Flow)>,
) -> Backtrack<(Context<'global>, Flow)> {
    // Check the shape before assigning its pattern
    let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp_r));
    if ops::r#match(runner_ctx.arena(), &instr.pattern, value) {
        // The shorthand binding belongs to the nested block
        let ctx_bound =
            unwrap!(assign::assign_exp(runner_ctx.arena_mut(), ctx.clone(), &instr.exp_l, value));
        let (_, flow) = unwrap!(evaluate_block(runner_ctx, ctx_bound, &instr.block));
        ok!((ctx, flow))
    } else {
        ok!((
            ctx,
            Flow::cont(
                instr.exp_r.node.span.clone(),
                error::prem::condition_unmet(format!(
                    "{} does not match the expected pattern",
                    Print::to_string(&instr.exp_r)
                ))
            )
        ))
    }
}

// - OptionGet instruction

/// Binds a present option payload in the nested block; absence continues.
fn eval_option_get_instr<'global, Tier, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::OptionGetInstr<Tier>,
    evaluate_block: &mut impl FnMut(
        &mut RunnerContext<'_, PlInterp, Iface, Ext>,
        Context<'global>,
        &ast::Block<Tier>,
    ) -> Backtrack<(Context<'global>, Flow)>,
) -> Backtrack<(Context<'global>, Flow)> {
    // Only a present option enters the nested block
    let value = unwrap!(eval_exp(runner_ctx, &ctx, &instr.exp_r));
    if let Some(value) =
        get::opt(runner_ctx.arena(), &value).expect("option binding value must be an option")
    {
        // The shorthand binding belongs to the nested block
        let ctx_bound =
            unwrap!(assign::assign_exp(runner_ctx.arena_mut(), ctx.clone(), &instr.exp_l, value));
        let (_, flow) = unwrap!(evaluate_block(runner_ctx, ctx_bound, &instr.block));
        ok!((ctx, flow))
    } else {
        ok!((
            ctx,
            Flow::cont(
                instr.exp_r.node.span.clone(),
                error::prem::condition_unmet(format!(
                    "{} evaluated to an empty option",
                    Print::to_string(&instr.exp_r)
                ))
            )
        ))
    }
}

// - Backtrack instruction

/// Chooses a conclusion from isolated group-body alternatives.
fn eval_backtrack_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::BacktrackInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    eval_alternatives(runner_ctx, ctx, &instr.blocks, eval_group_block)
}

// - Rule group instruction

/// Runs a relation group in its own local scope.
fn eval_rule_group_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::RuleGroupInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    eval_group_block(runner_ctx, ctx, &instr.block)
}

// - Route instruction

/// Chooses a conclusion from isolated dispatch alternatives.
fn eval_route_instr<'global, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: Context<'global>,
    instr: &ast::RouteInstr,
) -> Backtrack<(Context<'global>, Flow)> {
    eval_alternatives(runner_ctx, ctx, &instr.blocks, eval_dispatch_block)
}

// = Iteration

// - Condition iteration

/// Evaluates a condition under nested iterations; every element must hold.
fn eval_cond_iter<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    iters: &[ast::ExpIter],
    eval: &mut impl FnMut(&mut RunnerContext<'_, PlInterp, Iface, Ext>, &Context<'_>) -> Backtrack<bool>,
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
