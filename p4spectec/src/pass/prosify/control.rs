//! Normalize singleton cases and classify conditions by their continuations
//!
//! `classify_spec` turns singleton cases into conditions
//! and walks blocks backwards to distinguish `If` from `Check`
//!
//! For a definition body without an otherwise clause:
//!
//! ```text
//! If p { Return a }
//! If q { Return b }
//! ```
//!
//! becomes
//!
//! ```text
//! If p { Return a }
//! Check q { Return b }
//! ```
//!
//! The first condition can fail into the next instruction
//! The last condition must hold because no continuation remains
//! Later alternative arms and otherwise clauses also provide continuations
//! Other arms of a selected `Case` do not
//! Table matching blocks are left unchanged

use crate::lang::{common::prim, pl::ast as pl};

// == Guards

/// Converts a nonbinding guard into a boolean predicate.
fn exp_of_guard(exp_scrut: pl::Exp, guard: pl::Guard) -> pl::Exp {
    let span = exp_scrut.node.span;
    let kind = match guard {
        pl::Guard::Bool(true) => return exp_scrut,
        pl::Guard::Bool(false) => {
            let exp_false = crate::annotated_note_phrase! {
                node: pl::ExpKind::Bool(false), note: pl::TypKind::Bool, span: span,
            };
            pl::ExpKind::Cmp(
                pl::CmpOp::Bool(prim::bool::CmpOp::Eq),
                pl::OpTyp::Bool,
                Box::new(exp_scrut),
                Box::new(exp_false),
            )
        }
        pl::Guard::Cmp(op, op_typ, exp_r) => {
            pl::ExpKind::Cmp(op, op_typ, Box::new(exp_scrut), Box::new(exp_r))
        }
        pl::Guard::Sub(typ) => pl::ExpKind::Sub(Box::new(exp_scrut), typ),
        pl::Guard::Match(pattern) => pl::ExpKind::Match(Box::new(exp_scrut), pattern),
        pl::Guard::LetSub(..) | pl::Guard::LetMatch(..) => {
            unreachable!("conditional bindings are introduced after classification")
        }
        pl::Guard::Mem(exp_list) => pl::ExpKind::Mem(Box::new(exp_scrut), Box::new(exp_list)),
    };
    crate::annotated_note_phrase! { node: kind, note: pl::TypKind::Bool, span: span }
}

// == Instructions

fn classify_instr<Tier>(
    has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    mut instr: pl::Instr<Tier>,
) -> pl::Instr<Tier> {
    instr.node.node = classify_instr_kind(has_continuation, classify_tier, instr.node.node);
    instr
}

fn classify_instr_kind<Tier>(
    has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    instr_kind: pl::InstrKind<Tier>,
) -> pl::InstrKind<Tier> {
    match instr_kind {
        pl::InstrKind::IfLetSub(_) | pl::InstrKind::IfLetMatch(_) => {
            unreachable!("conditional bindings are introduced after classification")
        }
        pl::InstrKind::If(instr) => classify_if_instr(has_continuation, classify_tier, instr),
        pl::InstrKind::Check(instr) => classify_check_instr(has_continuation, classify_tier, instr),
        pl::InstrKind::Hold(instr) => classify_hold_instr(has_continuation, classify_tier, instr),
        pl::InstrKind::Case(instr) => classify_case_instr(has_continuation, classify_tier, instr),
        pl::InstrKind::Tier(instr) => classify_tier_instr(has_continuation, classify_tier, instr),
        instr_kind @ (pl::InstrKind::Let(_)
        | pl::InstrKind::Debug(_)
        | pl::InstrKind::Destruct(_)) => instr_kind,
    }
}

// - If instruction

/// Classifies the body and requires the condition when no continuation remains.
fn classify_if_instr<Tier>(
    has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    mut instr_if: pl::IfInstr<Tier>,
) -> pl::InstrKind<Tier> {
    instr_if.block = classify_block(has_continuation, classify_tier, instr_if.block);
    if has_continuation {
        pl::InstrKind::If(instr_if)
    } else {
        pl::InstrKind::Check(pl::CheckInstr {
            exp: instr_if.exp,
            iter_exps: instr_if.iter_exps,
            block: instr_if.block,
        })
    }
}

// - Check instruction

fn classify_check_instr<Tier>(
    has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    mut instr_check: pl::CheckInstr<Tier>,
) -> pl::InstrKind<Tier> {
    instr_check.block = classify_block(has_continuation, classify_tier, instr_check.block);
    pl::InstrKind::Check(instr_check)
}

// - Hold instruction

fn classify_hold_instr<Tier>(
    has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    mut instr_hold: pl::HoldInstr<Tier>,
) -> pl::InstrKind<Tier> {
    instr_hold.hold_case =
        classify_hold_case(has_continuation, classify_tier, instr_hold.hold_case);
    pl::InstrKind::Hold(instr_hold)
}

// - Case instruction

/// Normalizes singleton guards and classifies each selected case body.
fn classify_case_instr<Tier>(
    has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    mut instr_case: pl::CaseInstr<Tier>,
) -> pl::InstrKind<Tier> {
    // Normalize singleton guards before classifying their body once
    if instr_case.cases.len() == 1 {
        let case = instr_case.cases.pop().expect("singleton case");
        let instr_if = pl::IfInstr {
            exp: exp_of_guard(instr_case.exp, case.guard),
            iter_exps: vec![],
            block: case.block,
            dangle: instr_case.dangle,
        };
        return classify_if_instr(has_continuation, classify_tier, instr_if);
    }
    // Case selection commits to its first matching guard
    instr_case.cases = instr_case
        .cases
        .into_iter()
        .map(|case| classify_case(has_continuation, classify_tier, case))
        .collect();
    pl::InstrKind::Case(instr_case)
}

// - Tier instruction

fn classify_tier_instr<Tier>(
    has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    instr_tier: pl::TierInstr<Tier>,
) -> pl::InstrKind<Tier> {
    let tier = classify_tier(has_continuation, instr_tier.tier);
    pl::InstrKind::Tier(pl::TierInstr { tier })
}

// == Holding conditions

fn classify_hold_case<Tier>(
    has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    hold_case: pl::HoldCase<Tier>,
) -> pl::HoldCase<Tier> {
    match hold_case {
        pl::HoldCase::Both(block_hold, block_not_hold) => pl::HoldCase::Both(
            classify_block(has_continuation, classify_tier, block_hold),
            classify_block(has_continuation, classify_tier, block_not_hold),
        ),
        pl::HoldCase::Hold(block, dangle) => {
            pl::HoldCase::Hold(classify_block(has_continuation, classify_tier, block), dangle)
        }
        pl::HoldCase::NotHold(block, dangle) => {
            pl::HoldCase::NotHold(classify_block(has_continuation, classify_tier, block), dangle)
        }
    }
}

// == Cases

fn classify_case<Tier>(
    has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    mut case: pl::Case<Tier>,
) -> pl::Case<Tier> {
    case.block = classify_block(has_continuation, classify_tier, case.block);
    case
}

// == Group instructions

fn classify_group_tier(has_continuation: bool, tier: pl::GroupInstr) -> pl::GroupInstr {
    match tier {
        pl::GroupInstr::Backtrack(instr) => classify_backtrack_instr(has_continuation, instr),
        tier
        @ (pl::GroupInstr::Result(_) | pl::GroupInstr::Return(_) | pl::GroupInstr::Rule(_)) => tier,
    }
}

// - Backtrack instruction

fn classify_backtrack_instr(
    has_continuation: bool,
    mut instr_backtrack: pl::BacktrackInstr,
) -> pl::GroupInstr {
    instr_backtrack.blocks =
        classify_blocks(has_continuation, classify_group_tier, instr_backtrack.blocks);
    pl::GroupInstr::Backtrack(instr_backtrack)
}

// == Dispatch instructions

fn classify_dispatch_tier(has_continuation: bool, tier: pl::DispatchInstr) -> pl::DispatchInstr {
    match tier {
        pl::DispatchInstr::Group(instr) => classify_rule_group_instr(has_continuation, instr),
        pl::DispatchInstr::Route(instr) => classify_route_instr(has_continuation, instr),
    }
}

// - Rule group instruction

fn classify_rule_group_instr(
    has_continuation: bool,
    mut instr_group: pl::RuleGroupInstr,
) -> pl::DispatchInstr {
    instr_group.block = classify_group_block(has_continuation, instr_group.block);
    pl::DispatchInstr::Group(instr_group)
}

// - Route instruction

fn classify_route_instr(
    has_continuation: bool,
    mut instr_route: pl::RouteInstr,
) -> pl::DispatchInstr {
    instr_route.blocks =
        classify_blocks(has_continuation, classify_dispatch_tier, instr_route.blocks);
    pl::DispatchInstr::Route(instr_route)
}

// == Blocks

/// Visits a block backwards, counting every following instruction as a continuation.
fn classify_block<Tier>(
    mut has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    block: pl::Block<Tier>,
) -> pl::Block<Tier> {
    // Classify each instruction with all following instructions available
    let mut instrs = Vec::with_capacity(block.len());
    for instr in block.into_iter().rev() {
        instrs.push(classify_instr(has_continuation, classify_tier, instr));
        has_continuation = true;
    }
    // Restore source order after propagating continuations backwards
    instrs.reverse();
    instrs
}

/// Classifies alternatives with all later blocks included in their continuation.
fn classify_blocks<Tier>(
    mut has_continuation: bool,
    classify_tier: fn(bool, Tier) -> Tier,
    blocks: Vec<pl::Block<Tier>>,
) -> Vec<pl::Block<Tier>> {
    // Propagate the availability of later alternatives backwards
    let mut blocks_classified = Vec::with_capacity(blocks.len());
    for block in blocks.into_iter().rev() {
        blocks_classified.push(classify_block(has_continuation, classify_tier, block));
        has_continuation = true;
    }
    // Preserve alternative selection order
    blocks_classified.reverse();
    blocks_classified
}

fn classify_group_block(has_continuation: bool, block: pl::GroupBlock) -> pl::GroupBlock {
    classify_block(has_continuation, classify_group_tier, block)
}

fn classify_dispatch_block(has_continuation: bool, block: pl::DispatchBlock) -> pl::DispatchBlock {
    classify_block(has_continuation, classify_dispatch_tier, block)
}

// == Relation definitions

fn classify_rel_def(def_rel: pl::RelDef) -> pl::RelDef {
    match def_rel {
        pl::RelDef::Defined(def_rel) => pl::RelDef::Defined(classify_defined_rel_def(def_rel)),
        pl::RelDef::Extern(def_rel) => pl::RelDef::Extern(def_rel),
    }
}

fn classify_defined_rel_def(mut def_rel: pl::DefinedRel) -> pl::DefinedRel {
    def_rel.block = classify_dispatch_block(def_rel.block_else_opt.is_some(), def_rel.block);
    def_rel.block_else_opt = def_rel
        .block_else_opt
        .map(|block| classify_dispatch_block(false, block));
    def_rel
}

// == Meta-function definitions

fn classify_func_def(def_func: pl::MetaFuncDef) -> pl::MetaFuncDef {
    match def_func {
        pl::MetaFuncDef::Defined(def_func) => {
            pl::MetaFuncDef::Defined(classify_defined_func_def(def_func))
        }
        def_func @ (pl::MetaFuncDef::Extern(_)
        | pl::MetaFuncDef::Builtin(_)
        | pl::MetaFuncDef::Table(_)) => def_func,
    }
}

fn classify_defined_func_def(mut def_func: pl::DefinedFunc) -> pl::DefinedFunc {
    def_func.block = classify_group_block(def_func.block_else_opt.is_some(), def_func.block);
    def_func.block_else_opt = def_func
        .block_else_opt
        .map(|block| classify_group_block(false, block));
    def_func
}

// == Definitions

fn classify_def(mut def: pl::Def) -> pl::Def {
    def.node.node = classify_def_kind(def.node.node);
    def
}

fn classify_def_kind(def_kind: pl::DefKind) -> pl::DefKind {
    match def_kind {
        pl::DefKind::Rel(def_rel) => pl::DefKind::Rel(classify_rel_def(def_rel)),
        pl::DefKind::MetaFunc(def_func) => pl::DefKind::MetaFunc(classify_func_def(def_func)),
        def_kind @ (pl::DefKind::Typ(_) | pl::DefKind::Var(_)) => def_kind,
    }
}

// == Entry point

/// Classifies canonical definitions before rendering or extracting rule groups.
pub(super) fn classify_spec(spec: pl::Spec) -> pl::Spec {
    spec.into_iter().map(classify_def).collect()
}
