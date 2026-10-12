//! Classify conditions using the continuations of the complete prose tree
//!
//! A condition becomes Check only when failure has no remaining instruction,
//! alternative arm, or otherwise block in this definition. Calls are separate.

use crate::lang::pl::ast as pl;

/// Classifies a condition after classifying its nested body.
fn classify_if<Tier>(instr_if: pl::IfInstr<Tier>, has_continuation: bool) -> pl::InstrKind<Tier> {
    if has_continuation || !instr_if.iter_exps.is_empty() {
        pl::InstrKind::If(instr_if)
    } else {
        pl::InstrKind::Check(pl::CheckInstr {
            exp: instr_if.exp,
            iter_exps: instr_if.iter_exps,
            block: instr_if.block,
        })
    }
}

/// Visits a block backwards, counting every following instruction as a continuation.
fn classify_block<Tier>(
    block: &mut pl::Block<Tier>,
    mut has_continuation: bool,
    classify_tier: fn(&mut Tier, bool),
) {
    let mut instrs = Vec::with_capacity(block.len());
    for mut instr in std::mem::take(block).into_iter().rev() {
        instr.node.node = match instr.node.node {
            pl::InstrKind::If(mut instr_if) => {
                classify_block(&mut instr_if.block, has_continuation, classify_tier);
                classify_if(instr_if, has_continuation)
            }
            pl::InstrKind::Check(mut instr_check) => {
                classify_block(&mut instr_check.block, has_continuation, classify_tier);
                pl::InstrKind::Check(instr_check)
            }
            pl::InstrKind::Hold(mut instr_hold) => {
                match &mut instr_hold.hold_case {
                    pl::HoldCase::Both(block_hold, block_not_hold) => {
                        classify_block(block_hold, has_continuation, classify_tier);
                        classify_block(block_not_hold, has_continuation, classify_tier);
                    }
                    pl::HoldCase::Hold(block, _) | pl::HoldCase::NotHold(block, _) => {
                        classify_block(block, has_continuation, classify_tier);
                    }
                }
                pl::InstrKind::Hold(instr_hold)
            }
            pl::InstrKind::Case(mut instr_case) => {
                // Case selection commits to its first matching guard
                for case in &mut instr_case.cases {
                    classify_block(&mut case.block, has_continuation, classify_tier);
                }
                pl::InstrKind::Case(instr_case)
            }
            pl::InstrKind::Tier(mut instr_tier) => {
                classify_tier(&mut instr_tier.tier, has_continuation);
                pl::InstrKind::Tier(instr_tier)
            }
            kind @ (pl::InstrKind::Let(_)
            | pl::InstrKind::Debug(_)
            | pl::InstrKind::Destruct(_)) => kind,
        };
        instrs.push(instr);
        has_continuation = true;
    }
    instrs.reverse();
    *block = instrs;
}

/// Classifies alternatives with all later arms included in their continuation.
fn classify_arms<Tier>(
    blocks: &mut [pl::Block<Tier>],
    mut has_continuation: bool,
    classify_tier: fn(&mut Tier, bool),
) {
    for block in blocks.iter_mut().rev() {
        classify_block(block, has_continuation, classify_tier);
        has_continuation = true;
    }
}

/// Propagates group-level backtracking without analyzing called definitions.
fn classify_group_tier(tier: &mut pl::GroupInstr, has_continuation: bool) {
    if let pl::GroupInstr::Backtrack(instr) = tier {
        classify_arms(&mut instr.blocks, has_continuation, classify_group_tier);
    }
}

/// Carries dispatch continuations across the rule-group tier boundary.
fn classify_dispatch_tier(tier: &mut pl::DispatchInstr, has_continuation: bool) {
    match tier {
        pl::DispatchInstr::Group(instr) => classify_group_block(&mut instr.block, has_continuation),
        pl::DispatchInstr::Route(instr) => {
            classify_arms(&mut instr.blocks, has_continuation, classify_dispatch_tier);
        }
    }
}

/// Classifies a group body with its enclosing continuation.
fn classify_group_block(block: &mut pl::GroupBlock, has_continuation: bool) {
    classify_block(block, has_continuation, classify_group_tier);
}

/// Classifies a dispatch body with its enclosing continuation.
fn classify_dispatch_block(block: &mut pl::DispatchBlock, has_continuation: bool) {
    classify_block(block, has_continuation, classify_dispatch_tier);
}

/// Classifies canonical definitions before rendering or extracting rule groups.
pub(super) fn classify_spec(mut spec: pl::Spec) -> pl::Spec {
    for def in &mut spec {
        match &mut def.node.node {
            pl::DefKind::Rel(pl::RelDef::Defined(def_rel)) => {
                classify_dispatch_block(&mut def_rel.block, def_rel.block_else_opt.is_some());
                if let Some(block) = &mut def_rel.block_else_opt {
                    classify_dispatch_block(block, false);
                }
            }
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(def_func)) => {
                classify_group_block(&mut def_func.block, def_func.block_else_opt.is_some());
                if let Some(block) = &mut def_func.block_else_opt {
                    classify_group_block(block, false);
                }
            }
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Table(def_func)) => {
                for row in &mut def_func.rows {
                    classify_group_block(&mut row.block, false);
                }
            }
            _ => {}
        }
    }
    spec
}
