//! Normalize singleton cases and classify conditions by their continuations
//!
//! A condition becomes Check only when failure has no remaining instruction,
//! alternative arm, or otherwise block in this definition. Calls are separate.

use crate::lang::{common::prim, pl::ast as pl};

/// Converts a nonbinding guard into a boolean predicate.
fn guard_to_condition(exp_scrut: pl::Exp, guard: pl::Guard) -> pl::Exp {
    let span = exp_scrut.node.span.clone();
    let kind = match guard {
        pl::Guard::Bool(true) => return exp_scrut,
        pl::Guard::Bool(false) => {
            let exp_false = crate::annotated_note_phrase! {
                node: pl::ExpKind::Bool(false), note: pl::TypKind::Bool, span: span.clone(),
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
        pl::Guard::Sub(typ, subcheck) => pl::ExpKind::Sub(Box::new(exp_scrut), typ, subcheck),
        pl::Guard::Match(pattern) => pl::ExpKind::Match(Box::new(exp_scrut), pattern),
        pl::Guard::Mem(exp_list) => pl::ExpKind::Mem(Box::new(exp_scrut), Box::new(exp_list)),
    };
    crate::annotated_note_phrase! { node: kind, note: pl::TypKind::Bool, span: span }
}

/// Classifies a condition after classifying its nested body.
fn classify_if<Tier>(instr_if: pl::IfInstr<Tier>, has_continuation: bool) -> pl::InstrKind<Tier> {
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
                // Normalize singleton guards within the classification traversal
                if instr_case.cases.len() == 1 {
                    let case = instr_case.cases.pop().expect("singleton case");
                    classify_if(
                        pl::IfInstr {
                            exp: guard_to_condition(instr_case.exp, case.guard),
                            iter_exps: vec![],
                            block: case.block,
                            dangle: instr_case.dangle,
                        },
                        has_continuation,
                    )
                } else {
                    pl::InstrKind::Case(instr_case)
                }
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
                let mut has_continuation = false;
                for row in def_func.rows.iter_mut().rev() {
                    classify_group_block(&mut row.block, has_continuation);
                    has_continuation |= !row.block.is_empty();
                }
            }
            _ => {}
        }
    }
    spec
}
