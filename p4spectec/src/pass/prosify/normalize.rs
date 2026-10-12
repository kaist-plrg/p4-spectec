//! Normalize singleton case analyses before classifying prose conditions
//!
//! Every guard becomes a predicate containing its scrutinee exactly once.
//! The arm remains a nested block, preserving evaluation order and bindings.

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

/// Normalizes shared control flow, delegating only tier-specific nesting.
fn normalize_block<Tier>(block: &mut pl::Block<Tier>, normalize_tier: fn(&mut Tier)) {
    for instr in block.iter_mut() {
        match &mut instr.node.node {
            pl::InstrKind::If(instr_if) => normalize_block(&mut instr_if.block, normalize_tier),
            pl::InstrKind::Check(instr_check) => {
                normalize_block(&mut instr_check.block, normalize_tier)
            }
            pl::InstrKind::Hold(instr_hold) => match &mut instr_hold.hold_case {
                pl::HoldCase::Both(block_hold, block_not_hold) => {
                    normalize_block(block_hold, normalize_tier);
                    normalize_block(block_not_hold, normalize_tier);
                }
                pl::HoldCase::Hold(block, _) | pl::HoldCase::NotHold(block, _) => {
                    normalize_block(block, normalize_tier);
                }
            },
            pl::InstrKind::Case(instr_case) => {
                for case in &mut instr_case.cases {
                    normalize_block(&mut case.block, normalize_tier);
                }
            }
            pl::InstrKind::Tier(instr_tier) => normalize_tier(&mut instr_tier.tier),
            pl::InstrKind::Let(_) | pl::InstrKind::Debug(_) | pl::InstrKind::Destruct(_) => {}
        }
    }
    // Move the singleton's expression and block without cloning either
    *block = std::mem::take(block)
        .into_iter()
        .map(|mut instr| {
            instr.node.node = match instr.node.node {
                pl::InstrKind::Case(mut instr_case) if instr_case.cases.len() == 1 => {
                    let case = instr_case.cases.pop().expect("singleton case");
                    pl::InstrKind::If(pl::IfInstr {
                        exp: guard_to_condition(instr_case.exp, case.guard),
                        iter_exps: vec![],
                        block: case.block,
                        dangle: instr_case.dangle,
                    })
                }
                kind => kind,
            };
            instr
        })
        .collect();
}

/// Normalizes all alternatives in a group instruction.
fn normalize_group_tier(tier: &mut pl::GroupInstr) {
    if let pl::GroupInstr::Backtrack(instr) = tier {
        for block in &mut instr.blocks {
            normalize_block(block, normalize_group_tier);
        }
    }
}

/// Normalizes routes and forwards into each group's body.
fn normalize_dispatch_tier(tier: &mut pl::DispatchInstr) {
    match tier {
        pl::DispatchInstr::Group(instr) => normalize_block(&mut instr.block, normalize_group_tier),
        pl::DispatchInstr::Route(instr) => {
            for block in &mut instr.blocks {
                normalize_block(block, normalize_dispatch_tier);
            }
        }
    }
}

/// Normalizes every conditional before continuation analysis.
pub(super) fn normalize_spec(mut spec: pl::Spec) -> pl::Spec {
    for def in &mut spec {
        match &mut def.node.node {
            pl::DefKind::Rel(pl::RelDef::Defined(def_rel)) => {
                normalize_block(&mut def_rel.block, normalize_dispatch_tier);
                if let Some(block) = &mut def_rel.block_else_opt {
                    normalize_block(block, normalize_dispatch_tier);
                }
            }
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Defined(def_func)) => {
                normalize_block(&mut def_func.block, normalize_group_tier);
                if let Some(block) = &mut def_func.block_else_opt {
                    normalize_block(block, normalize_group_tier);
                }
            }
            pl::DefKind::MetaFunc(pl::MetaFuncDef::Table(def_func)) => {
                for row in &mut def_func.rows {
                    normalize_block(&mut row.block, normalize_group_tier);
                }
            }
            _ => {}
        }
    }
    spec
}
