//! Merge equal conditions within runs of OL If instructions
//!
//! `find_identical_if` finds a later equal condition, even across other Ifs;
//! the two bodies are merged at the earlier instruction:
//!
//! ```text
//! if p { return a }; if q { return b }; if p { return c }
//!
//! becomes
//!
//! if p { return a; return c }; if q { return b }
//! ```
//!
//! The search stops at a non-If instruction and requires matching iterators.
//! `find_identical_if` inspects later siblings;
//! it does not rewrite their bodies.
//! `merge_block` merges into the current If
//! and retries before entering its body.
//! Conditions with unknown overlap remain separate.

use std::collections::VecDeque;

use crate::lang::traits::eq::SyntaxEq;

use crate::runtime::envs::algo::TDEnv;

use crate::pass::structure::{
    StructureError,
    ol::ast::*,
    opt::{
        merge,
        overlap::{Overlap, overlap_exp},
    },
};

// == Instructions

fn merge_instr_kind(
    tdenv: &TDEnv,
    changed: &mut bool,
    instrs_tail: &mut VecDeque<Instr>,
    instr_kind: InstrKind,
) -> Result<InstrKind, StructureError> {
    match instr_kind {
        InstrKind::If(instr) => merge_if_instr(tdenv, changed, instrs_tail, instr),
        InstrKind::Hold(instr) => merge_hold_instr(tdenv, changed, instr),
        InstrKind::Case(instr) => merge_case_instr(tdenv, changed, instr),
        InstrKind::Group(instr) => merge_group_instr(tdenv, changed, instr),
        InstrKind::Let(instr) => merge_let_instr(tdenv, changed, instr),
        InstrKind::Rule(instr) => merge_rule_instr(tdenv, changed, instr),
        InstrKind::Return(_) | InstrKind::Result(_) | InstrKind::Debug(_) => Ok(instr_kind),
    }
}

/// Rewrites a block, letting each If pull in later identical Ifs.
fn merge_block(tdenv: &TDEnv, changed: &mut bool, block: Block) -> Result<Block, StructureError> {
    let mut block_output = Vec::with_capacity(block.len());
    let mut instrs_tail = VecDeque::from(block);
    while let Some(instr) = instrs_tail.pop_front() {
        let instr_kind = merge_instr_kind(tdenv, changed, &mut instrs_tail, instr.node)?;
        let instr = crate::phrase!(node: instr_kind, span: instr.span);
        block_output.push(instr);
    }
    Ok(block_output)
}

// - If instruction

/// Finds the first later If with the same condition and iterators in the run.
fn find_identical_if(
    tdenv: &TDEnv,
    instr_target: &IfInstr,
    instrs_tail: &VecDeque<Instr>,
) -> Result<Option<usize>, StructureError> {
    let IfInstr { exp: exp_target, iter_exps: iter_exps_target, .. } = instr_target;
    // The search stops at the first non-If
    for (idx, instr) in instrs_tail.iter().enumerate() {
        let InstrKind::If(instr_if) = &instr.node else {
            break;
        };
        let IfInstr { exp, iter_exps, .. } = instr_if;
        let eq_iter_exps = iter_exps.syntax_eq(iter_exps_target);
        let overlap = overlap_exp(tdenv, exp_target, exp)?;
        if eq_iter_exps && matches!(overlap, Overlap::Identical) {
            return Ok(Some(idx));
        }
    }
    Ok(None)
}

/// Absorbs every identical later If, then rewrites the merged body.
fn merge_if_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instrs_tail: &mut VecDeque<Instr>,
    mut instr_if: IfInstr,
) -> Result<InstrKind, StructureError> {
    // Keep absorbing identical Ifs
    while let Some(idx) = find_identical_if(tdenv, &instr_if, instrs_tail)? {
        *changed = true;
        let instr_match = instrs_tail
            .remove(idx)
            .expect("matching instruction exists");
        let InstrKind::If(instr_match) = instr_match.node else { unreachable!() };
        let IfInstr { block: block_match, .. } = instr_match;
        instr_if.block = merge::merge_block(instr_if.block, block_match);
    }
    let IfInstr { exp, iter_exps, block } = instr_if;
    let block = merge_block(tdenv, changed, block)?;
    let instr = IfInstr { exp, iter_exps, block };
    Ok(InstrKind::If(instr))
}

// - Hold instruction

fn merge_hold_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instr: HoldInstr,
) -> Result<InstrKind, StructureError> {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr;
    let block_hold = merge_block(tdenv, changed, block_hold)?;
    let block_not_hold = merge_block(tdenv, changed, block_not_hold)?;
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    Ok(InstrKind::Hold(instr))
}

// - Case instruction

fn merge_case_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instr: CaseInstr,
) -> Result<InstrKind, StructureError> {
    let CaseInstr { exp, cases, total } = instr;
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            let block = merge_block(tdenv, changed, block)?;
            let case = Case { guard, block };
            Ok(case)
        })
        .collect::<Result<_, StructureError>>()?;
    let instr = CaseInstr { exp, cases, total };
    Ok(InstrKind::Case(instr))
}

// - Group instruction

fn merge_group_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instr: GroupInstr,
) -> Result<InstrKind, StructureError> {
    let GroupInstr { id, rel_signature, exps, block } = instr;
    let block = merge_block(tdenv, changed, block)?;
    let instr = GroupInstr { id, rel_signature, exps, block };
    Ok(InstrKind::Group(instr))
}

// - Let instruction

fn merge_let_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instr: LetInstr,
) -> Result<InstrKind, StructureError> {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr;
    let block = merge_block(tdenv, changed, block)?;
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    Ok(InstrKind::Let(instr))
}

// - Rule instruction

fn merge_rule_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instr: RuleInstr,
) -> Result<InstrKind, StructureError> {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr;
    let block = merge_block(tdenv, changed, block)?;
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    Ok(InstrKind::Rule(instr))
}

// == Entry point

/// Merges identical Ifs throughout the block, flagging `changed` on any merge.
pub(crate) fn apply(
    tdenv: &TDEnv,
    changed: &mut bool,
    block: Block,
) -> Result<Block, StructureError> {
    merge_block(tdenv, changed, block)
}
