//! Combine OL If and Case instructions into ordered case branches
//!
//! `casify_if_then_if` turns disjoint tests of the same value into a Case;
//! complementary tests also mark the Case as total:
//!
//! ```text
//! if x = true { return a }; if x = false { return b }
//!
//! becomes
//!
//! case x (total) { true => return a; false => return b }
//! ```
//!
//! Tests `x = 1` and `x = 2` on an integer form a partial Case instead.
//! `casify_from_if` and `casify_from_case` also combine existing Cases,
//! then `casify_block` retries the combined Case before entering its bodies.
//! Equal guards merge their bodies while preserving other branches:
//! `if x = 2 { A }; case x { 1 => B; 2 => C; 3 => D }`
//! becomes `case x { 1 => B; 2 => A; C; 3 => D }`.
//! Iterated Ifs and instructions other than If or Case stop the search.

use std::collections::VecDeque;

use crate::lang::traits::eq::SyntaxEq;

use crate::runtime::envs::algo::TDEnv;

use crate::pass::structure::{
    StructureError,
    ol::ast::*,
    opt::{
        merge::merge_block,
        overlap::{Overlap, exp_as_guard, overlap_exp, overlap_guard},
    },
};

// == Instructions

/// Rewrites a block, letting each If or Case combine with later siblings.
fn casify_block(tdenv: &TDEnv, changed: &mut bool, block: Block) -> Result<Block, StructureError> {
    let mut block_output = Vec::with_capacity(block.len());
    let mut instrs_tail = VecDeque::from(block);
    while let Some(instr) = instrs_tail.pop_front() {
        let instr_kind = casify_instr_kind(tdenv, changed, &mut instrs_tail, instr.node)?;
        let instr = crate::phrase!(node: instr_kind, span: instr.span);
        block_output.push(instr);
    }
    Ok(block_output)
}

fn casify_instr_kind(
    tdenv: &TDEnv,
    changed: &mut bool,
    instrs_tail: &mut VecDeque<Instr>,
    instr_kind: InstrKind,
) -> Result<InstrKind, StructureError> {
    match instr_kind {
        InstrKind::If(instr) => casify_if_instr(tdenv, changed, instrs_tail, instr),
        InstrKind::Hold(instr) => casify_hold_instr(tdenv, changed, instr),
        InstrKind::Case(instr) => casify_case_instr(tdenv, changed, instrs_tail, instr),
        InstrKind::Group(instr) => casify_group_instr(tdenv, changed, instr),
        InstrKind::Let(instr) => casify_let_instr(tdenv, changed, instr),
        InstrKind::Rule(instr) => casify_rule_instr(tdenv, changed, instr),
        InstrKind::Return(_) | InstrKind::Result(_) | InstrKind::Debug(_) => Ok(instr_kind),
    }
}

// - If instruction

/// Turns an If into a Case with a later If or Case if possible, then recurses.
fn casify_if_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instrs_tail: &mut VecDeque<Instr>,
    mut instr_if: IfInstr,
) -> Result<InstrKind, StructureError> {
    if let Some((idx, instr_case)) = casify_from_if(tdenv, &mut instr_if, instrs_tail)? {
        *changed = true;
        instrs_tail.remove(idx);
        return casify_case_instr(tdenv, changed, instrs_tail, instr_case);
    }
    let IfInstr { exp, iter_exps, block } = instr_if;
    let block = casify_block(tdenv, changed, block)?;
    let instr = IfInstr { exp, iter_exps, block };
    Ok(InstrKind::If(instr))
}

// - Hold instruction

fn casify_hold_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instr: HoldInstr,
) -> Result<InstrKind, StructureError> {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr;
    let block_hold = casify_block(tdenv, changed, block_hold)?;
    let block_not_hold = casify_block(tdenv, changed, block_not_hold)?;
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    Ok(InstrKind::Hold(instr))
}

// - Case instruction

/// Absorbs later Ifs and Cases on the same value, then recurses into branches.
fn casify_case_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instrs_tail: &mut VecDeque<Instr>,
    mut instr_case: CaseInstr,
) -> Result<InstrKind, StructureError> {
    // Keep absorbing while a later sibling combines
    while let Some((idx, instr_case_merged)) =
        casify_from_case(tdenv, &mut instr_case, instrs_tail)?
    {
        *changed = true;
        instrs_tail.remove(idx);
        instr_case = instr_case_merged;
    }
    let CaseInstr { exp, cases, total } = instr_case;
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            let block = casify_block(tdenv, changed, block)?;
            let case = Case { guard, block };
            Ok(case)
        })
        .collect::<Result<_, StructureError>>()?;
    let instr = CaseInstr { exp, cases, total };
    Ok(InstrKind::Case(instr))
}

// - Group instruction

fn casify_group_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instr: GroupInstr,
) -> Result<InstrKind, StructureError> {
    let GroupInstr { id, rel_signature, exps, block } = instr;
    let block = casify_block(tdenv, changed, block)?;
    let instr = GroupInstr { id, rel_signature, exps, block };
    Ok(InstrKind::Group(instr))
}

// - Let instruction

fn casify_let_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instr: LetInstr,
) -> Result<InstrKind, StructureError> {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr;
    let block = casify_block(tdenv, changed, block)?;
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    Ok(InstrKind::Let(instr))
}

// - Rule instruction

fn casify_rule_instr(
    tdenv: &TDEnv,
    changed: &mut bool,
    instr: RuleInstr,
) -> Result<InstrKind, StructureError> {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr;
    let block = casify_block(tdenv, changed, block)?;
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    Ok(InstrKind::Rule(instr))
}

// == Downstream search

/// Finds the first later If or Case that combines with this If.
///
/// Skipped Ifs and Cases stay in place.
fn casify_from_if(
    tdenv: &TDEnv,
    instr_target: &mut IfInstr,
    instrs_tail: &mut VecDeque<Instr>,
) -> Result<Option<(usize, CaseInstr)>, StructureError> {
    // Only un-iterated Ifs combine
    if !instr_target.iter_exps.is_empty() {
        return Ok(None);
    }
    for (idx, instr) in instrs_tail.iter_mut().enumerate() {
        let instr_case = match &mut instr.node {
            InstrKind::If(instr_if) if instr_if.iter_exps.is_empty() => {
                casify_if_then_if(tdenv, instr_target, instr_if)?
            }
            InstrKind::Case(instr_case) => casify_if_then_case(tdenv, instr_target, instr_case)?,
            // Anything but an If or Case stops the search
            _ => break,
        };
        if let Some(instr_case) = instr_case {
            return Ok(Some((idx, instr_case)));
        }
    }
    Ok(None)
}

/// Finds the first later If or Case that combines with this Case.
fn casify_from_case(
    tdenv: &TDEnv,
    instr_target: &mut CaseInstr,
    instrs_tail: &mut VecDeque<Instr>,
) -> Result<Option<(usize, CaseInstr)>, StructureError> {
    for (idx, instr) in instrs_tail.iter_mut().enumerate() {
        let instr_case = match &mut instr.node {
            InstrKind::If(instr_if) if instr_if.iter_exps.is_empty() => {
                casify_case_then_if(tdenv, instr_target, instr_if)?
            }
            InstrKind::Case(instr_case) => casify_case_then_case(tdenv, instr_target, instr_case)?,
            // Anything but an If or Case stops the search
            _ => break,
        };
        if let Some(instr_case) = instr_case {
            return Ok(Some((idx, instr_case)));
        }
    }
    Ok(None)
}

// == Combining conditions

// - If and If

/// Combines two Ifs on the same value into a two-branch Case.
fn casify_if_then_if(
    tdenv: &TDEnv,
    instr_target: &mut IfInstr,
    instr_if: &mut IfInstr,
) -> Result<Option<CaseInstr>, StructureError> {
    let overlap = overlap_exp(tdenv, &instr_target.exp, &instr_if.exp)?;
    let (exp, guard_a, guard_b, total) = match overlap {
        // x = 1 and x = 2 leave other integer values uncovered
        Overlap::Disjoint { exp, guard_a, guard_b } => (exp, guard_a, guard_b, false),
        // x = true and x = false cover both boolean values
        Overlap::Partition { exp, guard_a, guard_b } => (exp, guard_a, guard_b, true),
        Overlap::Identical | Overlap::Fuzzy => return Ok(None),
    };
    let block_a = std::mem::take(&mut instr_target.block);
    let case_a = Case { guard: guard_a, block: block_a };
    let block_b = std::mem::take(&mut instr_if.block);
    let case_b = Case { guard: guard_b, block: block_b };
    let cases = vec![case_a, case_b];
    let instr = CaseInstr { exp, cases, total };
    Ok(Some(instr))
}

// - If and Case

/// Merges an If into a Case: into an equal branch, or appended as a new branch.
fn casify_if_then_case(
    tdenv: &TDEnv,
    instr_target: &mut IfInstr,
    instr_case: &mut CaseInstr,
) -> Result<Option<CaseInstr>, StructureError> {
    let IfInstr { exp: exp_cond_target, block: block_target, .. } = instr_target;
    let CaseInstr { exp, cases, total } = instr_case;
    let Some(guard_target) = exp_as_guard(exp, exp_cond_target) else {
        return Ok(None);
    };
    for (idx, case) in cases.iter_mut().enumerate() {
        let Case { guard, block } = case;
        let overlap = overlap_guard(tdenv, exp, &guard_target, guard)?;
        match overlap {
            // An equal guard merges the bodies
            Overlap::Identical => {
                let block_target = std::mem::take(block_target);
                let block = std::mem::take(block);
                let block = merge_block(block_target, block);
                let mut cases = std::mem::take(cases);
                cases[idx].block = block;
                let instr = CaseInstr { exp: exp.clone(), cases, total: *total };
                return Ok(Some(instr));
            }
            Overlap::Disjoint { .. } | Overlap::Partition { .. } => {}
            Overlap::Fuzzy => return Ok(None),
        }
    }
    // Refuse the grouping before either input body is moved
    if *total {
        return Ok(None);
    }
    let mut cases = std::mem::take(cases);
    let block = std::mem::take(block_target);
    let case = Case { guard: guard_target, block };
    cases.push(case);
    let instr = CaseInstr { exp: exp.clone(), cases, total: *total };
    Ok(Some(instr))
}

// - Case and If

/// Merges a later If into a Case on the same value.
fn casify_case_then_if(
    tdenv: &TDEnv,
    instr_target: &mut CaseInstr,
    instr_if: &mut IfInstr,
) -> Result<Option<CaseInstr>, StructureError> {
    let CaseInstr { exp, cases, total } = instr_target;
    let IfInstr { exp: exp_cond, block, .. } = instr_if;
    let Some(guard) = exp_as_guard(exp, exp_cond) else {
        return Ok(None);
    };
    let Some(cases) = merge_case_and_if(tdenv, exp, *total, cases, guard, block)? else {
        return Ok(None);
    };
    // Case followed by If becomes partial, even when the Case was total
    let instr = CaseInstr { exp: exp.clone(), cases, total: false };
    Ok(Some(instr))
}

// - Case and Case

/// Merges a later Case on the same value branch by branch.
fn casify_case_then_case(
    tdenv: &TDEnv,
    instr_target: &mut CaseInstr,
    instr_case: &mut CaseInstr,
) -> Result<Option<CaseInstr>, StructureError> {
    let CaseInstr { exp: exp_target, cases: cases_target, total: total_target } = instr_target;
    let CaseInstr { exp, cases, .. } = instr_case;
    if !exp_target.syntax_eq(exp) {
        return Ok(None);
    }
    // A later unmergeable guard must leave both input bodies untouched
    // Place every later branch before moving anything
    let mut guards: Vec<_> = cases_target.iter().map(|case| &case.guard).collect();
    let mut idxs = Vec::with_capacity(cases.len());
    for case in cases.iter() {
        let Some(idx) =
            find_case_merge(tdenv, exp_target, guards.iter().copied(), *total_target, &case.guard)?
        else {
            return Ok(None);
        };
        // A new guard extends the targets for the following branches
        if idx == guards.len() {
            guards.push(&case.guard);
        }
        idxs.push(idx);
    }
    // Merge or append each branch at its place
    let mut cases_target = std::mem::take(cases_target);
    let cases = std::mem::take(cases);
    for (case, idx) in cases.into_iter().zip(idxs) {
        apply_case_merge(idx, case, &mut cases_target);
    }
    let instr = CaseInstr { exp: exp_target.clone(), cases: cases_target, total: *total_target };
    Ok(Some(instr))
}

// - Guard analysis and owned body merging

/// Places a guarded block among the cases: into an equal guard, or appended.
fn merge_case_and_if(
    tdenv: &TDEnv,
    exp_target: &Exp,
    total_target: bool,
    cases_target: &mut Vec<Case>,
    guard: Guard,
    block: &mut Block,
) -> Result<Option<Vec<Case>>, StructureError> {
    let Some(idx) = find_case_merge(
        tdenv,
        exp_target,
        cases_target.iter().map(|case| &case.guard),
        total_target,
        &guard,
    )?
    else {
        return Ok(None);
    };
    let mut cases = std::mem::take(cases_target);
    let block = std::mem::take(block);
    let case = Case { guard, block };
    apply_case_merge(idx, case, &mut cases);
    Ok(Some(cases))
}

/// Finds the branch with an equal guard, or the end position for appending.
///
/// A fuzzy overlap forbids the merge; a total Case forbids appending.
fn find_case_merge<'a>(
    tdenv: &TDEnv,
    exp_target: &Exp,
    guards_target: impl ExactSizeIterator<Item = &'a Guard>,
    total_target: bool,
    guard: &Guard,
) -> Result<Option<usize>, StructureError> {
    let guards_len = guards_target.len();
    for (idx, guard_target) in guards_target.enumerate() {
        match overlap_guard(tdenv, exp_target, guard_target, guard)? {
            Overlap::Identical => return Ok(Some(idx)),
            Overlap::Disjoint { .. } | Overlap::Partition { .. } => {}
            Overlap::Fuzzy => return Ok(None),
        }
    }
    // Refuse the grouping before either input body is moved
    if total_target {
        return Ok(None);
    }
    Ok(Some(guards_len))
}

/// Merges the case into the branch at `idx`, or appends it.
fn apply_case_merge(idx: usize, case: Case, cases: &mut Vec<Case>) {
    if idx == cases.len() {
        cases.push(case);
    } else {
        let block_target = std::mem::take(&mut cases[idx].block);
        cases[idx].block = merge_block(block_target, case.block);
    }
}

// == Entry point

/// Forms Cases throughout the block, flagging `changed` on any combination.
pub(crate) fn apply(
    tdenv: &TDEnv,
    changed: &mut bool,
    block: Block,
) -> Result<Block, StructureError> {
    casify_block(tdenv, changed, block)
}
