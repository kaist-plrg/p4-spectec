//! Stamp fallible prose instructions with their failure destination
//!
//! A `Fallthrough` note says where control goes when an instruction
//! does not conclude: the next arm of a backtrack, the next rule group,
//! the otherwise block, or failure.
//! Only instructions that can fail, those that call or check something,
//! receive a note.
//!
//! For example, in a relation whose dispatch tries group `A` then group `B`
//! before an otherwise block, a fallible step in `A` is stamped `Group(B)`,
//! one in `B` is stamped `Else`, and without an otherwise block it is `Fail`.

use std::collections::HashMap;

use crate::lang::{
    pl::{ast as pl, rule_group},
    traits::has_call::HasCall,
};

/// Failure destination by rule-group name.
type Fallthroughs = HashMap<String, pl::Fallthrough>;

// == Failure classification

/// Reports whether an instruction can fail before entering nested blocks.
fn can_fail_instr(instr: &pl::Instr<pl::GroupInstr>) -> bool {
    match &instr.node.node {
        pl::InstrKind::If(pl::IfInstr { exp, .. }) => exp.has_call(),
        // Holds and checked lets always may fail
        pl::InstrKind::Hold(..) => true,
        pl::InstrKind::Case(pl::CaseInstr { exp, cases, .. }) => {
            exp.has_call() || cases.iter().any(|case| case.guard.has_call())
        }
        pl::InstrKind::Let(pl::LetInstr { exp_r, .. }) => exp_r.has_call(),
        pl::InstrKind::Debug(pl::DebugInstr { exp })
        | pl::InstrKind::Destruct(pl::DestructInstr { exp, .. }) => exp.has_call(),
        pl::InstrKind::CheckLetSub(..)
        | pl::InstrKind::CheckLetMatch(..)
        | pl::InstrKind::OptionGet(..) => true,
        pl::InstrKind::Tier(pl::TierInstr { tier }) => can_fail_group_instr(tier),
    }
}

/// Whether a group-tier instruction can fail: a call in its expressions.
fn can_fail_group_instr(instr: &pl::GroupInstr) -> bool {
    match instr {
        pl::GroupInstr::Rule(pl::RuleInstr { not_exp, .. }) => {
            not_exp.args().into_iter().any(HasCall::has_call)
        }
        pl::GroupInstr::Result(pl::ResultInstr { exps_output, .. }) => {
            exps_output.iter().any(HasCall::has_call)
        }
        pl::GroupInstr::Return(pl::ReturnInstr { exp }) => exp.has_call(),
        pl::GroupInstr::Backtrack(_) => false,
    }
}

// == Instruction stamping

// - Group instruction

/// Notes the instruction if it can fail, then recurses.
fn stamp_group_instr(
    fallthrough: &pl::Fallthrough,
    mut instr: pl::Instr<pl::GroupInstr>,
) -> pl::Instr<pl::GroupInstr> {
    // Only fallible instructions get a destination
    if can_fail_instr(&instr) {
        instr.node.note = Some(fallthrough.clone());
    }
    instr.node.node = stamp_group_instr_kind(fallthrough, instr.node.node);
    instr
}

/// Recurses into the blocks of a group-tier instruction.
fn stamp_group_instr_kind(
    fallthrough: &pl::Fallthrough,
    instr_kind: pl::InstrKind<pl::GroupInstr>,
) -> pl::InstrKind<pl::GroupInstr> {
    match instr_kind {
        pl::InstrKind::If(mut instr_if) => {
            instr_if.block = stamp_group_block(fallthrough, instr_if.block);
            pl::InstrKind::If(instr_if)
        }
        pl::InstrKind::Hold(mut instr_hold) => {
            instr_hold.hold_case = stamp_group_hold_case(fallthrough, instr_hold.hold_case);
            pl::InstrKind::Hold(instr_hold)
        }
        pl::InstrKind::Case(mut instr_case) => {
            for case in &mut instr_case.cases {
                case.block = stamp_group_block(fallthrough, std::mem::take(&mut case.block));
            }
            pl::InstrKind::Case(instr_case)
        }
        pl::InstrKind::CheckLetSub(mut instr_check) => {
            instr_check.block = stamp_group_block(fallthrough, instr_check.block);
            pl::InstrKind::CheckLetSub(instr_check)
        }
        pl::InstrKind::CheckLetMatch(mut instr_check) => {
            instr_check.block = stamp_group_block(fallthrough, instr_check.block);
            pl::InstrKind::CheckLetMatch(instr_check)
        }
        pl::InstrKind::OptionGet(mut instr_get) => {
            instr_get.block = stamp_group_block(fallthrough, instr_get.block);
            pl::InstrKind::OptionGet(instr_get)
        }
        pl::InstrKind::Tier(instr_tier) => {
            let tier = stamp_group_tier(fallthrough, instr_tier.tier);
            pl::InstrKind::Tier(pl::TierInstr { tier })
        }
        // Leaves have no blocks
        kind @ (pl::InstrKind::Let(_) | pl::InstrKind::Debug(_) | pl::InstrKind::Destruct(_)) => {
            kind
        }
    }
}

/// Stamps each instruction with the same destination.
fn stamp_group_block(fallthrough: &pl::Fallthrough, block: pl::GroupBlock) -> pl::GroupBlock {
    block
        .into_iter()
        .map(|instr| stamp_group_instr(fallthrough, instr))
        .collect()
}

// - Holding condition

/// Recurses into a hold's branches.
fn stamp_group_hold_case(
    fallthrough: &pl::Fallthrough,
    hold_case: pl::HoldCase<pl::GroupInstr>,
) -> pl::HoldCase<pl::GroupInstr> {
    match hold_case {
        pl::HoldCase::Both(block_hold, block_not_hold) => {
            let block_hold = stamp_group_block(fallthrough, block_hold);
            let block_not_hold = stamp_group_block(fallthrough, block_not_hold);
            pl::HoldCase::Both(block_hold, block_not_hold)
        }
        pl::HoldCase::Hold(block, dangle) => {
            let block = stamp_group_block(fallthrough, block);
            pl::HoldCase::Hold(block, dangle)
        }
        pl::HoldCase::NotHold(block, dangle) => {
            let block = stamp_group_block(fallthrough, block);
            pl::HoldCase::NotHold(block, dangle)
        }
    }
}

// - Group tier

/// Recurses into a backtrack; the other group instructions have no blocks.
fn stamp_group_tier(fallthrough: &pl::Fallthrough, instr_group: pl::GroupInstr) -> pl::GroupInstr {
    match instr_group {
        pl::GroupInstr::Backtrack(instr_backtrack) => {
            let instr_backtrack = stamp_backtrack_instr(fallthrough, instr_backtrack);
            pl::GroupInstr::Backtrack(instr_backtrack)
        }
        instr_group @ (pl::GroupInstr::Result(_)
        | pl::GroupInstr::Return(_)
        | pl::GroupInstr::Rule(_)) => instr_group,
    }
}

/// Each arm falls through to the next; the last arm to the outer destination.
fn stamp_backtrack_instr(
    fallthrough: &pl::Fallthrough,
    mut instr_backtrack: pl::BacktrackInstr,
) -> pl::BacktrackInstr {
    let idx_last = instr_backtrack.blocks.len().saturating_sub(1);
    let mut blocks = Vec::with_capacity(instr_backtrack.blocks.len());
    for (idx, block) in instr_backtrack.blocks.into_iter().enumerate() {
        // The last arm has no next arm
        let fallthrough_block =
            if idx < idx_last { pl::Fallthrough::Next } else { fallthrough.clone() };
        blocks.push(stamp_group_block(&fallthrough_block, block));
    }
    instr_backtrack.blocks = blocks;
    instr_backtrack
}

// - Dispatch instruction

/// Recurses into a dispatch-tier instruction; only group bodies get notes.
fn stamp_dispatch_instr(
    fallthroughs: &Fallthroughs,
    mut instr: pl::Instr<pl::DispatchInstr>,
) -> pl::Instr<pl::DispatchInstr> {
    instr.node.node = stamp_dispatch_instr_kind(fallthroughs, instr.node.node);
    instr
}

/// Recurses into the blocks of a dispatch-tier instruction.
fn stamp_dispatch_instr_kind(
    fallthroughs: &Fallthroughs,
    instr_kind: pl::InstrKind<pl::DispatchInstr>,
) -> pl::InstrKind<pl::DispatchInstr> {
    match instr_kind {
        pl::InstrKind::If(mut instr_if) => {
            instr_if.block = stamp_dispatch_block(fallthroughs, instr_if.block);
            pl::InstrKind::If(instr_if)
        }
        pl::InstrKind::Hold(mut instr_hold) => {
            instr_hold.hold_case = stamp_dispatch_hold_case(fallthroughs, instr_hold.hold_case);
            pl::InstrKind::Hold(instr_hold)
        }
        pl::InstrKind::Case(mut instr_case) => {
            for case in &mut instr_case.cases {
                case.block = stamp_dispatch_block(fallthroughs, std::mem::take(&mut case.block));
            }
            pl::InstrKind::Case(instr_case)
        }
        pl::InstrKind::CheckLetSub(mut instr_check) => {
            instr_check.block = stamp_dispatch_block(fallthroughs, instr_check.block);
            pl::InstrKind::CheckLetSub(instr_check)
        }
        pl::InstrKind::CheckLetMatch(mut instr_check) => {
            instr_check.block = stamp_dispatch_block(fallthroughs, instr_check.block);
            pl::InstrKind::CheckLetMatch(instr_check)
        }
        pl::InstrKind::OptionGet(mut instr_get) => {
            instr_get.block = stamp_dispatch_block(fallthroughs, instr_get.block);
            pl::InstrKind::OptionGet(instr_get)
        }
        pl::InstrKind::Tier(instr_tier) => {
            let tier = stamp_dispatch_tier(fallthroughs, instr_tier.tier);
            pl::InstrKind::Tier(pl::TierInstr { tier })
        }
        // Leaves have no blocks
        kind @ (pl::InstrKind::Let(_) | pl::InstrKind::Debug(_) | pl::InstrKind::Destruct(_)) => {
            kind
        }
    }
}

/// Stamps each instruction of a dispatch block.
fn stamp_dispatch_block(
    fallthroughs: &Fallthroughs,
    block: pl::DispatchBlock,
) -> pl::DispatchBlock {
    block
        .into_iter()
        .map(|instr| stamp_dispatch_instr(fallthroughs, instr))
        .collect()
}

// - Holding condition

/// Recurses into a hold's branches.
fn stamp_dispatch_hold_case(
    fallthroughs: &Fallthroughs,
    hold_case: pl::HoldCase<pl::DispatchInstr>,
) -> pl::HoldCase<pl::DispatchInstr> {
    match hold_case {
        pl::HoldCase::Both(block_hold, block_not_hold) => {
            let block_hold = stamp_dispatch_block(fallthroughs, block_hold);
            let block_not_hold = stamp_dispatch_block(fallthroughs, block_not_hold);
            pl::HoldCase::Both(block_hold, block_not_hold)
        }
        pl::HoldCase::Hold(block, dangle) => {
            let block = stamp_dispatch_block(fallthroughs, block);
            pl::HoldCase::Hold(block, dangle)
        }
        pl::HoldCase::NotHold(block, dangle) => {
            let block = stamp_dispatch_block(fallthroughs, block);
            pl::HoldCase::NotHold(block, dangle)
        }
    }
}

// - Tier instruction

/// Recurses into a route's arms or a rule group's body.
fn stamp_dispatch_tier(
    fallthroughs: &Fallthroughs,
    instr_dispatch: pl::DispatchInstr,
) -> pl::DispatchInstr {
    match instr_dispatch {
        pl::DispatchInstr::Route(instr_route) => {
            let instr_route = stamp_route_instr(fallthroughs, instr_route);
            pl::DispatchInstr::Route(instr_route)
        }
        pl::DispatchInstr::Group(instr_group) => {
            let instr_group = stamp_rulegroup_instr(fallthroughs, instr_group);
            pl::DispatchInstr::Group(instr_group)
        }
    }
}

/// Stamps every arm of a route.
fn stamp_route_instr(
    fallthroughs: &Fallthroughs,
    mut instr_route: pl::RouteInstr,
) -> pl::RouteInstr {
    let mut blocks = Vec::with_capacity(instr_route.blocks.len());
    for block in instr_route.blocks {
        blocks.push(stamp_dispatch_block(fallthroughs, block));
    }
    instr_route.blocks = blocks;
    instr_route
}

/// Stamps a group's body with the destination collected for that group.
fn stamp_rulegroup_instr(
    fallthroughs: &Fallthroughs,
    mut instr_group: pl::RuleGroupInstr,
) -> pl::RuleGroupInstr {
    let fallthrough = fallthroughs
        .get(&instr_group.id_group.node)
        .expect("every dispatched group has a failure destination");
    instr_group.block = stamp_group_block(fallthrough, instr_group.block);
    instr_group
}

// == Relation definitions

/// Stamps a defined relation.
fn stamp_rel_def(def_rel: pl::RelDef) -> pl::RelDef {
    match def_rel {
        pl::RelDef::Defined(def_rel) => {
            let def_rel = stamp_defined_rel_def(def_rel);
            pl::RelDef::Defined(def_rel)
        }
        pl::RelDef::Extern(def_rel) => pl::RelDef::Extern(def_rel),
    }
}

/// Computes each group's destination, then stamps the dispatch block.
fn stamp_defined_rel_def(mut def_rel: pl::DefinedRel) -> pl::DefinedRel {
    // A non-empty otherwise block is the final destination, else failure
    let fallthrough_final = if def_rel
        .block_else_opt
        .as_ref()
        .is_some_and(|block_else| !block_else.is_empty())
    {
        pl::Fallthrough::Else
    } else {
        pl::Fallthrough::Fail
    };
    let fallthroughs = collect_fallthroughs(fallthrough_final, &def_rel.block);
    def_rel.block = stamp_dispatch_block(&fallthroughs, def_rel.block);
    def_rel
}

/// Maps each rule group to the group tried after it fails.
///
/// Groups are gathered per route arm; an arm falls through to the first group
/// of the next arm, and the last arm to `fallthrough_final`.
fn collect_fallthroughs(
    fallthrough_final: pl::Fallthrough,
    block: &pl::DispatchBlock,
) -> Fallthroughs {
    // A block that is one route contributes an arm per route block
    let blocks = match block.as_slice() {
        [instr] => match &instr.node.node {
            pl::InstrKind::Tier(pl::TierInstr { tier: pl::DispatchInstr::Route(instr_route) }) => {
                instr_route.blocks.iter().collect::<Vec<_>>()
            }
            _ => vec![block],
        },
        _ => vec![block],
    };
    let mut fallthroughs = Fallthroughs::new();
    let mut fallthrough = fallthrough_final;
    let mut ids_group_by_block = Vec::new();
    for block in blocks {
        let rule_groups = rule_group::collect_rule_groups(block);
        let ids_group: Vec<&pl::Id> = rule_groups
            .iter()
            .map(|rule_group| rule_group.id_group)
            .collect();
        if !ids_group.is_empty() {
            ids_group_by_block.push(ids_group);
        }
    }
    // Walk arms backwards so each points at the following arm's first group
    for ids_group in ids_group_by_block.into_iter().rev() {
        fallthroughs.extend(
            ids_group
                .iter()
                .map(|id_group| (id_group.node.clone(), fallthrough.clone())),
        );
        let id_group_first = ids_group.first().expect("filtered empty groups");
        fallthrough = pl::Fallthrough::Group((**id_group_first).clone());
    }
    fallthroughs
}

// == Meta-function definitions

/// Stamps a defined function; the other kinds have no group blocks.
fn stamp_func_def(def_func: pl::MetaFuncDef) -> pl::MetaFuncDef {
    match def_func {
        pl::MetaFuncDef::Defined(def_func) => {
            let def_func = stamp_defined_func_def(def_func);
            pl::MetaFuncDef::Defined(def_func)
        }
        def_func @ (pl::MetaFuncDef::Extern(_)
        | pl::MetaFuncDef::Builtin(_)
        | pl::MetaFuncDef::Table(_)) => def_func,
    }
}

/// Stamps the body with the otherwise block or failure as destination.
fn stamp_defined_func_def(mut def_func: pl::DefinedFunc) -> pl::DefinedFunc {
    // A non-empty otherwise block is the destination, else failure
    let fallthrough = if def_func
        .block_else_opt
        .as_ref()
        .is_some_and(|block_else| !block_else.is_empty())
    {
        pl::Fallthrough::Else
    } else {
        pl::Fallthrough::Fail
    };
    def_func.block = stamp_group_block(&fallthrough, def_func.block);
    def_func
}

// == Definitions

/// Stamps one definition.
fn stamp_def(mut def: pl::Def) -> pl::Def {
    def.node.node = stamp_def_kind(def.node.node);
    def
}

/// Stamps relations and functions; types and variables have no blocks.
fn stamp_def_kind(def_kind: pl::DefKind) -> pl::DefKind {
    match def_kind {
        pl::DefKind::Rel(def_rel) => {
            let def_rel = stamp_rel_def(def_rel);
            pl::DefKind::Rel(def_rel)
        }
        pl::DefKind::MetaFunc(def_func) => {
            let def_func = stamp_func_def(def_func);
            pl::DefKind::MetaFunc(def_func)
        }
        kind @ (pl::DefKind::Typ(_) | pl::DefKind::Var(_)) => kind,
    }
}

// == Entry point

/// Stamps every definition.
pub(super) fn stamp_spec(spec: pl::Spec) -> pl::Spec {
    spec.into_iter().map(stamp_def).collect()
}
