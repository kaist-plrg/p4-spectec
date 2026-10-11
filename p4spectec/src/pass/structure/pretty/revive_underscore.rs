//! Give used underscore-prefixed inputs and OL binders ordinary names
//!
//! `candid_renamer` strips leading underscores and chooses fresh names;
//! `downstream_block` records which candidates are actually used.
//! When `x` is available:
//!
//! ```text
//! let _x = source { return _x }
//!
//! becomes
//!
//! let x = source { return x }
//! ```
//!
//! An unused `_x` keeps its name; a conflicting `x` forces a fresh name.
//! `apply_rel` and `apply_func` also check uses of inputs in the fallback,
//! while nested bindings of the same underscore name stop substitution.

use std::rc::Rc;

use crate::lang::{
    common::ds::set::IdSet, data::notation::tree as notation, hints::input, traits::free::FreeIds,
};

use crate::lang::il::{
    ast::{Arg, Exp},
    fresh,
};

use crate::pass::structure::{ol::ast::*, re::renamer::Renamer};

// == Candidate names

/// Keeps the underscore-prefixed identifiers.
fn underscores(frees: IdSet) -> IdSet {
    frees
        .iter()
        .filter(|id| id.node.starts_with('_'))
        .cloned()
        .collect()
}

/// Proposes an underscore-free fresh name for each binder in `ids`.
fn candid_renamer(mut frees: IdSet, ids: &IdSet) -> (IdSet, Renamer) {
    let mut renamer = Renamer::empty();
    for id in ids.iter() {
        let mut id_strip = id.clone();
        id_strip.node = id_strip.node.trim_start_matches('_').to_owned();
        let id_revive = fresh::id(&frees, &id_strip);
        frees.insert(id_revive.clone());
        renamer.add(id.clone(), id_revive);
    }
    (frees, renamer)
}

// == Downstream uses

// - Expressions

/// Renames candidate uses in an expression, recording which were used.
fn downstream_exp(renamer: &Renamer, changed: &mut bool, ids_revive: &mut IdSet, exp: Exp) -> Exp {
    let ids_used = underscores(exp.free_ids());
    ids_revive.append(renamer.dom().intersection(&ids_used));
    renamer.rename_exp(changed, exp)
}

fn downstream_exps(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    exps: Vec<Exp>,
) -> Vec<Exp> {
    let ids_used = underscores(exps.as_slice().free_ids());
    ids_revive.append(renamer.dom().intersection(&ids_used));
    renamer.rename_exps(changed, exps)
}

// - Instructions

/// Renames candidate uses in an instruction, recording which were used.
///
/// With `_x -> x`, `return _x` becomes `return x`
/// and adds `_x` to `ids_revive`;
/// a nested `let _x = value { return _x }` keeps its own `_x` inside the body.
fn downstream_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: Instr,
) -> Instr {
    let instr_kind_ol = downstream_instr_kind(renamer, changed, ids_revive, instr_ol.node);
    crate::phrase!(node: instr_kind_ol, span: instr_ol.span)
}

fn downstream_instr_kind(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_kind_ol: InstrKind,
) -> InstrKind {
    match instr_kind_ol {
        InstrKind::If(instr_ol) => downstream_if_instr(renamer, changed, ids_revive, instr_ol),
        InstrKind::Hold(instr_ol) => downstream_hold_instr(renamer, changed, ids_revive, instr_ol),
        InstrKind::Case(instr_ol) => downstream_case_instr(renamer, changed, ids_revive, instr_ol),
        InstrKind::Group(instr_ol) => {
            downstream_group_instr(renamer, changed, ids_revive, instr_ol)
        }
        InstrKind::Let(instr_ol) => downstream_let_instr(renamer, changed, ids_revive, instr_ol),
        InstrKind::Rule(instr_ol) => downstream_rule_instr(renamer, changed, ids_revive, instr_ol),
        InstrKind::Result(instr_ol) => {
            downstream_result_instr(renamer, changed, ids_revive, instr_ol)
        }
        InstrKind::Return(instr_ol) => {
            downstream_return_instr(renamer, changed, ids_revive, instr_ol)
        }
        InstrKind::Debug(instr_ol) => {
            downstream_debug_instr(renamer, changed, ids_revive, instr_ol)
        }
    }
}

fn downstream_block(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    block: Block,
) -> Block {
    block
        .into_iter()
        .map(|instr_ol| downstream_instr(renamer, changed, ids_revive, instr_ol))
        .collect()
}

// - If instruction

fn downstream_if_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: IfInstr,
) -> InstrKind {
    let IfInstr { exp, iter_exps, block } = instr_ol;
    let exp = downstream_exp(renamer, changed, ids_revive, exp);
    let iter_exps = renamer.rename_iterexps(changed, iter_exps);
    let block = downstream_block(renamer, changed, ids_revive, block);
    let instr = IfInstr { exp, iter_exps, block };
    InstrKind::If(instr)
}

// - Hold instruction

fn downstream_hold_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: HoldInstr,
) -> InstrKind {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr_ol;
    let not_exp = not_exp.map(|exp| downstream_exp(renamer, changed, ids_revive, exp.clone()));
    let iter_exps = renamer.rename_iterexps(changed, iter_exps);
    let block_hold = downstream_block(renamer, changed, ids_revive, block_hold);
    let block_not_hold = downstream_block(renamer, changed, ids_revive, block_not_hold);
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    InstrKind::Hold(instr)
}

// - Case instruction

fn downstream_case_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: CaseInstr,
) -> InstrKind {
    let CaseInstr { exp, cases, total } = instr_ol;
    let exp = downstream_exp(renamer, changed, ids_revive, exp);
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            // Guards may use candidates too
            let ids_used = underscores(guard.free_ids());
            ids_revive.append(renamer.dom().intersection(&ids_used));
            let guard = renamer.rename_guard(changed, guard);
            let block = downstream_block(renamer, changed, ids_revive, block);
            Case { guard, block }
        })
        .collect();
    let instr = CaseInstr { exp, cases, total };
    InstrKind::Case(instr)
}

// - Group instruction

fn downstream_group_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: GroupInstr,
) -> InstrKind {
    let GroupInstr { id, rel_signature, exps, block } = instr_ol;
    let exps = downstream_exps(renamer, changed, ids_revive, exps);
    let block = downstream_block(renamer, changed, ids_revive, block);
    let instr = GroupInstr { id, rel_signature, exps, block };
    InstrKind::Group(instr)
}

// - Let instruction

/// Renames source and body; a rebound underscore name shadows its candidate.
fn downstream_let_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: LetInstr,
) -> InstrKind {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
    let exp_r = downstream_exp(renamer, changed, ids_revive, exp_r);
    let iter_instrs = renamer.rename_iterinstrs_bound(changed, iter_instrs);
    // The RHS uses the outer _x; the body uses the newly bound _x
    let ids_bound = underscores(exp_l.free_ids());
    let renamer = renamer.filter(|id, _| !ids_bound.contains(id));
    let block = downstream_block(&renamer, changed, ids_revive, block);
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    InstrKind::Let(instr)
}

// - Rule instruction

/// Renames inputs and body; rebound underscore outputs shadow the candidate.
fn downstream_rule_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: RuleInstr,
) -> InstrKind {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr_ol;
    let exps = not_exp.args().to_vec();
    // Elaboration validates hints; OL rewrites preserve notation arity
    let (exps_input, exps_output) =
        input::split(&input_hint, exps).expect("validated relation hints and argument counts");
    // Inputs are uses; the outputs bind their own underscore names
    let exps_input = downstream_exps(renamer, changed, ids_revive, exps_input);
    let ids_bound = underscores(exps_output.as_slice().free_ids());
    // Renaming preserves the argument counts returned by input::split
    let exps = input::combine(&input_hint, exps_input, exps_output)
        .expect("validated relation hints and argument counts");
    let not_exp = notation::Mixfix::new(Rc::clone(not_exp.mixop()), exps)
        .expect("validated arguments preserve the mixfix arity");
    let iter_instrs = renamer.rename_iterinstrs_bound(changed, iter_instrs);
    let renamer = renamer.filter(|id, _| !ids_bound.contains(id));
    let block = downstream_block(&renamer, changed, ids_revive, block);
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    InstrKind::Rule(instr)
}

// - Result instruction

fn downstream_result_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: ResultInstr,
) -> InstrKind {
    let ResultInstr { rel_signature, exps } = instr_ol;
    let exps = downstream_exps(renamer, changed, ids_revive, exps);
    let instr = ResultInstr { rel_signature, exps };
    InstrKind::Result(instr)
}

// - Return instruction

fn downstream_return_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: ReturnInstr,
) -> InstrKind {
    let ReturnInstr { exp } = instr_ol;
    let exp = downstream_exp(renamer, changed, ids_revive, exp);
    let instr = ReturnInstr { exp };
    InstrKind::Return(instr)
}

// - Debug instruction

fn downstream_debug_instr(
    renamer: &Renamer,
    changed: &mut bool,
    ids_revive: &mut IdSet,
    instr_ol: DebugInstr,
) -> InstrKind {
    let DebugInstr { exp, instr } = instr_ol;
    let exp = downstream_exp(renamer, changed, ids_revive, exp);
    let instr = downstream_instr(renamer, changed, ids_revive, *instr);
    let instr = Box::new(instr);
    let instr = DebugInstr { exp, instr };
    InstrKind::Debug(instr)
}

// == Upstream bindings

/// Revives the underscore binders of an instruction that its body uses.
///
/// `let (_x, _y) = source { return _x }`
/// becomes `let (x, _y) = source { return x }` when `x` is available.
fn upstream_instr(changed: &mut bool, frees: &IdSet, instr_ol: Instr) -> Instr {
    let instr_kind_ol = upstream_instr_kind(changed, frees, instr_ol.node);
    crate::phrase!(node: instr_kind_ol, span: instr_ol.span)
}

fn upstream_instr_kind(changed: &mut bool, frees: &IdSet, instr_kind_ol: InstrKind) -> InstrKind {
    match instr_kind_ol {
        InstrKind::If(instr_ol) => upstream_if_instr(changed, frees, instr_ol),
        InstrKind::Hold(instr_ol) => upstream_hold_instr(changed, frees, instr_ol),
        InstrKind::Case(instr_ol) => upstream_case_instr(changed, frees, instr_ol),
        InstrKind::Group(instr_ol) => upstream_group_instr(changed, frees, instr_ol),
        InstrKind::Let(instr_ol) => upstream_let_instr(changed, frees, instr_ol),
        InstrKind::Rule(instr_ol) => upstream_rule_instr(changed, frees, instr_ol),
        _ => instr_kind_ol,
    }
}

fn upstream_block(changed: &mut bool, frees: &IdSet, block: Block) -> Block {
    block
        .into_iter()
        .map(|instr_ol| upstream_instr(changed, frees, instr_ol))
        .collect()
}

// - If instruction

fn upstream_if_instr(changed: &mut bool, frees: &IdSet, instr_ol: IfInstr) -> InstrKind {
    let IfInstr { exp, iter_exps, block } = instr_ol;
    let block = upstream_block(changed, frees, block);
    let instr = IfInstr { exp, iter_exps, block };
    InstrKind::If(instr)
}

// - Hold instruction

fn upstream_hold_instr(changed: &mut bool, frees: &IdSet, instr_ol: HoldInstr) -> InstrKind {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr_ol;
    let block_hold = upstream_block(changed, frees, block_hold);
    let block_not_hold = upstream_block(changed, frees, block_not_hold);
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    InstrKind::Hold(instr)
}

// - Case instruction

fn upstream_case_instr(changed: &mut bool, frees: &IdSet, instr_ol: CaseInstr) -> InstrKind {
    let CaseInstr { exp, cases, total } = instr_ol;
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            let block = upstream_block(changed, frees, block);
            Case { guard, block }
        })
        .collect();
    let instr = CaseInstr { exp, cases, total };
    InstrKind::Case(instr)
}

// - Group instruction

fn upstream_group_instr(changed: &mut bool, frees: &IdSet, instr_ol: GroupInstr) -> InstrKind {
    let GroupInstr { id, rel_signature, exps, block } = instr_ol;
    let block = upstream_block(changed, frees, block);
    let instr = GroupInstr { id, rel_signature, exps, block };
    InstrKind::Group(instr)
}

// - Let instruction

/// Proposes names for the let's binders, keeps the used ones, and renames.
fn upstream_let_instr(changed: &mut bool, frees: &IdSet, instr_ol: LetInstr) -> InstrKind {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
    // Propose names for the binders, keep those the body used
    let ids_bound = underscores(exp_l.free_ids());
    let (_, renamer) = candid_renamer(frees.clone(), &ids_bound);
    let mut ids_revive = IdSet::new();
    let block = downstream_block(&renamer, changed, &mut ids_revive, block);
    let renamer = renamer.filter(|id, _| ids_revive.contains(id));
    let exp_l = renamer.rename_exp(changed, exp_l);
    let iter_instrs = renamer.rename_iterinstrs_bind(changed, iter_instrs);
    let block = renamer.rename_block(changed, block);
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    InstrKind::Let(instr)
}

// - Rule instruction

/// Proposes names for the rule call's outputs, keeps used ones, and renames.
fn upstream_rule_instr(changed: &mut bool, frees: &IdSet, instr_ol: RuleInstr) -> InstrKind {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr_ol;
    let exps = not_exp.args().to_vec();
    // Elaboration validates hints; OL rewrites preserve notation arity
    let (exps_input, exps_output) =
        input::split(&input_hint, exps).expect("validated relation hints and argument counts");
    // Propose names for the output binders, keep those the body used
    let ids_bound = underscores(exps_output.as_slice().free_ids());
    let (_, renamer) = candid_renamer(frees.clone(), &ids_bound);
    let mut ids_revive = IdSet::new();
    let block = downstream_block(&renamer, changed, &mut ids_revive, block);
    let renamer = renamer.filter(|id, _| ids_revive.contains(id));
    let exps_output = renamer.rename_exps(changed, exps_output);
    // Renaming preserves the argument counts returned by input::split
    let exps = input::combine(&input_hint, exps_input, exps_output)
        .expect("validated relation hints and argument counts");
    let not_exp = notation::Mixfix::new(Rc::clone(not_exp.mixop()), exps)
        .expect("validated arguments preserve the mixfix arity");
    let iter_instrs = renamer.rename_iterinstrs_bind(changed, iter_instrs);
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    InstrKind::Rule(instr)
}

// == Entry points

/// Revives the used underscore inputs of a relation, then its binders.
///
/// An input `_x` used only in `else { return _x }` is also renamed to `x`.
pub(crate) fn apply_rel(
    changed: &mut bool,
    (mut exps_match, mut block, mut block_else): (Vec<Exp>, Block, Option<Block>),
) -> (Vec<Exp>, Block, Option<Block>) {
    let frees_input = exps_match.as_slice().free_ids();
    let ids_bound = underscores(frees_input.clone());
    // Propose names for the underscore inputs, avoiding names in either block
    let frees_else = block_else
        .as_ref()
        .map(FreeIds::free_ids)
        .unwrap_or_default();
    let frees = frees_input.union(block.free_ids()).union(frees_else);
    let (frees, renamer) = candid_renamer(frees, &ids_bound);
    let mut ids_revive = IdSet::new();
    block = downstream_block(&renamer, changed, &mut ids_revive, block);
    block_else =
        block_else.map(|block| downstream_block(&renamer, changed, &mut ids_revive, block));
    // Uses in both blocks decide which inputs revive; then revive the binders
    let renamer = renamer.filter(|id, _| ids_revive.contains(id));
    exps_match = renamer.rename_exps(changed, exps_match);
    block = upstream_block(changed, &frees, block);
    block_else = block_else.map(|block| upstream_block(changed, &frees, block));
    (exps_match, block, block_else)
}

/// Revives the used underscore arguments of a function, then its binders.
pub(crate) fn apply_func(
    changed: &mut bool,
    (mut args_input, mut block, mut block_else): (Vec<Arg>, Block, Option<Block>),
) -> (Vec<Arg>, Block, Option<Block>) {
    let frees_input = args_input.as_slice().free_ids();
    let ids_bound = underscores(frees_input.clone());
    // Propose names for the underscore inputs, avoiding names in either block
    let frees_else = block_else
        .as_ref()
        .map(FreeIds::free_ids)
        .unwrap_or_default();
    let frees = frees_input.union(block.free_ids()).union(frees_else);
    let (frees, renamer) = candid_renamer(frees, &ids_bound);
    let mut ids_revive = IdSet::new();
    block = downstream_block(&renamer, changed, &mut ids_revive, block);
    block_else =
        block_else.map(|block| downstream_block(&renamer, changed, &mut ids_revive, block));
    // Uses in both blocks decide which inputs revive; then revive the binders
    let renamer = renamer.filter(|id, _| ids_revive.contains(id));
    args_input = renamer.rename_args(changed, args_input);
    block = upstream_block(changed, &frees, block);
    block_else = block_else.map(|block| upstream_block(changed, &frees, block));
    (args_input, block, block_else)
}
