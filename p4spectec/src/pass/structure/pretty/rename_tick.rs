//! Shorten tick suffixes on definition inputs and OL binding names
//!
//! `find_rename_ticks` picks the smallest unused tick count for each base name;
//! `upstream_block` carries enclosing names into nested bindings.
//! When `x` is already in use but `x'` is available:
//!
//! ```text
//! let x''' = source { return (x''', x) }
//!
//! becomes
//!
//! let x' = source { return (x', x) }
//! ```
//!
//! `apply_rel` and `apply_func` rename definition inputs consistently
//! in both the main body and the fallback, avoiding names already used there.

use std::rc::Rc;

use crate::lang::{
    common::ds::set::IdSet, data::notation::tree as notation, hints::input, traits::free::FreeIds,
};

use crate::lang::il::ast::{Arg, Exp};

use crate::pass::structure::{ol::ast::*, re::renamer::Renamer};

// == Candidate names

/// Picks the shortest tick spelling of `id` not in `frees`.
///
/// With `{x, x'', x'''}`, `x'''` becomes `x'`;
/// its own spelling reserves no slot.
fn find_rename_ticks(frees: &IdSet, id: &Id) -> Option<Id> {
    let mut id_rename = id.clone();
    id_rename
        .node
        .truncate(id.node.trim_end_matches('\'').len());
    while id_rename.node != id.node && frees.contains(&id_rename) {
        id_rename.node.push('\'');
    }
    (id.node != id_rename.node).then_some(id_rename)
}

/// Builds the renaming for the ticked names among `ids`, reserving each choice.
///
/// `(x'', x''')` can become `(x, x')`.
fn binding_renamer(frees: impl FnOnce() -> IdSet, ids: &IdSet) -> Renamer {
    // Nothing to do without ticked names
    let mut renamer = Renamer::empty();
    if !ids.iter().any(|id| id.node.ends_with('\'')) {
        return renamer;
    }
    // Reserve each choice so the next binding cannot take it
    let mut frees = frees();
    for id in ids.iter().filter(|id| id.node.ends_with('\'')) {
        if let Some(id_rename) = find_rename_ticks(&frees, id) {
            frees.take(id);
            frees.insert(id_rename.clone());
            renamer.add(id.clone(), id_rename);
        }
    }
    renamer
}

// == Upstream bindings

/// Rewrites an instruction; `frees` are enclosing names new names must avoid.
///
/// Under `if x { ... }`, a binding `x'` can stay `x'` but cannot become `x`.
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
    let frees = exp.free_ids().union(frees.clone());
    let block = upstream_block(changed, &frees, block);
    let instr = IfInstr { exp, iter_exps, block };
    InstrKind::If(instr)
}

// - Hold instruction

fn upstream_hold_instr(changed: &mut bool, frees: &IdSet, instr_ol: HoldInstr) -> InstrKind {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr_ol;
    let frees = not_exp.free_ids().union(frees.clone());
    let block_hold = upstream_block(changed, &frees, block_hold);
    let block_not_hold = upstream_block(changed, &frees, block_not_hold);
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    InstrKind::Hold(instr)
}

// - Case instruction

fn upstream_case_instr(changed: &mut bool, frees: &IdSet, instr_ol: CaseInstr) -> InstrKind {
    let CaseInstr { exp, cases, total } = instr_ol;
    let frees = exp.free_ids().union(frees.clone());
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            let frees = guard.free_ids().union(frees.clone());
            let block = upstream_block(changed, &frees, block);
            Case { guard, block }
        })
        .collect();
    let instr = CaseInstr { exp, cases, total };
    InstrKind::Case(instr)
}

// - Group instruction

fn upstream_group_instr(changed: &mut bool, frees: &IdSet, instr_ol: GroupInstr) -> InstrKind {
    let GroupInstr { id, rel_signature, exps, block } = instr_ol;
    let frees = exps.as_slice().free_ids().union(frees.clone());
    let block = upstream_block(changed, &frees, block);
    let instr = GroupInstr { id, rel_signature, exps, block };
    InstrKind::Group(instr)
}

// - Let instruction

/// Shortens ticks on the let's binders, then rewrites its body.
fn upstream_let_instr(changed: &mut bool, frees_upstream: &IdSet, instr_ol: LetInstr) -> InstrKind {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
    // Avoid the pattern, the source, the body, and enclosing names
    let frees_l = exp_l.free_ids();
    let frees_r = exp_r.free_ids();
    let renamer = binding_renamer(
        || {
            frees_l
                .clone()
                .union(frees_r.clone())
                .union(block.free_ids())
                .union(frees_upstream.clone())
        },
        &frees_l,
    );
    let exp_l = renamer.rename_exp(changed, exp_l);
    let iter_instrs = renamer.rename_iterinstrs_bind(changed, iter_instrs);
    // The body sees the renamed binders
    let frees = exp_l
        .free_ids()
        .union(frees_r)
        .union(frees_upstream.clone());
    let block = renamer.rename_block(changed, block);
    let block = upstream_block(changed, &frees, block);
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    InstrKind::Let(instr)
}

// - Rule instruction

/// Shortens ticks on the rule call's outputs, then rewrites its body.
fn upstream_rule_instr(
    changed: &mut bool,
    frees_upstream: &IdSet,
    instr_ol: RuleInstr,
) -> InstrKind {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr_ol;
    let exps = not_exp.args().to_vec();
    // Elaboration validates hints; OL rewrites preserve notation arity
    let (exps_input, exps_output) =
        input::split(&input_hint, exps).expect("validated relation hints and argument counts");
    // Only the outputs are binders here
    let frees_output = exps_output.as_slice().free_ids();
    let frees_input = exps_input.as_slice().free_ids();
    let renamer = binding_renamer(
        || {
            frees_input
                .clone()
                .union(frees_output.clone())
                .union(block.free_ids())
                .union(frees_upstream.clone())
        },
        &frees_output,
    );
    let exps_output = renamer.rename_exps(changed, exps_output);
    let iter_instrs = renamer.rename_iterinstrs_bind(changed, iter_instrs);
    // The body sees inputs, renamed outputs, and enclosing names
    let frees = frees_input
        .union(exps_output.as_slice().free_ids())
        .union(frees_upstream.clone());
    // Rebuild the notation with the renamed outputs
    // Renaming preserves the argument counts returned by input::split
    let exps = input::combine(&input_hint, exps_input, exps_output)
        .expect("validated relation hints and argument counts");
    let not_exp = notation::Mixfix::new(Rc::clone(not_exp.mixop()), exps)
        .expect("validated arguments preserve the mixfix arity");
    let block = renamer.rename_block(changed, block);
    let block = upstream_block(changed, &frees, block);
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    InstrKind::Rule(instr)
}

// == Definition inputs

/// Shortens ticks on relation inputs consistently across both blocks.
fn upstream_exps(
    changed: &mut bool,
    (mut exps_match, mut block, mut block_else): (Vec<Exp>, Block, Option<Block>),
) -> (Vec<Exp>, Block, Option<Block>) {
    // Nothing to do without ticked inputs
    // Names in use across the inputs and both blocks
    let ids = exps_match.as_slice().free_ids();
    if !ids.iter().any(|id| id.node.ends_with('\'')) {
        return (exps_match, block, block_else);
    }
    let frees_else = block_else
        .as_ref()
        .map(FreeIds::free_ids)
        .unwrap_or_default();
    let mut frees = ids.clone().union(block.free_ids()).union(frees_else);
    for id in ids.iter().filter(|id| id.node.ends_with('\'')) {
        if let Some(id_rename) = find_rename_ticks(&frees, id) {
            frees.take(id);
            frees.insert(id_rename.clone());
            // Rename consistently in the inputs and both blocks
            let renamer = Renamer::singleton(id.clone(), id_rename);
            exps_match = renamer.rename_exps(changed, exps_match);
            block = renamer.rename_block(changed, block);
            block_else = block_else.map(|block| renamer.rename_block(changed, block));
        }
    }
    (exps_match, block, block_else)
}

/// Shortens ticks on function arguments consistently across both blocks.
fn upstream_args(
    changed: &mut bool,
    (mut args_input, mut block, mut block_else): (Vec<Arg>, Block, Option<Block>),
) -> (Vec<Arg>, Block, Option<Block>) {
    // Nothing to do without ticked inputs
    // Names in use across the inputs and both blocks
    let ids = args_input.as_slice().free_ids();
    if !ids.iter().any(|id| id.node.ends_with('\'')) {
        return (args_input, block, block_else);
    }
    let frees_else = block_else
        .as_ref()
        .map(FreeIds::free_ids)
        .unwrap_or_default();
    let mut frees = ids.clone().union(block.free_ids()).union(frees_else);
    for id in ids.iter().filter(|id| id.node.ends_with('\'')) {
        if let Some(id_rename) = find_rename_ticks(&frees, id) {
            frees.take(id);
            frees.insert(id_rename.clone());
            // Rename consistently in the inputs and both blocks
            let renamer = Renamer::singleton(id.clone(), id_rename);
            args_input = renamer.rename_args(changed, args_input);
            block = renamer.rename_block(changed, block);
            block_else = block_else.map(|block| renamer.rename_block(changed, block));
        }
    }
    (args_input, block, block_else)
}

// == Entry points

/// Shortens ticks in a relation's inputs, then in its blocks.
pub(crate) fn apply_rel(
    changed: &mut bool,
    body: (Vec<Exp>, Block, Option<Block>),
) -> (Vec<Exp>, Block, Option<Block>) {
    let (exps_match, mut block, mut block_else) = upstream_exps(changed, body);
    let frees = exps_match.as_slice().free_ids();
    block = upstream_block(changed, &frees, block);
    block_else = block_else.map(|block| upstream_block(changed, &frees, block));
    (exps_match, block, block_else)
}

/// Shortens ticks in a function's arguments, then in its blocks.
pub(crate) fn apply_func(
    changed: &mut bool,
    body: (Vec<Arg>, Block, Option<Block>),
) -> (Vec<Arg>, Block, Option<Block>) {
    let (args_input, mut block, mut block_else) = upstream_args(changed, body);
    let frees = args_input.as_slice().free_ids();
    block = upstream_block(changed, &frees, block);
    block_else = block_else.map(|block| upstream_block(changed, &frees, block));
    (args_input, block, block_else)
}
