//! Recognize hinted destructuring in prose instructions
//!
//! A let over case notation with a matching `prose_fields` hint becomes Destruct.
//! Leading bindings in conditional bodies remain explicit for rendering.

use crate::lang::pl::ast as pl;

// == Destructuring

/// Whether a bound expression is shown; `_`-prefixed names are hidden.
fn visible(exp: &pl::Exp) -> bool {
    match &exp.node.node {
        pl::ExpKind::Id(id) => !id.node.starts_with('_'),
        pl::ExpKind::Iter(exp_inner, _) => visible(exp_inner),
        _ => true,
    }
}

/// Folds `let C(x, y) = e` with a `prose_fields` hint into a destructuring.
fn shorten_destruct<Tier>(instr: &mut pl::Instr<Tier>) {
    let Some(field_names) = instr
        .hints
        .node
        .prose_fields
        .as_ref()
        .map(|hint| hint.node.as_slice())
    else {
        return;
    };
    let pl::InstrKind::Let(pl::LetInstr { exp_l, exp_r, iter_instrs }) = &instr.node.node else {
        return;
    };
    if !iter_instrs.is_empty() {
        return;
    }
    let pl::ExpKind::Case(not_exp) = &exp_l.node.node else {
        return;
    };
    let exps = not_exp.args();
    // Field names must match the arity and something must be visible
    if exps.len() != field_names.len() || exps.iter().all(|exp| !visible(exp)) {
        return;
    }
    let bindings = exps
        .iter()
        .zip(field_names)
        .map(|(exp, name)| (visible(exp).then(|| name.node.clone()), exp.clone()))
        .collect();
    instr.node.node = pl::InstrKind::Destruct(pl::DestructInstr { bindings, exp: exp_r.clone() });
}

/// Shortens bindings while preserving a conditional body's leading let.
fn shorten_block_shorthands<Tier>(
    block: pl::Block<Tier>,
    preserve_leading_let: bool,
) -> pl::Block<Tier> {
    block
        .into_iter()
        .enumerate()
        .map(|(idx, mut instr)| {
            // The renderer needs the original conditional binding and its hints
            if idx != 0 || !preserve_leading_let {
                shorten_destruct(&mut instr);
            }
            instr
        })
        .collect()
}

// - Dispatch instruction

/// Shortens a dispatch-tier instruction.
fn shorten_dispatch_instr(mut instr: pl::Instr<pl::DispatchInstr>) -> pl::Instr<pl::DispatchInstr> {
    instr.node.node = shorten_dispatch_instr_kind(instr.node.node);
    instr
}

/// Recurses into the blocks of a dispatch-tier instruction.
fn shorten_dispatch_instr_kind(
    instr_kind: pl::InstrKind<pl::DispatchInstr>,
) -> pl::InstrKind<pl::DispatchInstr> {
    match instr_kind {
        pl::InstrKind::If(mut instr_if) => {
            instr_if.block = shorten_dispatch_block(instr_if.block, true);
            pl::InstrKind::If(instr_if)
        }
        pl::InstrKind::Hold(mut instr_hold) => {
            instr_hold.hold_case = shorten_dispatch_hold_case(instr_hold.hold_case);
            pl::InstrKind::Hold(instr_hold)
        }
        pl::InstrKind::Case(mut instr_case) => {
            for case in &mut instr_case.cases {
                case.block = shorten_dispatch_block(std::mem::take(&mut case.block), true);
            }
            pl::InstrKind::Case(instr_case)
        }
        pl::InstrKind::Tier(instr_tier) => {
            let tier = shorten_dispatch_tier(instr_tier.tier);
            pl::InstrKind::Tier(pl::TierInstr { tier })
        }
        // Leaves have no blocks
        kind @ (pl::InstrKind::Let(_) | pl::InstrKind::Debug(_) | pl::InstrKind::Destruct(_)) => {
            kind
        }
    }
}

/// Shortens a dispatch block and its nested bodies.
fn shorten_dispatch_block(
    block: pl::DispatchBlock,
    preserve_leading_let: bool,
) -> pl::DispatchBlock {
    shorten_block_shorthands(block, preserve_leading_let)
        .into_iter()
        .map(shorten_dispatch_instr)
        .collect()
}

// - Holding condition

/// Recurses into a hold's branches.
fn shorten_dispatch_hold_case(
    hold_case: pl::HoldCase<pl::DispatchInstr>,
) -> pl::HoldCase<pl::DispatchInstr> {
    match hold_case {
        pl::HoldCase::Both(block_hold, block_not_hold) => {
            let block_hold = shorten_dispatch_block(block_hold, false);
            let block_not_hold = shorten_dispatch_block(block_not_hold, false);
            pl::HoldCase::Both(block_hold, block_not_hold)
        }
        pl::HoldCase::Hold(block, dangle) => {
            let block = shorten_dispatch_block(block, false);
            pl::HoldCase::Hold(block, dangle)
        }
        pl::HoldCase::NotHold(block, dangle) => {
            let block = shorten_dispatch_block(block, false);
            pl::HoldCase::NotHold(block, dangle)
        }
    }
}

// - Tier instruction

/// Recurses into a rule group's body or a route's arms.
fn shorten_dispatch_tier(instr_dispatch: pl::DispatchInstr) -> pl::DispatchInstr {
    match instr_dispatch {
        pl::DispatchInstr::Group(mut instr_group) => {
            instr_group.block = shorten_group_block(instr_group.block, false);
            pl::DispatchInstr::Group(instr_group)
        }
        pl::DispatchInstr::Route(mut instr_route) => {
            let mut blocks = Vec::with_capacity(instr_route.blocks.len());
            for block in instr_route.blocks {
                blocks.push(shorten_dispatch_block(block, false));
            }
            instr_route.blocks = blocks;
            pl::DispatchInstr::Route(instr_route)
        }
    }
}

// - Group instruction

/// Shortens a group-tier instruction.
fn shorten_group_instr(mut instr: pl::Instr<pl::GroupInstr>) -> pl::Instr<pl::GroupInstr> {
    instr.node.node = shorten_group_instr_kind(instr.node.node);
    instr
}

/// Recurses into the blocks of a group-tier instruction.
fn shorten_group_instr_kind(
    instr_kind: pl::InstrKind<pl::GroupInstr>,
) -> pl::InstrKind<pl::GroupInstr> {
    match instr_kind {
        pl::InstrKind::If(mut instr_if) => {
            instr_if.block = shorten_group_block(instr_if.block, true);
            pl::InstrKind::If(instr_if)
        }
        pl::InstrKind::Hold(mut instr_hold) => {
            instr_hold.hold_case = shorten_group_hold_case(instr_hold.hold_case);
            pl::InstrKind::Hold(instr_hold)
        }
        pl::InstrKind::Case(mut instr_case) => {
            for case in &mut instr_case.cases {
                case.block = shorten_group_block(std::mem::take(&mut case.block), true);
            }
            pl::InstrKind::Case(instr_case)
        }
        pl::InstrKind::Tier(instr_tier) => {
            let tier = shorten_group_tier(instr_tier.tier);
            pl::InstrKind::Tier(pl::TierInstr { tier })
        }
        // Leaves have no blocks
        kind @ (pl::InstrKind::Let(_) | pl::InstrKind::Debug(_) | pl::InstrKind::Destruct(_)) => {
            kind
        }
    }
}

/// Shortens a group block and its nested bodies.
fn shorten_group_block(block: pl::GroupBlock, preserve_leading_let: bool) -> pl::GroupBlock {
    shorten_block_shorthands(block, preserve_leading_let)
        .into_iter()
        .map(shorten_group_instr)
        .collect()
}

// - Holding condition

/// Recurses into a hold's branches.
fn shorten_group_hold_case(
    hold_case: pl::HoldCase<pl::GroupInstr>,
) -> pl::HoldCase<pl::GroupInstr> {
    match hold_case {
        pl::HoldCase::Both(block_hold, block_not_hold) => {
            let block_hold = shorten_group_block(block_hold, false);
            let block_not_hold = shorten_group_block(block_not_hold, false);
            pl::HoldCase::Both(block_hold, block_not_hold)
        }
        pl::HoldCase::Hold(block, dangle) => {
            let block = shorten_group_block(block, false);
            pl::HoldCase::Hold(block, dangle)
        }
        pl::HoldCase::NotHold(block, dangle) => {
            let block = shorten_group_block(block, false);
            pl::HoldCase::NotHold(block, dangle)
        }
    }
}

// - Tier instruction

/// Recurses into a backtrack's arms; other group instructions have no blocks.
fn shorten_group_tier(instr_group: pl::GroupInstr) -> pl::GroupInstr {
    match instr_group {
        pl::GroupInstr::Backtrack(mut instr_backtrack) => {
            let mut blocks = Vec::with_capacity(instr_backtrack.blocks.len());
            for block in instr_backtrack.blocks {
                blocks.push(shorten_group_block(block, false));
            }
            instr_backtrack.blocks = blocks;
            pl::GroupInstr::Backtrack(instr_backtrack)
        }
        instr_group @ (pl::GroupInstr::Result(_)
        | pl::GroupInstr::Return(_)
        | pl::GroupInstr::Rule(_)) => instr_group,
    }
}

// == Relation definitions

/// Shortens a defined relation's blocks.
fn shorten_rel_def(def_rel: pl::RelDef) -> pl::RelDef {
    match def_rel {
        pl::RelDef::Defined(mut def_rel) => {
            def_rel.block = shorten_dispatch_block(def_rel.block, false);
            def_rel.block_else_opt = def_rel
                .block_else_opt
                .map(|block| shorten_dispatch_block(block, false));
            pl::RelDef::Defined(def_rel)
        }
        pl::RelDef::Extern(def_rel) => pl::RelDef::Extern(def_rel),
    }
}

// == Meta-function definitions

/// Shortens table rows and defined function blocks.
fn shorten_func_def(def_func: pl::MetaFuncDef) -> pl::MetaFuncDef {
    match def_func {
        pl::MetaFuncDef::Table(mut def_func) => {
            for row in &mut def_func.rows {
                row.block = shorten_group_block(std::mem::take(&mut row.block), false);
            }
            pl::MetaFuncDef::Table(def_func)
        }
        pl::MetaFuncDef::Defined(mut def_func) => {
            def_func.block = shorten_group_block(def_func.block, false);
            def_func.block_else_opt = def_func
                .block_else_opt
                .map(|block| shorten_group_block(block, false));
            pl::MetaFuncDef::Defined(def_func)
        }
        def_func @ (pl::MetaFuncDef::Extern(_) | pl::MetaFuncDef::Builtin(_)) => def_func,
    }
}

// == Definitions

/// Shortens one definition.
fn shorten_def(mut def: pl::Def) -> pl::Def {
    def.node.node = shorten_def_kind(def.node.node);
    def
}

/// Shortens relations and functions; types and variables have no blocks.
fn shorten_def_kind(def_kind: pl::DefKind) -> pl::DefKind {
    match def_kind {
        pl::DefKind::Rel(def_rel) => {
            let def_rel = shorten_rel_def(def_rel);
            pl::DefKind::Rel(def_rel)
        }
        pl::DefKind::MetaFunc(def_func) => {
            let def_func = shorten_func_def(def_func);
            pl::DefKind::MetaFunc(def_func)
        }
        kind @ (pl::DefKind::Typ(_) | pl::DefKind::Var(_)) => kind,
    }
}

// == Entry point

/// Applies the shorthands to every definition.
pub(super) fn shorten_spec(spec: pl::Spec) -> pl::Spec {
    spec.into_iter().map(shorten_def).collect()
}
