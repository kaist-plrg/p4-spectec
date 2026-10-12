//! Recognize conditional bindings and hinted destructuring in prose instructions
//!
//! A let over case notation with a matching `prose_fields` hint becomes Destruct
//! Binding analysis has already renamed repeated binders and extracted checks
//! `if v matches C; let C(x) = v` becomes an explicit conditional binding
//! Hinted and iterated lets remain intact for their own rendering

use crate::lang::{
    il::ast::{ListPattern, OptPattern, Pattern},
    pl::ast::{self as pl, ExpKind},
    traits::eq::SyntaxEq,
};

// == Conditional bindings

/// Matches a guard to the binding shape produced by partial binding.
fn matches_binding(pattern: &pl::Pattern, exp: &pl::Exp) -> bool {
    if exp.hints.node != Default::default() {
        return false;
    }
    match (pattern, &exp.node.node) {
        (Pattern::Case(mixop), ExpKind::Case(not_exp)) => not_exp.mixop().syntax_eq(mixop),
        (Pattern::List(ListPattern::Nil), ExpKind::List(exps)) => exps.is_empty(),
        (Pattern::List(ListPattern::Fixed(len)), ExpKind::List(exps)) => exps.len() == *len,
        (Pattern::List(ListPattern::Cons), ExpKind::Cons(..)) => true,
        (Pattern::Opt(OptPattern::None), ExpKind::Opt(None)) => true,
        (Pattern::Opt(OptPattern::Some), ExpKind::Opt(Some(_))) => true,
        _ => false,
    }
}

/// Compares only plain variable references, retaining their type distinction.
fn same_variable(exp_l: &pl::Exp, exp_r: &pl::Exp) -> bool {
    matches!((&exp_l.node.node, &exp_r.node.node), (ExpKind::Id(id_l), ExpKind::Id(id_r)) if id_l.node == id_r.node)
        && exp_l.node.note.syntax_eq(&exp_r.node.note)
        && exp_l.hints.node == Default::default()
        && exp_r.hints.node == Default::default()
}

/// Finds an unhinted, noniterated leading let eligible for a conditional binding.
fn leading_let<Tier>(block: &[pl::Instr<Tier>]) -> Option<&pl::LetInstr> {
    let instr = block.first()?;
    if instr.hints.node != Default::default() {
        return None;
    }
    let pl::InstrKind::Let(instr_let) = &instr.node.node else { return None };
    if !instr_let.iter_instrs.is_empty() || instr_let.exp_r.hints.node != Default::default() {
        return None;
    }
    Some(instr_let)
}

/// Checks that the subtype test accounts for the whole binding operation.
fn sub_binding(typ: &pl::Typ, exp_scrut: &pl::Exp, instr_let: &pl::LetInstr) -> bool {
    // Preserve narrowing casts, without inventing a cast for same-type bindings
    let same_source = match &instr_let.exp_r.node.node {
        ExpKind::DownCast(typ_cast, exp) => {
            typ.syntax_eq(typ_cast)
                && same_variable(exp, exp_scrut)
                && !exp_scrut.node.note.syntax_eq(&typ.node)
        }
        _ => same_variable(&instr_let.exp_r, exp_scrut),
    };
    // Keep the binding's type and presentation while retaining the original test
    same_source
        && instr_let.exp_l.node.note.syntax_eq(&typ.node)
        && instr_let.exp_r.node.note.syntax_eq(&typ.node)
        && instr_let.exp_l.hints.node == Default::default()
}

/// Checks that the match proves the binding shape on the same typed variable.
fn match_binding(pattern: &pl::Pattern, exp_scrut: &pl::Exp, instr_let: &pl::LetInstr) -> bool {
    same_variable(&instr_let.exp_r, exp_scrut)
        && instr_let
            .exp_l
            .node
            .note
            .syntax_eq(&instr_let.exp_r.node.note)
        && matches_binding(pattern, &instr_let.exp_l)
}

/// Removes the leading let after its binding has been validated.
fn take_binding<Tier>(block: &mut pl::Block<Tier>) -> pl::Exp {
    let pl::InstrKind::Let(instr_let) = block.remove(0).node.node else {
        unreachable!("validated leading let")
    };
    instr_let.exp_l
}

/// Folds a safe leading let into its subtype or pattern condition.
fn shorten_if_instr<Tier>(
    shorten_block: fn(pl::Block<Tier>) -> pl::Block<Tier>,
    mut instr_if: pl::IfInstr<Tier>,
) -> pl::InstrKind<Tier> {
    // Iterated conditions and hinted bindings keep their separate instructions
    let binding = instr_if.iter_exps.is_empty()
        && leading_let(&instr_if.block).is_some_and(|instr_let| match &instr_if.exp.node.node {
            ExpKind::Sub(exp, typ, _) => sub_binding(typ, exp, instr_let),
            ExpKind::Match(exp, pattern) => match_binding(pattern, exp, instr_let),
            _ => false,
        });
    if !binding {
        instr_if.block = shorten_block(instr_if.block);
        return pl::InstrKind::If(instr_if);
    }
    // Move the binding into syntax before shortening the remaining body
    let exp_l = take_binding(&mut instr_if.block);
    let block = shorten_block(instr_if.block);
    let dangle = instr_if.dangle;
    match instr_if.exp.node.node {
        ExpKind::Sub(exp_r, typ, subcheck) => pl::InstrKind::IfLetSub(pl::IfLetSubInstr {
            typ,
            subcheck,
            exp_l,
            exp_r: *exp_r,
            block,
            dangle,
        }),
        ExpKind::Match(exp_r, pattern) => pl::InstrKind::IfLetMatch(pl::IfLetMatchInstr {
            pattern,
            exp_l,
            exp_r: *exp_r,
            block,
            dangle,
        }),
        _ => unreachable!("validated conditional binding"),
    }
}

/// Folds each safe leading let into its case guard before shortening the body.
fn shorten_case_instr<Tier>(
    shorten_block: fn(pl::Block<Tier>) -> pl::Block<Tier>,
    mut instr_case: pl::CaseInstr<Tier>,
) -> pl::InstrKind<Tier> {
    for case in &mut instr_case.cases {
        // Keep guards and bindings separate unless the guard proves the binding
        let binding = leading_let(&case.block).is_some_and(|instr_let| match &case.guard {
            pl::Guard::Sub(typ, _) => sub_binding(typ, &instr_case.exp, instr_let),
            pl::Guard::Match(pattern) => match_binding(pattern, &instr_case.exp, instr_let),
            _ => false,
        });
        if binding {
            let exp_l = take_binding(&mut case.block);
            case.guard = match &case.guard {
                pl::Guard::Sub(typ, subcheck) => {
                    pl::Guard::LetSub(typ.clone(), subcheck.clone(), exp_l)
                }
                pl::Guard::Match(pattern) => pl::Guard::LetMatch(pattern.clone(), exp_l),
                _ => unreachable!("validated conditional binding"),
            };
        }
        // Shorten the continuation after preserving its binding in the guard
        case.block = shorten_block(std::mem::take(&mut case.block));
    }
    pl::InstrKind::Case(instr_case)
}

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

// - Dispatch instruction

/// Shortens a dispatch-tier instruction.
fn shorten_dispatch_instr(mut instr: pl::Instr<pl::DispatchInstr>) -> pl::Instr<pl::DispatchInstr> {
    shorten_destruct(&mut instr);
    instr.node.node = shorten_dispatch_instr_kind(instr.node.node);
    instr
}

/// Recurses into the blocks of a dispatch-tier instruction.
fn shorten_dispatch_instr_kind(
    instr_kind: pl::InstrKind<pl::DispatchInstr>,
) -> pl::InstrKind<pl::DispatchInstr> {
    match instr_kind {
        pl::InstrKind::If(instr_if) => shorten_if_instr(shorten_dispatch_block, instr_if),
        pl::InstrKind::IfLetSub(mut instr) => {
            instr.block = shorten_dispatch_block(instr.block);
            pl::InstrKind::IfLetSub(instr)
        }
        pl::InstrKind::IfLetMatch(mut instr) => {
            instr.block = shorten_dispatch_block(instr.block);
            pl::InstrKind::IfLetMatch(instr)
        }
        pl::InstrKind::Check(mut instr_check) => {
            instr_check.block = shorten_dispatch_block(instr_check.block);
            pl::InstrKind::Check(instr_check)
        }
        pl::InstrKind::Hold(mut instr_hold) => {
            instr_hold.hold_case = shorten_dispatch_hold_case(instr_hold.hold_case);
            pl::InstrKind::Hold(instr_hold)
        }
        pl::InstrKind::Case(instr_case) => shorten_case_instr(shorten_dispatch_block, instr_case),
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
fn shorten_dispatch_block(block: pl::DispatchBlock) -> pl::DispatchBlock {
    block.into_iter().map(shorten_dispatch_instr).collect()
}

// - Holding condition

/// Recurses into a hold's branches.
fn shorten_dispatch_hold_case(
    hold_case: pl::HoldCase<pl::DispatchInstr>,
) -> pl::HoldCase<pl::DispatchInstr> {
    match hold_case {
        pl::HoldCase::Both(block_hold, block_not_hold) => {
            let block_hold = shorten_dispatch_block(block_hold);
            let block_not_hold = shorten_dispatch_block(block_not_hold);
            pl::HoldCase::Both(block_hold, block_not_hold)
        }
        pl::HoldCase::Hold(block, dangle) => {
            let block = shorten_dispatch_block(block);
            pl::HoldCase::Hold(block, dangle)
        }
        pl::HoldCase::NotHold(block, dangle) => {
            let block = shorten_dispatch_block(block);
            pl::HoldCase::NotHold(block, dangle)
        }
    }
}

// - Tier instruction

/// Recurses into a rule group's body or a route's arms.
fn shorten_dispatch_tier(instr_dispatch: pl::DispatchInstr) -> pl::DispatchInstr {
    match instr_dispatch {
        pl::DispatchInstr::Group(mut instr_group) => {
            instr_group.block = shorten_group_block(instr_group.block);
            pl::DispatchInstr::Group(instr_group)
        }
        pl::DispatchInstr::Route(mut instr_route) => {
            let mut blocks = Vec::with_capacity(instr_route.blocks.len());
            for block in instr_route.blocks {
                blocks.push(shorten_dispatch_block(block));
            }
            instr_route.blocks = blocks;
            pl::DispatchInstr::Route(instr_route)
        }
    }
}

// - Group instruction

/// Shortens a group-tier instruction.
fn shorten_group_instr(mut instr: pl::Instr<pl::GroupInstr>) -> pl::Instr<pl::GroupInstr> {
    shorten_destruct(&mut instr);
    instr.node.node = shorten_group_instr_kind(instr.node.node);
    instr
}

/// Recurses into the blocks of a group-tier instruction.
fn shorten_group_instr_kind(
    instr_kind: pl::InstrKind<pl::GroupInstr>,
) -> pl::InstrKind<pl::GroupInstr> {
    match instr_kind {
        pl::InstrKind::If(instr_if) => shorten_if_instr(shorten_group_block, instr_if),
        pl::InstrKind::IfLetSub(mut instr) => {
            instr.block = shorten_group_block(instr.block);
            pl::InstrKind::IfLetSub(instr)
        }
        pl::InstrKind::IfLetMatch(mut instr) => {
            instr.block = shorten_group_block(instr.block);
            pl::InstrKind::IfLetMatch(instr)
        }
        pl::InstrKind::Check(mut instr_check) => {
            instr_check.block = shorten_group_block(instr_check.block);
            pl::InstrKind::Check(instr_check)
        }
        pl::InstrKind::Hold(mut instr_hold) => {
            instr_hold.hold_case = shorten_group_hold_case(instr_hold.hold_case);
            pl::InstrKind::Hold(instr_hold)
        }
        pl::InstrKind::Case(instr_case) => shorten_case_instr(shorten_group_block, instr_case),
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
fn shorten_group_block(block: pl::GroupBlock) -> pl::GroupBlock {
    block.into_iter().map(shorten_group_instr).collect()
}

// - Holding condition

/// Recurses into a hold's branches.
fn shorten_group_hold_case(
    hold_case: pl::HoldCase<pl::GroupInstr>,
) -> pl::HoldCase<pl::GroupInstr> {
    match hold_case {
        pl::HoldCase::Both(block_hold, block_not_hold) => {
            let block_hold = shorten_group_block(block_hold);
            let block_not_hold = shorten_group_block(block_not_hold);
            pl::HoldCase::Both(block_hold, block_not_hold)
        }
        pl::HoldCase::Hold(block, dangle) => {
            let block = shorten_group_block(block);
            pl::HoldCase::Hold(block, dangle)
        }
        pl::HoldCase::NotHold(block, dangle) => {
            let block = shorten_group_block(block);
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
                blocks.push(shorten_group_block(block));
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
            def_rel.block = shorten_dispatch_block(def_rel.block);
            def_rel.block_else_opt = def_rel.block_else_opt.map(shorten_dispatch_block);
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
                row.block = shorten_group_block(std::mem::take(&mut row.block));
            }
            pl::MetaFuncDef::Table(def_func)
        }
        pl::MetaFuncDef::Defined(mut def_func) => {
            def_func.block = shorten_group_block(def_func.block);
            def_func.block_else_opt = def_func.block_else_opt.map(shorten_group_block);
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
