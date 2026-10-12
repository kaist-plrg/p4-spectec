//! Recognize compact prose instructions after annotation
//!
//! Three shapes fold into one instruction each:
//! a case arm or condition that renames its scrutinee
//! (`CheckLetSub`, `CheckLetMatch`, and the `CheckLet*` guards),
//! a let over a case notation with a `prose_fields` hint (`Destruct`),
//! and a let followed by an `is Some` check that binds the content
//! (`OptionGet`).
//!
//! For example, `let t = e; if t is Some { let ?(x) = t; B }` folds to the
//! single `OptionGet` step `let x = !e { B }`.

use std::collections::VecDeque;

use crate::lang::il::ast::OptPattern;

use crate::lang::pl::ast as pl;

// == Expression aliases

/// Whether two expressions are the same variable, through iterations.
fn is_eq_exp_var(exp_a: &pl::Exp, exp_b: &pl::Exp) -> bool {
    match (&exp_a.node.node, &exp_b.node.node) {
        (pl::ExpKind::Id(id_a), pl::ExpKind::Id(id_b)) => id_a.node == id_b.node,
        (pl::ExpKind::Iter(exp_a, _), pl::ExpKind::Iter(exp_b, _)) => is_eq_exp_var(exp_a, exp_b),
        _ => false,
    }
}

/// Whether `exp_r` is the scrutinee itself or a downcast of it.
fn is_scrutinee_alias(exp_scrut: &pl::Exp, exp_r: &pl::Exp) -> bool {
    match &exp_r.node.node {
        pl::ExpKind::DownCast(_, exp_inner) => is_eq_exp_var(exp_scrut, exp_inner),
        _ => is_eq_exp_var(exp_scrut, exp_r),
    }
}

/// Removes a leading `let x = scrutinee` from `block` and returns `x`.
fn take_leading_rename<Tier>(exp_scrut: &pl::Exp, block: &mut pl::Block<Tier>) -> Option<pl::Exp> {
    let is_leading_rename = matches!(
        block.first(),
        Some(instr) if matches!(
            &instr.node.node,
            pl::InstrKind::Let(pl::LetInstr { exp_r, iter_instrs, .. })
                if iter_instrs.is_empty() && is_scrutinee_alias(exp_scrut, exp_r)
        )
    );
    // Only a first instruction that renames the scrutinee qualifies
    if !is_leading_rename {
        return None;
    }
    let instr = block.remove(0);
    let pl::InstrKind::Let(pl::LetInstr { exp_l, .. }) = instr.node.node else {
        return None;
    };
    Some(exp_l)
}

// == Case guards

/// Folds a renaming let at the head of a subtype or match arm into its guard.
fn shorten_case_guards<Tier>(instr: &mut pl::Instr<Tier>) {
    let pl::InstrKind::Case(pl::CaseInstr { exp, cases, .. }) = &mut instr.node.node else {
        return;
    };
    for case in cases {
        let guard = match &case.guard {
            // Subtype guard: `let x = scrut, scrut has type t`
            pl::Guard::Sub(typ, subcheck) => {
                take_leading_rename(exp, &mut case.block).map(|exp_target| {
                    pl::Guard::CheckLetSub(typ.clone(), subcheck.clone(), exp_target)
                })
            }
            // Match guard: `let x = scrut, scrut matches p`
            pl::Guard::Match(pattern) => take_leading_rename(exp, &mut case.block)
                .map(|exp_target| pl::Guard::CheckLetMatch(pattern.clone(), exp_target)),
            _ => None,
        };
        if let Some(guard) = guard {
            case.guard = guard;
        }
    }
}

// == Checked bindings

/// Folds `if scrut has type t { let x = scrut; .. }`, its match twin,
/// or a single-arm case with such a guard, into a checked let.
fn shorten_check_let<Tier>(instr: &mut pl::Instr<Tier>) {
    /// The check a checked let performs.
    enum Check {
        Sub(pl::Typ, Box<pl::Subcheck>, pl::Exp),
        Match(pl::Pattern, pl::Exp),
    }

    let check = match &instr.node.node {
        // An un-iterated condition that is a subtype or match test
        pl::InstrKind::If(pl::IfInstr { exp, iter_exps, .. }) if iter_exps.is_empty() => {
            match &exp.node.node {
                pl::ExpKind::Sub(exp_scrut, typ, subcheck) => {
                    Some(Check::Sub(typ.clone(), subcheck.clone(), exp_scrut.as_ref().clone()))
                }
                pl::ExpKind::Match(exp_scrut, pattern) => {
                    Some(Check::Match(pattern.clone(), exp_scrut.as_ref().clone()))
                }
                _ => None,
            }
        }
        // A single-arm case with a subtype or match guard
        pl::InstrKind::Case(pl::CaseInstr { exp, cases, .. }) if cases.len() == 1 => {
            let case = &cases[0];
            match &case.guard {
                pl::Guard::Sub(typ, subcheck) => {
                    Some(Check::Sub(typ.clone(), subcheck.clone(), exp.clone()))
                }
                pl::Guard::Match(pattern) => Some(Check::Match(pattern.clone(), exp.clone())),
                _ => None,
            }
        }
        _ => None,
    };
    let Some(check) = check else { return };

    let block = match &mut instr.node.node {
        pl::InstrKind::If(pl::IfInstr { block, .. }) => block,
        pl::InstrKind::Case(pl::CaseInstr { cases, .. }) => &mut cases[0].block,
        _ => unreachable!(),
    };
    let exp_scrut = match &check {
        Check::Sub(_, _, exp_scrut) | Check::Match(_, exp_scrut) => exp_scrut,
    };
    // The block must start by renaming the scrutinee
    let Some(exp_l) = take_leading_rename(exp_scrut, block) else { return };
    let block = std::mem::take(block);
    instr.node.node = match check {
        Check::Sub(typ, subcheck, exp_r) => {
            pl::InstrKind::CheckLetSub(pl::CheckLetSubInstr { typ, subcheck, exp_l, exp_r, block })
        }
        Check::Match(pattern, exp_r) => {
            pl::InstrKind::CheckLetMatch(pl::CheckLetMatchInstr { pattern, exp_l, exp_r, block })
        }
    };
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

/// Applies the single-instruction shorthands in order.
fn shorten_instr_shorthands<Tier>(mut instr: pl::Instr<Tier>) -> pl::Instr<Tier> {
    shorten_case_guards(&mut instr);
    shorten_check_let(&mut instr);
    shorten_destruct(&mut instr);
    instr
}

// == Option extraction

/// Folds `let t = e` followed by `if t is Some { let ?(x) = t; .. }`
/// into `let x = !e` with the block.
fn shorten_option_get<Tier>(
    instrs_pending: &mut VecDeque<pl::Instr<Tier>>,
) -> Option<pl::Instr<Tier>> {
    // Needs the let and the following if
    if instrs_pending.len() < 2 {
        return None;
    }
    // An un-iterated let followed by an un-iterated `is Some` test
    let is_option_get =
        match (&instrs_pending.front()?.node.node, &instrs_pending.get(1)?.node.node) {
            (
                pl::InstrKind::Let(pl::LetInstr {
                    exp_l: exp_tmp,
                    exp_r: exp_value,
                    iter_instrs: iter_let,
                }),
                pl::InstrKind::If(pl::IfInstr {
                    exp: exp_cond,
                    iter_exps: iter_if,
                    block: block_then,
                    ..
                }),
            ) if iter_let.is_empty() && iter_if.is_empty() => {
                let pl::ExpKind::Match(exp_scrut, pl::Pattern::Opt(OptPattern::Some)) =
                    &exp_cond.node.node
                else {
                    return None;
                };
                let instr_then = block_then.first()?;
                let pl::InstrKind::Let(pl::LetInstr { exp_l, exp_r, iter_instrs }) =
                    &instr_then.node.node
                else {
                    return None;
                };
                let pl::ExpKind::Opt(Some(exp_target)) = &exp_l.node.node else {
                    return None;
                };
                // The temporary must be what is tested and unwrapped
                if !iter_instrs.is_empty()
                    || !is_eq_exp_var(exp_tmp, exp_scrut)
                    || !is_eq_exp_var(exp_tmp, exp_r)
                {
                    return None;
                }
                let _ = (exp_target, exp_value);
                true
            }
            _ => false,
        };
    if !is_option_get {
        return None;
    }
    // Consume both instructions and rebuild them as one
    let instr_source = instrs_pending.pop_front()?;
    let instr_if = instrs_pending.pop_front()?;
    let pl::InstrKind::Let(pl::LetInstr { exp_r: exp_value, .. }) = instr_source.node.node else {
        unreachable!();
    };
    let pl::InstrKind::If(pl::IfInstr { mut block, .. }) = instr_if.node.node else {
        unreachable!();
    };
    let instr_target = block.remove(0);
    let pl::InstrKind::Let(pl::LetInstr { exp_l, .. }) = instr_target.node.node else {
        unreachable!();
    };
    let pl::ExpKind::Opt(Some(exp_target)) = exp_l.node.node else {
        unreachable!();
    };
    Some(crate::annotated_note_phrase! {
        node: pl::InstrKind::OptionGet(pl::OptionGetInstr {
            exp_l: *exp_target,
            exp_r: exp_value,
            block,
        }),
        note: instr_source.node.note,
        span: instr_source.node.span,
        hints: instr_source.hints,
    })
}

/// Folds pairs across a block, then each instruction on its own.
fn shorten_block_shorthands<Tier>(block: pl::Block<Tier>) -> pl::Block<Tier> {
    let mut instrs_pending = VecDeque::from(block);
    let mut block_output = Vec::new();
    while !instrs_pending.is_empty() {
        // A pair fold consumes two instructions
        if let Some(instr) = shorten_option_get(&mut instrs_pending) {
            block_output.push(instr);
        } else {
            block_output.push(
                instrs_pending
                    .pop_front()
                    .expect("pending instruction was checked as non-empty"),
            );
        }
    }
    block_output
        .into_iter()
        .map(shorten_instr_shorthands)
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
            instr_if.block = shorten_dispatch_block(instr_if.block);
            pl::InstrKind::If(instr_if)
        }
        pl::InstrKind::Hold(mut instr_hold) => {
            instr_hold.hold_case = shorten_dispatch_hold_case(instr_hold.hold_case);
            pl::InstrKind::Hold(instr_hold)
        }
        pl::InstrKind::Case(mut instr_case) => {
            for case in &mut instr_case.cases {
                case.block = shorten_dispatch_block(std::mem::take(&mut case.block));
            }
            pl::InstrKind::Case(instr_case)
        }
        pl::InstrKind::CheckLetSub(mut instr_check) => {
            instr_check.block = shorten_dispatch_block(instr_check.block);
            pl::InstrKind::CheckLetSub(instr_check)
        }
        pl::InstrKind::CheckLetMatch(mut instr_check) => {
            instr_check.block = shorten_dispatch_block(instr_check.block);
            pl::InstrKind::CheckLetMatch(instr_check)
        }
        pl::InstrKind::OptionGet(mut instr_get) => {
            instr_get.block = shorten_dispatch_block(instr_get.block);
            pl::InstrKind::OptionGet(instr_get)
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

/// Shortens a dispatch block, pairs first.
fn shorten_dispatch_block(block: pl::DispatchBlock) -> pl::DispatchBlock {
    shorten_block_shorthands(block)
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
    instr.node.node = shorten_group_instr_kind(instr.node.node);
    instr
}

/// Recurses into the blocks of a group-tier instruction.
fn shorten_group_instr_kind(
    instr_kind: pl::InstrKind<pl::GroupInstr>,
) -> pl::InstrKind<pl::GroupInstr> {
    match instr_kind {
        pl::InstrKind::If(mut instr_if) => {
            instr_if.block = shorten_group_block(instr_if.block);
            pl::InstrKind::If(instr_if)
        }
        pl::InstrKind::Hold(mut instr_hold) => {
            instr_hold.hold_case = shorten_group_hold_case(instr_hold.hold_case);
            pl::InstrKind::Hold(instr_hold)
        }
        pl::InstrKind::Case(mut instr_case) => {
            for case in &mut instr_case.cases {
                case.block = shorten_group_block(std::mem::take(&mut case.block));
            }
            pl::InstrKind::Case(instr_case)
        }
        pl::InstrKind::CheckLetSub(mut instr_check) => {
            instr_check.block = shorten_group_block(instr_check.block);
            pl::InstrKind::CheckLetSub(instr_check)
        }
        pl::InstrKind::CheckLetMatch(mut instr_check) => {
            instr_check.block = shorten_group_block(instr_check.block);
            pl::InstrKind::CheckLetMatch(instr_check)
        }
        pl::InstrKind::OptionGet(mut instr_get) => {
            instr_get.block = shorten_group_block(instr_get.block);
            pl::InstrKind::OptionGet(instr_get)
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

/// Shortens a group block, pairs first.
fn shorten_group_block(block: pl::GroupBlock) -> pl::GroupBlock {
    shorten_block_shorthands(block)
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
