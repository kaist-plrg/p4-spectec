//! Substitute variable aliases while avoiding capture by nested bindings
//!
//! `remove_let_instr` substitutes the alias throughout its OL body, then
//! removes the Let wrapper:
//!
//! ```text
//! let y = x { return y }
//!
//! becomes
//!
//! return x
//! ```
//!
//! A nested binder named `x` is renamed before substitution
//! so that uses of `y` still refer to the outer `x`;
//! iterated aliases need matching iterators.

use crate::lang::{common::source::Span, traits::eq::SyntaxEq};

use crate::lang::il::ast::{ExpKind, Id, Iter};

use crate::pass::structure::{
    ol::ast::*,
    re::{renamer::Renamer, replacer::Replacer},
};

// == Instructions

fn remove_instr(instr_ol: Instr) -> Block {
    remove_instr_kind(instr_ol.node, instr_ol.span)
}

/// Removes alias lets; other instructions only recurse.
fn remove_instr_kind(instr_kind_ol: InstrKind, span: Span) -> Block {
    match instr_kind_ol {
        InstrKind::If(instr_ol) => remove_if_instr(instr_ol, span),
        InstrKind::Hold(instr_ol) => remove_hold_instr(instr_ol, span),
        InstrKind::Case(instr_ol) => remove_case_instr(instr_ol, span),
        InstrKind::Group(instr_ol) => remove_group_instr(instr_ol, span),
        InstrKind::Let(instr_ol) => remove_let_instr(instr_ol, span),
        InstrKind::Rule(instr_ol) => remove_rule_instr(instr_ol, span),
        InstrKind::Result(_) | InstrKind::Return(_) | InstrKind::Debug(_) => {
            let instr = crate::phrase! {node: instr_kind_ol, span: span};
            vec![instr]
        }
    }
}

fn remove_block(block: Block) -> Block {
    let mut block_output = Vec::new();
    for instr_ol in block {
        let block = remove_instr(instr_ol);
        block_output.extend(block);
    }
    block_output
}

// - If instruction

fn remove_if_instr(instr_ol: IfInstr, span: Span) -> Block {
    let IfInstr { exp, iter_exps, block } = instr_ol;
    let block = remove_block(block);
    let instr = IfInstr { exp, iter_exps, block };
    let instr = crate::phrase! {node: InstrKind::If(instr), span: span};
    vec![instr]
}

// - Hold instruction

fn remove_hold_instr(instr_ol: HoldInstr, span: Span) -> Block {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr_ol;
    let block_hold = remove_block(block_hold);
    let block_not_hold = remove_block(block_not_hold);
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    let instr = crate::phrase! {node: InstrKind::Hold(instr), span: span};
    vec![instr]
}

// - Case instruction

fn remove_case_instr(instr_ol: CaseInstr, span: Span) -> Block {
    let CaseInstr { exp, cases, total } = instr_ol;
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            let block = remove_block(block);
            Case { guard, block }
        })
        .collect();
    let instr = CaseInstr { exp, cases, total };
    let instr = crate::phrase! {node: InstrKind::Case(instr), span: span};
    vec![instr]
}

// - Group instruction

fn remove_group_instr(instr_ol: GroupInstr, span: Span) -> Block {
    let GroupInstr { id, rel_signature, exps, block } = instr_ol;
    let block = remove_block(block);
    let instr = GroupInstr { id, rel_signature, exps, block };
    let instr = crate::phrase! {node: InstrKind::Group(instr), span: span};
    vec![instr]
}

// - Let instruction

/// Recognizes one iteration around a variable, such as `x*` or `x?`.
fn iterated_id_exp(exp: &Exp) -> Option<(&Id, &Iter)> {
    let ExpKind::Iter(exp, ExpIter { iter, .. }) = &exp.node else {
        return None;
    };
    let ExpKind::Id(id) = &exp.node else {
        return None;
    };
    Some((id, iter))
}

/// Substitutes an alias such as `let y = x` into its body and drops the let.
fn remove_let_instr(instr_ol: LetInstr, span: Span) -> Block {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
    // let y = x { return y } -> return x
    if let (ExpKind::Id(id_l), ExpKind::Id(id_r)) = (&exp_l.node, &exp_r.node) {
        let renamer = Renamer::singleton(id_l.clone(), id_r.clone());
        let block = renamer.rename_instrs(&mut false, block);
        return remove_block(block);
    }
    // let y* = x* { return y* } -> return x*; iterators must match
    if let (Some((id_l, iter_l)), Some((id_r, iter_r))) =
        (iterated_id_exp(&exp_l), iterated_id_exp(&exp_r))
        && iter_l.syntax_eq(iter_r)
    {
        let renamer = Renamer::singleton(id_l.clone(), id_r.clone());
        let block = renamer.rename_instrs(&mut false, block);
        return remove_block(block);
    }
    // let y = x* { return y } -> return x*
    if let ExpKind::Id(id_l) = &exp_l.node
        && iterated_id_exp(&exp_r).is_some()
    {
        let replacer = Replacer::singleton(id_l.clone(), exp_r);
        let block = replacer.replace_instrs(block);
        return remove_block(block);
    }
    let block = remove_block(block);
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    let instr = crate::phrase! {node: InstrKind::Let(instr), span: span};
    vec![instr]
}

// - Rule instruction

fn remove_rule_instr(instr_ol: RuleInstr, span: Span) -> Block {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr_ol;
    let block = remove_block(block);
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    let instr = crate::phrase! {node: InstrKind::Rule(instr), span: span};
    vec![instr]
}

// == Entry point

/// Removes every alias let in the block.
pub(crate) fn apply(block: Block) -> Block {
    remove_block(block)
}
