//! Remove rule-group wrappers and keep their instructions in source order
//!
//! `remove_block` splices each Group body into its enclosing OL block:
//!
//! ```text
//! group rule_a { if p { return a }; if q { return b } }
//!
//! becomes
//!
//! if p { return a }; if q { return b }
//! ```
//!
//! Nested groups are removed too; their bodies stay in the same order.

use crate::lang::common::source::Span;

use crate::pass::structure::ol::ast::*;

// == Instructions

fn remove_instr(instr_ol: Instr) -> Block {
    remove_instr_kind(instr_ol.node, instr_ol.span)
}

/// Splices a group's body in place; other instructions keep their nesting.
fn remove_instr_kind(instr_kind_ol: InstrKind, span: Span) -> Block {
    match instr_kind_ol {
        InstrKind::If(instr_ol) => remove_if_instr(instr_ol, span),
        InstrKind::Hold(instr_ol) => remove_hold_instr(instr_ol, span),
        InstrKind::Case(instr_ol) => remove_case_instr(instr_ol, span),
        InstrKind::Group(instr_ol) => remove_group_instr(instr_ol),
        InstrKind::Let(instr_ol) => remove_let_instr(instr_ol, span),
        InstrKind::Rule(instr_ol) => remove_rule_instr(instr_ol, span),
        InstrKind::Result(_) | InstrKind::Return(_) | InstrKind::Debug(_) => {
            let instr = crate::phrase! {node: instr_kind_ol, span: span};
            vec![instr]
        }
    }
}

fn remove_block(block: Block) -> Block {
    block.into_iter().flat_map(remove_instr).collect()
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
        .collect::<Vec<_>>();
    let instr = CaseInstr { exp, cases, total };
    let instr = crate::phrase! {node: InstrKind::Case(instr), span: span};
    vec![instr]
}

// - Group instruction

/// Drops the group wrapper, keeping its body.
fn remove_group_instr(instr_ol: GroupInstr) -> Block {
    let GroupInstr { block, .. } = instr_ol;
    remove_block(block)
}

// - Let instruction

fn remove_let_instr(instr_ol: LetInstr, span: Span) -> Block {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
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

/// Removes every rule group in the block.
pub(crate) fn apply(block: Block) -> Block {
    remove_block(block)
}
