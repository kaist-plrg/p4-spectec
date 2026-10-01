//! Preserve semantic fallthrough when lowering optimized instructions to SL
//!
//! With no fallback, `If(p, [A])` gets `dangle=true`;
//! an explicit fallback makes it false.
//! A total `Case` also gets `dangle=false`.

use crate::lang::sl::ast as sl;

use super::ol::ast as ol;

// == Instructions

/// Lowers an instruction to SL, propagating the dangle flag into its blocks.
fn insert_instr(instr_ol: ol::Instr, dangle: bool) -> sl::Instr {
    let instr_kind_sl = insert_instr_kind(instr_ol.node, dangle);
    crate::phrase!(node: instr_kind_sl, span: instr_ol.span)
}

fn insert_instr_kind(instr_kind_ol: ol::InstrKind, dangle: bool) -> sl::InstrKind {
    match instr_kind_ol {
        ol::InstrKind::If(instr_ol) => insert_if_instr(instr_ol, dangle),
        ol::InstrKind::Hold(instr_ol) => insert_hold_instr(instr_ol, dangle),
        ol::InstrKind::Case(instr_ol) => insert_case_instr(instr_ol, dangle),
        ol::InstrKind::Group(instr_ol) => insert_group_instr(instr_ol, dangle),
        ol::InstrKind::Let(instr_ol) => insert_let_instr(instr_ol, dangle),
        ol::InstrKind::Rule(instr_ol) => insert_rule_instr(instr_ol, dangle),
        ol::InstrKind::Result(instr_ol) => insert_result_instr(instr_ol),
        ol::InstrKind::Return(instr_ol) => insert_return_instr(instr_ol),
        ol::InstrKind::Debug(instr_ol) => insert_debug_instr(instr_ol, dangle),
    }
}

// - If instruction

fn insert_if_instr(instr_ol: ol::IfInstr, dangle: bool) -> sl::InstrKind {
    let ol::IfInstr { exp, iter_exps, block: block_ol } = instr_ol;
    let block = insert_block(block_ol, dangle);
    sl::InstrKind::If(sl::IfInstr { exp, iter_exps, block, dangle })
}

// - Hold instruction

/// Lowers a hold instruction; only the present branches form the SL hold case.
fn insert_hold_instr(instr_ol: ol::HoldInstr, dangle: bool) -> sl::InstrKind {
    let ol::HoldInstr {
        id,
        not_exp,
        iter_exps,
        block_hold: block_hold_ol,
        block_not_hold: block_not_hold_ol,
    } = instr_ol;
    let block_hold_sl = insert_block(block_hold_ol, dangle);
    let block_not_hold_sl = insert_block(block_not_hold_ol, dangle);
    let hold_case = match (block_hold_sl.is_empty(), block_not_hold_sl.is_empty()) {
        // Lowering adds a continuation; optimization preserves its terminal
        (true, true) => unreachable!("hold must retain a continuation"),
        (false, true) => sl::HoldCase::Hold(block_hold_sl, dangle),
        (true, false) => sl::HoldCase::NotHold(block_not_hold_sl, dangle),
        (false, false) => sl::HoldCase::Both(block_hold_sl, block_not_hold_sl),
    };
    sl::InstrKind::Hold(sl::HoldInstr { id, not_exp, iter_exps, hold_case })
}

// - Case instruction

fn insert_case(case_ol: ol::Case, dangle: bool) -> sl::Case {
    let ol::Case { guard, block: block_ol } = case_ol;
    let block = insert_block(block_ol, dangle);
    sl::Case { guard, block }
}

/// Lowers a case analysis; a total one never dangles.
fn insert_case_instr(instr_ol: ol::CaseInstr, dangle: bool) -> sl::InstrKind {
    let ol::CaseInstr { exp, cases: cases_ol, total } = instr_ol;
    let cases = cases_ol
        .into_iter()
        .map(|case_ol| insert_case(case_ol, dangle))
        .collect();
    sl::InstrKind::Case(sl::CaseInstr { exp, cases, dangle: dangle && !total })
}

// - Group instruction

fn insert_group_instr(instr_ol: ol::GroupInstr, dangle: bool) -> sl::InstrKind {
    let ol::GroupInstr { id, rel_signature, exps, block: block_ol } = instr_ol;
    let block = insert_block(block_ol, dangle);
    sl::InstrKind::Group(sl::GroupInstr { id, rel_signature, exps, block })
}

// - Let instruction

fn insert_let_instr(instr_ol: ol::LetInstr, dangle: bool) -> sl::InstrKind {
    let ol::LetInstr { exp_l, exp_r, iter_instrs, block: block_ol } = instr_ol;
    let block = insert_block(block_ol, dangle);
    sl::InstrKind::Let(sl::LetInstr { exp_l, exp_r, iter_instrs, block })
}

// - Rule instruction

fn insert_rule_instr(instr_ol: ol::RuleInstr, dangle: bool) -> sl::InstrKind {
    let ol::RuleInstr { id, not_exp, input_hint, iter_instrs, block: block_ol } = instr_ol;
    let block = insert_block(block_ol, dangle);
    sl::InstrKind::Rule(sl::RuleInstr { id, not_exp, input_hint, iter_instrs, block })
}

// - Result instruction

fn insert_result_instr(instr_ol: ol::ResultInstr) -> sl::InstrKind {
    let ol::ResultInstr { rel_signature, exps } = instr_ol;
    sl::InstrKind::Result(sl::ResultInstr { rel_signature, exps })
}

// - Return instruction

fn insert_return_instr(instr_ol: ol::ReturnInstr) -> sl::InstrKind {
    let ol::ReturnInstr { exp } = instr_ol;
    sl::InstrKind::Return(sl::ReturnInstr { exp })
}

// - Debug instruction

fn insert_debug_instr(instr_ol: ol::DebugInstr, dangle: bool) -> sl::InstrKind {
    let ol::DebugInstr { exp, instr: instr_ol } = instr_ol;
    let instr_sl = insert_instr(*instr_ol, dangle);
    sl::InstrKind::Debug(sl::DebugInstr { exp, instr: Box::new(instr_sl) })
}

// == Blocks

fn insert_block(block_ol: ol::Block, dangle: bool) -> sl::Block {
    block_ol
        .into_iter()
        .map(|instr_ol| insert_instr(instr_ol, dangle))
        .collect()
}

// == Block insertion strategies

fn insert_nothing(block_ol: ol::Block) -> sl::Block {
    insert_block(block_ol, false)
}

fn insert_dangle(block_ol: ol::Block) -> sl::Block {
    insert_block(block_ol, true)
}

// == Fallback handling

/// Lowers a block and its optional otherwise block to SL.
///
/// Without an otherwise block, failing guards fall through (`dangle=true`).
pub(crate) fn instrument(
    block_ol: ol::Block,
    block_else_ol: Option<ol::Block>,
) -> (sl::Block, Option<sl::Block>) {
    match block_else_ol {
        // With an otherwise block nothing dangles
        Some(block_else_ol) => {
            let block = insert_nothing(block_ol);
            let block_else = insert_nothing(block_else_ol);
            (block, Some(block_else))
        }
        // Without one, failing guards fall through
        None => {
            let block = insert_dangle(block_ol);
            (block, None)
        }
    }
}

/// Lowers a block that never falls through, such as a table row.
pub(crate) fn instrument_without_else(block_ol: ol::Block) -> sl::Block {
    insert_nothing(block_ol)
}
