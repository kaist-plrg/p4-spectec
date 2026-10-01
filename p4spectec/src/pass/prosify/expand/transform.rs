//! Traverse SL definitions and expand nested calls in their executable blocks
//!
//! `expand_spec` descends from definitions to instructions. Each instruction
//! first expands its nested blocks, then delegates direct call lifting to
//! `lift::lift_instr` until the generated let instructions are stable.
//!
//! For example, the rule application `R($f($g(x)))` becomes
//! `let a = $g(x); let b = $f(a); R(b)`: one let per call, innermost first,
//! naming each intermediate result before it is used.

use crate::lang::{common::ds::set::IdSet, traits::free::FreeIds};

use crate::lang::sl::ast as sl;

use super::lift;

// == Instructions

/// Expands nested blocks and calls owned by an instruction.
fn expand_instr(ids_used: &mut IdSet, instr_sl: sl::Instr) -> sl::Instr {
    // Expand existing child blocks before introducing let instructions
    let instr_kind_sl = expand_instr_kind(ids_used, instr_sl.node);
    let instr_sl = crate::phrase! { node: instr_kind_sl, span: instr_sl.span };
    let (instr_sl, lifted) = lift::lift_instr(ids_used, instr_sl);
    if !lifted {
        return instr_sl;
    }
    // Process nested calls exposed inside the generated let instructions
    expand_instr(ids_used, instr_sl)
}

/// Expands the blocks nested in one instruction kind.
fn expand_instr_kind(ids_used: &mut IdSet, instr_kind_sl: sl::InstrKind) -> sl::InstrKind {
    match instr_kind_sl {
        sl::InstrKind::Let(instr_sl) => sl::InstrKind::Let(expand_let_instr(ids_used, instr_sl)),
        sl::InstrKind::Rule(instr_sl) => sl::InstrKind::Rule(expand_rule_instr(ids_used, instr_sl)),
        sl::InstrKind::If(instr_sl) => sl::InstrKind::If(expand_if_instr(ids_used, instr_sl)),
        sl::InstrKind::Hold(instr_sl) => sl::InstrKind::Hold(expand_hold_instr(ids_used, instr_sl)),
        sl::InstrKind::Case(instr_sl) => sl::InstrKind::Case(expand_case_instr(ids_used, instr_sl)),
        sl::InstrKind::Group(instr_sl) => {
            sl::InstrKind::Group(expand_group_instr(ids_used, instr_sl))
        }
        sl::InstrKind::Debug(instr_sl) => {
            sl::InstrKind::Debug(expand_debug_instr(ids_used, instr_sl))
        }
        sl::InstrKind::Result(instr_sl) => sl::InstrKind::Result(instr_sl),
        sl::InstrKind::Return(instr_sl) => sl::InstrKind::Return(instr_sl),
    }
}

// - Let instruction

/// Expands the body of a let.
fn expand_let_instr(ids_used: &mut IdSet, mut instr_sl: sl::LetInstr) -> sl::LetInstr {
    instr_sl.block = expand_block(ids_used, instr_sl.block);
    instr_sl
}

// - Rule instruction

/// Expands the body of a rule call.
fn expand_rule_instr(ids_used: &mut IdSet, mut instr_sl: sl::RuleInstr) -> sl::RuleInstr {
    instr_sl.block = expand_block(ids_used, instr_sl.block);
    instr_sl
}

// - If instruction

/// Expands the then-block.
fn expand_if_instr(ids_used: &mut IdSet, mut instr_sl: sl::IfInstr) -> sl::IfInstr {
    instr_sl.block = expand_block(ids_used, instr_sl.block);
    instr_sl
}

// - Hold instruction

/// Expands the branches of a hold.
fn expand_hold_instr(ids_used: &mut IdSet, mut instr_sl: sl::HoldInstr) -> sl::HoldInstr {
    instr_sl.hold_case = expand_hold_case(ids_used, instr_sl.hold_case);
    instr_sl
}

/// Expands whichever branches a hold has.
fn expand_hold_case(ids_used: &mut IdSet, hold_case_sl: sl::HoldCase) -> sl::HoldCase {
    match hold_case_sl {
        sl::HoldCase::Both(block_hold_sl, block_not_hold_sl) => sl::HoldCase::Both(
            expand_block(ids_used, block_hold_sl),
            expand_block(ids_used, block_not_hold_sl),
        ),
        sl::HoldCase::Hold(block_sl, dangle) => {
            sl::HoldCase::Hold(expand_block(ids_used, block_sl), dangle)
        }
        sl::HoldCase::NotHold(block_sl, dangle) => {
            sl::HoldCase::NotHold(expand_block(ids_used, block_sl), dangle)
        }
    }
}

// - Case instruction

/// Expands every arm's block.
fn expand_case_instr(ids_used: &mut IdSet, mut instr_sl: sl::CaseInstr) -> sl::CaseInstr {
    for case_sl in &mut instr_sl.cases {
        case_sl.block = expand_block(ids_used, std::mem::take(&mut case_sl.block));
    }
    instr_sl
}

// - Group instruction

/// Expands a rule group's body.
fn expand_group_instr(ids_used: &mut IdSet, mut instr_sl: sl::GroupInstr) -> sl::GroupInstr {
    instr_sl.block = expand_block(ids_used, instr_sl.block);
    instr_sl
}

// - Debug instruction

/// Expands the instruction a debug wraps.
fn expand_debug_instr(ids_used: &mut IdSet, mut instr_sl: sl::DebugInstr) -> sl::DebugInstr {
    instr_sl.instr = Box::new(expand_instr(ids_used, *instr_sl.instr));
    instr_sl
}

// - Block

/// Expands each instruction of a block in order.
fn expand_block(ids_used: &mut IdSet, block_sl: sl::Block) -> sl::Block {
    block_sl
        .into_iter()
        .map(|instr_sl| expand_instr(ids_used, instr_sl))
        .collect()
}

// == Relation definitions

/// Expands a relation; extern relations have no blocks.
fn expand_rel_def(def_rel_sl: sl::RelDef) -> sl::RelDef {
    match def_rel_sl {
        sl::RelDef::Extern(def_rel_sl) => sl::RelDef::Extern(def_rel_sl),
        sl::RelDef::Defined(def_rel_sl) => sl::RelDef::Defined(expand_defined_rel_def(def_rel_sl)),
    }
}

// - Defined relation definition

/// Expands the main and otherwise blocks of a defined relation.
fn expand_defined_rel_def(mut def_rel_sl: sl::DefinedRel) -> sl::DefinedRel {
    // Reserve every identifier visible to either branch
    let mut ids_used = def_rel_sl
        .exps_input
        .free_ids()
        .union(def_rel_sl.block.free_ids());
    if let Some(block_else_sl) = &def_rel_sl.block_else {
        ids_used.append(block_else_sl.free_ids());
    }

    // Give the main and otherwise blocks independent fresh-name scopes
    let mut ids_body = ids_used.clone();
    def_rel_sl.block = expand_block(&mut ids_body, def_rel_sl.block);
    def_rel_sl.block_else = def_rel_sl
        .block_else
        .map(|block_else_sl| expand_block(&mut ids_used, block_else_sl));
    def_rel_sl
}

// == Meta-function definitions

/// Expands a function; extern and builtin functions have no blocks.
fn expand_func_def(def_func_sl: sl::MetaFuncDef) -> sl::MetaFuncDef {
    match def_func_sl {
        sl::MetaFuncDef::Extern(def_func_sl) => sl::MetaFuncDef::Extern(def_func_sl),
        sl::MetaFuncDef::Builtin(def_func_sl) => sl::MetaFuncDef::Builtin(def_func_sl),
        sl::MetaFuncDef::Table(def_func_sl) => {
            sl::MetaFuncDef::Table(expand_table_func_def(def_func_sl))
        }
        sl::MetaFuncDef::Defined(def_func_sl) => {
            sl::MetaFuncDef::Defined(expand_defined_func_def(def_func_sl))
        }
    }
}

// - Table function definition

/// Expands every row of a table function.
fn expand_table_func_def(mut def_func_sl: sl::TableFunc) -> sl::TableFunc {
    def_func_sl.table_rows = def_func_sl
        .table_rows
        .into_iter()
        .map(expand_table_row)
        .collect();
    def_func_sl
}

/// Expands a row's block, with the row's own names reserved.
fn expand_table_row(mut row_sl: sl::TableRow) -> sl::TableRow {
    let mut ids_used = row_sl
        .exps_input
        .free_ids()
        .union(row_sl.exp.free_ids())
        .union(row_sl.block.free_ids());
    row_sl.block = expand_block(&mut ids_used, row_sl.block);
    row_sl
}

// - Defined function definition

/// Expands the main and otherwise blocks of a defined meta-function.
fn expand_defined_func_def(mut def_func_sl: sl::DefinedFunc) -> sl::DefinedFunc {
    // Reserve every identifier visible to either branch
    let mut ids_used = def_func_sl
        .params
        .free_ids()
        .union(def_func_sl.block.free_ids());
    if let Some(block_else_sl) = &def_func_sl.block_else {
        ids_used.append(block_else_sl.free_ids());
    }

    // Give the main and otherwise blocks independent fresh-name scopes
    let mut ids_body = ids_used.clone();
    def_func_sl.block = expand_block(&mut ids_body, def_func_sl.block);
    def_func_sl.block_else = def_func_sl
        .block_else
        .map(|block_else_sl| expand_block(&mut ids_used, block_else_sl));
    def_func_sl
}

// == Definitions

/// Expands one definition, keeping its span.
fn expand_def(def_sl: sl::Def) -> sl::Def {
    let def_kind_sl = expand_def_kind(def_sl.node);
    crate::phrase! { node: def_kind_sl, span: def_sl.span }
}

/// Expands relations and functions; types and variables have no blocks.
fn expand_def_kind(def_kind_sl: sl::DefKind) -> sl::DefKind {
    match def_kind_sl {
        sl::DefKind::Typ(def_typ_sl) => sl::DefKind::Typ(def_typ_sl),
        sl::DefKind::Var(def_var_sl) => sl::DefKind::Var(def_var_sl),
        sl::DefKind::Rel(def_rel_sl) => sl::DefKind::Rel(expand_rel_def(def_rel_sl)),
        sl::DefKind::MetaFunc(def_func_sl) => sl::DefKind::MetaFunc(expand_func_def(def_func_sl)),
    }
}

// == Entry point

/// Expands nested calls throughout a structured-language specification.
pub(super) fn expand_spec(spec_sl: sl::Spec) -> sl::Spec {
    spec_sl.into_iter().map(expand_def).collect()
}
