//! Remove matches whose expanded variant type has exactly one constructor
//!
//! `remove_if_instr` replaces a singleton-variant test with its OL body.
//! For a value `x` whose type contains only constructor WRAP:
//!
//! ```text
//! if x matches WRAP { return x }
//!
//! becomes
//!
//! return x
//! ```
//!
//! Type aliases are expanded before counting constructors;
//! tests on types with multiple constructors remain.

use crate::lang::common::source::Span;

use crate::lang::il::ast::ExpKind;

use crate::runtime::envs::algo::TDEnv;

use crate::pass::structure::{StructureError, ol::ast::*, opt::overlap::typ_as_variant};

// == Singleton matches

/// Checks for a match on a value whose variant type has a single constructor.
fn is_singleton_match(tdenv: &TDEnv, exp: &Exp) -> Result<bool, StructureError> {
    match &exp.node {
        ExpKind::Match(exp, _) => {
            let typ = crate::phrase!(node: exp.note.as_ref().clone(), span: exp.span);
            let mixops = typ_as_variant(tdenv, &typ)?;
            Ok(mixops.is_some_and(|mixops| mixops.len() == 1))
        }
        _ => Ok(false),
    }
}

// == Instructions

fn remove_instr(tdenv: &TDEnv, instr_ol: Instr) -> Result<Block, StructureError> {
    remove_instr_kind(tdenv, instr_ol.node, instr_ol.span)
}

fn remove_instr_kind(
    tdenv: &TDEnv,
    instr_kind_ol: InstrKind,
    span: Span,
) -> Result<Block, StructureError> {
    match instr_kind_ol {
        InstrKind::If(instr_ol) => remove_if_instr(tdenv, instr_ol, span),
        InstrKind::Hold(instr_ol) => remove_hold_instr(tdenv, instr_ol, span),
        InstrKind::Case(instr_ol) => remove_case_instr(tdenv, instr_ol, span),
        InstrKind::Group(instr_ol) => remove_group_instr(tdenv, instr_ol, span),
        InstrKind::Let(instr_ol) => remove_let_instr(tdenv, instr_ol, span),
        InstrKind::Rule(instr_ol) => remove_rule_instr(tdenv, instr_ol, span),
        InstrKind::Result(_) | InstrKind::Return(_) | InstrKind::Debug(_) => {
            let instr = crate::phrase!(node: instr_kind_ol, span: span);
            Ok(vec![instr])
        }
    }
}

fn remove_block(tdenv: &TDEnv, block: Block) -> Result<Block, StructureError> {
    let mut block_rewritten = Vec::new();
    for instr_ol in block {
        let block = remove_instr(tdenv, instr_ol)?;
        block_rewritten.extend(block);
    }
    Ok(block_rewritten)
}

// - If instruction

/// Replaces an If on a singleton match by its body.
fn remove_if_instr(tdenv: &TDEnv, instr_ol: IfInstr, span: Span) -> Result<Block, StructureError> {
    let IfInstr { exp, iter_exps, block } = instr_ol;
    if is_singleton_match(tdenv, &exp)? {
        return remove_block(tdenv, block);
    }
    let block = remove_block(tdenv, block)?;
    let instr = IfInstr { exp, iter_exps, block };
    let instr = crate::phrase!(node: InstrKind::If(instr), span: span);
    Ok(vec![instr])
}

// - Hold instruction

fn remove_hold_instr(
    tdenv: &TDEnv,
    instr_ol: HoldInstr,
    span: Span,
) -> Result<Block, StructureError> {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr_ol;
    let block_hold = remove_block(tdenv, block_hold)?;
    let block_not_hold = remove_block(tdenv, block_not_hold)?;
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    let instr = crate::phrase!(node: InstrKind::Hold(instr), span: span);
    Ok(vec![instr])
}

// - Case instruction

fn remove_case_instr(
    tdenv: &TDEnv,
    instr_ol: CaseInstr,
    span: Span,
) -> Result<Block, StructureError> {
    let CaseInstr { exp, cases, total } = instr_ol;
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            let block = remove_block(tdenv, block)?;
            let case = Case { guard, block };
            Ok(case)
        })
        .collect::<Result<_, StructureError>>()?;
    let instr = CaseInstr { exp, cases, total };
    let instr = crate::phrase!(node: InstrKind::Case(instr), span: span);
    Ok(vec![instr])
}

// - Group instruction

fn remove_group_instr(
    tdenv: &TDEnv,
    instr_ol: GroupInstr,
    span: Span,
) -> Result<Block, StructureError> {
    let GroupInstr { id, rel_signature, exps, block } = instr_ol;
    let block = remove_block(tdenv, block)?;
    let instr = GroupInstr { id, rel_signature, exps, block };
    let instr = crate::phrase!(node: InstrKind::Group(instr), span: span);
    Ok(vec![instr])
}

// - Let instruction

fn remove_let_instr(
    tdenv: &TDEnv,
    instr_ol: LetInstr,
    span: Span,
) -> Result<Block, StructureError> {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
    let block = remove_block(tdenv, block)?;
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    let instr = crate::phrase!(node: InstrKind::Let(instr), span: span);
    Ok(vec![instr])
}

// - Rule instruction

fn remove_rule_instr(
    tdenv: &TDEnv,
    instr_ol: RuleInstr,
    span: Span,
) -> Result<Block, StructureError> {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr_ol;
    let block = remove_block(tdenv, block)?;
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    let instr = crate::phrase!(node: InstrKind::Rule(instr), span: span);
    Ok(vec![instr])
}

// == Entry point

/// Removes singleton-variant matches throughout the block.
pub(crate) fn apply(tdenv: &TDEnv, block: Block) -> Result<Block, StructureError> {
    remove_block(tdenv, block)
}
