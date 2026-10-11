//! Mark variant case analyses whose guards cover exactly all constructors
//!
//! For a type `A | B`, branches matching `A` and `B` set `total=true`.

use std::{collections::BTreeSet, rc::Rc};

use crate::lang::il::ast::{Mixop, Pattern};

use crate::runtime::envs::algo::TDEnv;

use super::{error::StructureError, ol::ast::*, opt::overlap::typ_as_variant};

// == Variant coverage

/// Collects the constructors matched when every guard is a variant match.
fn find_variant_case_analysis(
    tdenv: &TDEnv,
    cases: &[Case],
) -> Result<Option<Vec<Rc<Mixop>>>, StructureError> {
    let mut mixops = Vec::new();
    for case in cases {
        match &case.guard {
            // A subtype guard covers every constructor of the subtype
            Guard::Sub(typ, _) => {
                // Casify admits subtype guards only after proving variant overlap
                let mixops_sub =
                    typ_as_variant(tdenv, typ)?.expect("case subtype guard has a variant type");
                mixops.extend(mixops_sub);
            }
            Guard::Match(Pattern::Case(mixop)) => mixops.push(Rc::clone(mixop)),
            _ => return Ok(None),
        }
    }
    Ok(Some(mixops))
}

// == Instructions

/// Marks total case analyses inside an instruction.
fn totalize_instr(tdenv: &TDEnv, instr: Instr) -> Result<Instr, StructureError> {
    let instr_kind = totalize_instr_kind(tdenv, instr.node)?;
    Ok(crate::phrase!(node: instr_kind, span: instr.span))
}

fn totalize_instr_kind(tdenv: &TDEnv, instr_kind: InstrKind) -> Result<InstrKind, StructureError> {
    match instr_kind {
        InstrKind::If(instr) => totalize_if_instr(tdenv, instr),
        InstrKind::Hold(instr) => totalize_hold_instr(tdenv, instr),
        InstrKind::Case(instr) => totalize_case_instr(tdenv, instr),
        InstrKind::Group(instr) => totalize_group_instr(tdenv, instr),
        InstrKind::Let(instr) => totalize_let_instr(tdenv, instr),
        InstrKind::Rule(instr) => totalize_rule_instr(tdenv, instr),
        instr_kind => Ok(instr_kind),
    }
}

// - If instruction

fn totalize_if_instr(tdenv: &TDEnv, instr: IfInstr) -> Result<InstrKind, StructureError> {
    let IfInstr { exp, iter_exps, block } = instr;
    let block = totalize_block(tdenv, block)?;
    Ok(InstrKind::If(IfInstr { exp, iter_exps, block }))
}

// - Hold instruction

fn totalize_hold_instr(tdenv: &TDEnv, instr: HoldInstr) -> Result<InstrKind, StructureError> {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr;
    let block_hold = totalize_block(tdenv, block_hold)?;
    let block_not_hold = totalize_block(tdenv, block_not_hold)?;
    Ok(InstrKind::Hold(HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold }))
}

// - Case instruction

fn totalize_case(tdenv: &TDEnv, case: Case) -> Result<Case, StructureError> {
    let Case { guard, block } = case;
    let block = totalize_block(tdenv, block)?;
    Ok(Case { guard, block })
}

/// Marks a case analysis total when its guards cover every constructor.
fn totalize_case_instr(tdenv: &TDEnv, instr: CaseInstr) -> Result<InstrKind, StructureError> {
    let CaseInstr { exp, cases, total } = instr;
    let cases = cases
        .into_iter()
        .map(|case| totalize_case(tdenv, case))
        .collect::<Result<Vec<_>, _>>()?;
    let total = if let Some(mixops_case) = find_variant_case_analysis(tdenv, &cases)? {
        let typ = crate::phrase!(node: exp.note.as_ref().clone(), span: exp.span.clone());
        // Typed constructor guards and variant subtyping determine the target type
        let mixops_total =
            typ_as_variant(tdenv, &typ)?.expect("variant case analysis has a variant target");
        // Compare the matched constructors with the type's constructors as sets
        let mixops_total: BTreeSet<_> = mixops_total.into_iter().collect();
        let mixops_case: BTreeSet<_> = mixops_case.into_iter().collect();
        mixops_case == mixops_total
    } else {
        total
    };
    Ok(InstrKind::Case(CaseInstr { exp, cases, total }))
}

// - Group instruction

fn totalize_group_instr(tdenv: &TDEnv, instr: GroupInstr) -> Result<InstrKind, StructureError> {
    let GroupInstr { id, rel_signature, exps, block } = instr;
    let block = totalize_block(tdenv, block)?;
    Ok(InstrKind::Group(GroupInstr { id, rel_signature, exps, block }))
}

// - Let instruction

fn totalize_let_instr(tdenv: &TDEnv, instr: LetInstr) -> Result<InstrKind, StructureError> {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr;
    let block = totalize_block(tdenv, block)?;
    Ok(InstrKind::Let(LetInstr { exp_l, exp_r, iter_instrs, block }))
}

// - Rule instruction

fn totalize_rule_instr(tdenv: &TDEnv, instr: RuleInstr) -> Result<InstrKind, StructureError> {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr;
    let block = totalize_block(tdenv, block)?;
    Ok(InstrKind::Rule(RuleInstr { id, not_exp, input_hint, iter_instrs, block }))
}

// == Entry point

fn totalize_block(tdenv: &TDEnv, block: Block) -> Result<Block, StructureError> {
    block
        .into_iter()
        .map(|instr| totalize_instr(tdenv, instr))
        .collect()
}

// == Entry points

/// Marks total variant case analyses throughout a block.
pub(crate) fn totalize(tdenv: &TDEnv, block: Block) -> Result<Block, StructureError> {
    totalize_block(tdenv, block)
}
