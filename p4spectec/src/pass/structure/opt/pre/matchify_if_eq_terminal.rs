//! Replace equality and inequality tests in OL If conditions with matches
//!
//! Empty options, empty lists, and constructors with no arguments are
//! recognized on either side of the comparison:
//!
//! ```text
//! if xs != [] { return xs }
//!
//! becomes
//!
//! if xs matches nonempty-list { return xs }
//! ```
//!
//! Likewise, equality with an empty option becomes a `none` match,
//! and `x != STOP` becomes `not (x matches STOP)` when `STOP` has no arguments.
//! Only the outer comparison is rewritten; nested expressions are left alone.

use std::rc::Rc;

use crate::lang::common::{
    prim::bool::{CmpOp as BoolCmpOp, UnOp as BoolUnOp},
    source::Span,
};

use crate::lang::il::ast::{CmpOp, ExpKind, ListPattern, OpTyp, OptPattern, Pattern, UnOp};

use crate::pass::structure::ol::ast::*;

// == Expressions

/// Rewrites a terminal comparison into a pattern match; other expressions stay.
fn matchify_exp(exp: Exp) -> Exp {
    let ExpKind::Cmp(op, op_typ, exp_l, exp_r) = exp.node else {
        return exp;
    };
    let exp_kind = match (op, &exp_l.node, &exp_r.node) {
        // x == None or None == x -> x matches None
        (CmpOp::Bool(BoolCmpOp::Eq), _, ExpKind::Opt(None)) => {
            ExpKind::Match(exp_l, Pattern::Opt(OptPattern::None))
        }
        (CmpOp::Bool(BoolCmpOp::Eq), ExpKind::Opt(None), _) => {
            ExpKind::Match(exp_r, Pattern::Opt(OptPattern::None))
        }
        // xs == [] or [] == xs -> xs matches Nil
        (CmpOp::Bool(BoolCmpOp::Eq), _, ExpKind::List(exps)) if exps.is_empty() => {
            ExpKind::Match(exp_l, Pattern::List(ListPattern::Nil))
        }
        (CmpOp::Bool(BoolCmpOp::Eq), ExpKind::List(exps), _) if exps.is_empty() => {
            ExpKind::Match(exp_r, Pattern::List(ListPattern::Nil))
        }
        // x == STOP or STOP == x -> x matches STOP
        (CmpOp::Bool(BoolCmpOp::Eq), _, ExpKind::Case(not_exp)) if not_exp.arity() == 0 => {
            let pattern = Pattern::Case(Rc::clone(not_exp.mixop()));
            ExpKind::Match(exp_l, pattern)
        }
        (CmpOp::Bool(BoolCmpOp::Eq), ExpKind::Case(not_exp), _) if not_exp.arity() == 0 => {
            let pattern = Pattern::Case(Rc::clone(not_exp.mixop()));
            ExpKind::Match(exp_r, pattern)
        }
        // x != None or None != x -> x matches Some
        (CmpOp::Bool(BoolCmpOp::Ne), _, ExpKind::Opt(None)) => {
            ExpKind::Match(exp_l, Pattern::Opt(OptPattern::Some))
        }
        (CmpOp::Bool(BoolCmpOp::Ne), ExpKind::Opt(None), _) => {
            ExpKind::Match(exp_r, Pattern::Opt(OptPattern::Some))
        }
        // xs != [] or [] != xs -> xs matches Cons
        (CmpOp::Bool(BoolCmpOp::Ne), _, ExpKind::List(exps)) if exps.is_empty() => {
            ExpKind::Match(exp_l, Pattern::List(ListPattern::Cons))
        }
        (CmpOp::Bool(BoolCmpOp::Ne), ExpKind::List(exps), _) if exps.is_empty() => {
            ExpKind::Match(exp_r, Pattern::List(ListPattern::Cons))
        }
        // x != STOP or STOP != x -> not (x matches STOP)
        (CmpOp::Bool(BoolCmpOp::Ne), _, ExpKind::Case(not_exp)) if not_exp.arity() == 0 => {
            let pattern = Pattern::Case(Rc::clone(not_exp.mixop()));
            let exp_kind = ExpKind::Match(exp_l, pattern);
            let exp_match =
                crate::note_phrase!(node: exp_kind, note: exp.note.clone(), span: exp.span.clone());
            ExpKind::Un(UnOp::Bool(BoolUnOp::Not), OpTyp::Bool, Box::new(exp_match))
        }
        (CmpOp::Bool(BoolCmpOp::Ne), ExpKind::Case(not_exp), _) if not_exp.arity() == 0 => {
            let pattern = Pattern::Case(Rc::clone(not_exp.mixop()));
            let exp_kind = ExpKind::Match(exp_r, pattern);
            let exp_match =
                crate::note_phrase!(node: exp_kind, note: exp.note.clone(), span: exp.span.clone());
            ExpKind::Un(UnOp::Bool(BoolUnOp::Not), OpTyp::Bool, Box::new(exp_match))
        }
        _ => ExpKind::Cmp(op, op_typ, exp_l, exp_r),
    };
    crate::note_phrase!(node: exp_kind, note: exp.note, span: exp.span)
}

// == Instructions

fn matchify_instr(instr_ol: Instr) -> Instr {
    matchify_instr_kind(instr_ol.node, instr_ol.span)
}

fn matchify_instr_kind(instr_kind_ol: InstrKind, span: Span) -> Instr {
    match instr_kind_ol {
        InstrKind::If(instr_ol) => matchify_if_instr(instr_ol, span),
        InstrKind::Hold(instr_ol) => matchify_hold_instr(instr_ol, span),
        InstrKind::Case(instr_ol) => matchify_case_instr(instr_ol, span),
        InstrKind::Group(instr_ol) => matchify_group_instr(instr_ol, span),
        InstrKind::Let(instr_ol) => matchify_let_instr(instr_ol, span),
        InstrKind::Rule(instr_ol) => matchify_rule_instr(instr_ol, span),
        InstrKind::Result(_) | InstrKind::Return(_) | InstrKind::Debug(_) => {
            crate::phrase! {node: instr_kind_ol, span: span}
        }
    }
}

fn matchify_block(block: Block) -> Block {
    block.into_iter().map(matchify_instr).collect()
}

// - If instruction

/// Rewrites the condition of an If, then its body.
fn matchify_if_instr(instr_ol: IfInstr, span: Span) -> Instr {
    let IfInstr { exp, iter_exps, block } = instr_ol;
    let exp = matchify_exp(exp);
    let block = matchify_block(block);
    let instr = IfInstr { exp, iter_exps, block };
    crate::phrase! {node: InstrKind::If(instr), span: span}
}

// - Hold instruction

fn matchify_hold_instr(instr_ol: HoldInstr, span: Span) -> Instr {
    let HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold } = instr_ol;
    let block_hold = matchify_block(block_hold);
    let block_not_hold = matchify_block(block_not_hold);
    let instr = HoldInstr { id, not_exp, iter_exps, block_hold, block_not_hold };
    crate::phrase! {node: InstrKind::Hold(instr), span: span}
}

// - Case instruction

fn matchify_case_instr(instr_ol: CaseInstr, span: Span) -> Instr {
    let CaseInstr { exp, cases, total } = instr_ol;
    let cases = cases
        .into_iter()
        .map(|case| {
            let Case { guard, block } = case;
            let block = matchify_block(block);
            Case { guard, block }
        })
        .collect::<Vec<_>>();
    let instr = CaseInstr { exp, cases, total };
    crate::phrase! {node: InstrKind::Case(instr), span: span}
}

// - Group instruction

fn matchify_group_instr(instr_ol: GroupInstr, span: Span) -> Instr {
    let GroupInstr { id, rel_signature, exps, block } = instr_ol;
    let block = matchify_block(block);
    let instr = GroupInstr { id, rel_signature, exps, block };
    crate::phrase! {node: InstrKind::Group(instr), span: span}
}

// - Let instruction

fn matchify_let_instr(instr_ol: LetInstr, span: Span) -> Instr {
    let LetInstr { exp_l, exp_r, iter_instrs, block } = instr_ol;
    let block = matchify_block(block);
    let instr = LetInstr { exp_l, exp_r, iter_instrs, block };
    crate::phrase! {node: InstrKind::Let(instr), span: span}
}

// - Rule instruction

fn matchify_rule_instr(instr_ol: RuleInstr, span: Span) -> Instr {
    let RuleInstr { id, not_exp, input_hint, iter_instrs, block } = instr_ol;
    let block = matchify_block(block);
    let instr = RuleInstr { id, not_exp, input_hint, iter_instrs, block };
    crate::phrase! {node: InstrKind::Rule(instr), span: span}
}

// == Entry point

/// Rewrites terminal comparisons in every If condition of the block.
pub(crate) fn apply(block: Block) -> Block {
    matchify_block(block)
}
