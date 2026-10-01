//! Syntax equality for optimization-language data
//!
//! Ignores source regions.

use crate::lang::traits::eq::SyntaxEq;

use super::ast::*;

// == Syntax equality

// - Instructions

impl SyntaxEq for InstrKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (InstrKind::If(instr_a), InstrKind::If(instr_b)) => instr_a.syntax_eq(instr_b),
            (InstrKind::Hold(instr_a), InstrKind::Hold(instr_b)) => instr_a.syntax_eq(instr_b),
            (InstrKind::Case(instr_a), InstrKind::Case(instr_b)) => instr_a.syntax_eq(instr_b),
            (InstrKind::Group(instr_a), InstrKind::Group(instr_b)) => instr_a.syntax_eq(instr_b),
            (InstrKind::Let(instr_a), InstrKind::Let(instr_b)) => instr_a.syntax_eq(instr_b),
            (InstrKind::Rule(instr_a), InstrKind::Rule(instr_b)) => instr_a.syntax_eq(instr_b),
            (InstrKind::Result(instr_a), InstrKind::Result(instr_b)) => instr_a.syntax_eq(instr_b),
            (InstrKind::Return(instr_a), InstrKind::Return(instr_b)) => instr_a.syntax_eq(instr_b),
            (InstrKind::Debug(instr_a), InstrKind::Debug(instr_b)) => instr_a.syntax_eq(instr_b),
            _ => false,
        }
    }
}

impl SyntaxEq for IfInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
            && self.iter_exps.syntax_eq(&other.iter_exps)
            && self.block.syntax_eq(&other.block)
    }
}

impl SyntaxEq for HoldInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_exp.syntax_eq(&other.not_exp)
            && self.iter_exps.syntax_eq(&other.iter_exps)
            && self.block_hold.syntax_eq(&other.block_hold)
            && self.block_not_hold.syntax_eq(&other.block_not_hold)
    }
}

impl SyntaxEq for CaseInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
            && self.cases.syntax_eq(&other.cases)
            && self.total == other.total
    }
}

impl SyntaxEq for GroupInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.rel_signature.syntax_eq(&other.rel_signature)
            && self.exps.syntax_eq(&other.exps)
            && self.block.syntax_eq(&other.block)
    }
}

impl SyntaxEq for LetInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp_l.syntax_eq(&other.exp_l)
            && self.exp_r.syntax_eq(&other.exp_r)
            && self.iter_instrs.syntax_eq(&other.iter_instrs)
            && self.block.syntax_eq(&other.block)
    }
}

impl SyntaxEq for RuleInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_exp.syntax_eq(&other.not_exp)
            && self.input_hint.syntax_eq(&other.input_hint)
            && self.iter_instrs.syntax_eq(&other.iter_instrs)
            && self.block.syntax_eq(&other.block)
    }
}

impl SyntaxEq for ResultInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.rel_signature.syntax_eq(&other.rel_signature) && self.exps.syntax_eq(&other.exps)
    }
}

impl SyntaxEq for ReturnInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
    }
}

impl SyntaxEq for DebugInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp) && self.instr.syntax_eq(&other.instr)
    }
}

// - Case analysis

impl SyntaxEq for Case {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.guard.syntax_eq(&other.guard) && self.block.syntax_eq(&other.block)
    }
}

// - Blocks

impl SyntaxEq for Block {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.as_slice().syntax_eq(other.as_slice())
    }
}

impl SyntaxEq for Option<ElseBlock> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(block_a), Some(block_b)) => block_a.syntax_eq(block_b),
            (None, None) => true,
            _ => false,
        }
    }
}
