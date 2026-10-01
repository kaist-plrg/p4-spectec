//! Free identifiers in optimization-language data

use crate::lang::{common::ds::set::IdSet, traits::free::FreeIds};

use super::ast::*;

// == Free identifiers

// - Instructions

impl FreeIds for InstrKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::If(instr) => instr.free_ids_into(free),
            Self::Hold(instr) => instr.free_ids_into(free),
            Self::Case(instr) => instr.free_ids_into(free),
            Self::Group(instr) => instr.free_ids_into(free),
            Self::Let(instr) => instr.free_ids_into(free),
            Self::Rule(instr) => instr.free_ids_into(free),
            Self::Result(instr) => instr.free_ids_into(free),
            Self::Return(instr) => instr.free_ids_into(free),
            Self::Debug(instr) => instr.free_ids_into(free),
        }
    }
}

impl FreeIds for IfInstr {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
        self.block.free_ids_into(free);
    }
}

impl FreeIds for HoldInstr {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.not_exp.free_ids_into(free);
        self.block_hold.free_ids_into(free);
        self.block_not_hold.free_ids_into(free);
    }
}

impl FreeIds for CaseInstr {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
        self.cases.as_slice().free_ids_into(free);
    }
}

impl FreeIds for GroupInstr {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exps.as_slice().free_ids_into(free);
        self.block.free_ids_into(free);
    }
}

impl FreeIds for LetInstr {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp_l.free_ids_into(free);
        self.exp_r.free_ids_into(free);
        self.block.free_ids_into(free);
    }
}

impl FreeIds for RuleInstr {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.not_exp.free_ids_into(free);
        self.block.free_ids_into(free);
    }
}

impl FreeIds for ResultInstr {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exps.as_slice().free_ids_into(free);
    }
}

impl FreeIds for ReturnInstr {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
    }
}

impl FreeIds for DebugInstr {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
        self.instr.free_ids_into(free);
    }
}

// - Case analysis

impl FreeIds for Case {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.guard.free_ids_into(free);
        self.block.free_ids_into(free);
    }
}

// - Blocks

impl FreeIds for Block {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.as_slice().free_ids_into(free);
    }
}
