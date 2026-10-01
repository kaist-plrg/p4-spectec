//! Free identifiers in structured-language data
//!
//! Only expressions contribute identifiers;
//! instructions collect from their expressions and blocks.

use crate::lang::{common::ds::set::IdSet, traits::free::FreeIds};

use super::ast::*;

// == Free identifiers

// - Parameters

impl FreeIds for ParamKind {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Exp(_, exp) => exp.free_ids(),
            Self::Def(..) => IdSet::new(),
        }
    }
}

// - Instructions

impl FreeIds for InstrKind {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::If(instr) => instr.free_ids(),
            Self::Hold(instr) => instr.free_ids(),
            Self::Case(instr) => instr.free_ids(),
            Self::Group(instr) => instr.free_ids(),
            Self::Let(instr) => instr.free_ids(),
            Self::Rule(instr) => instr.free_ids(),
            Self::Result(instr) => instr.free_ids(),
            Self::Return(instr) => instr.free_ids(),
            Self::Debug(instr) => instr.free_ids(),
        }
    }
}

impl FreeIds for IfInstr {
    fn free_ids(&self) -> IdSet {
        self.exp.free_ids().union(self.block.free_ids())
    }
}

impl FreeIds for HoldInstr {
    fn free_ids(&self) -> IdSet {
        self.not_exp.free_ids()
    }
}

impl FreeIds for CaseInstr {
    fn free_ids(&self) -> IdSet {
        self.exp.free_ids().union(self.cases.as_slice().free_ids())
    }
}

impl FreeIds for GroupInstr {
    fn free_ids(&self) -> IdSet {
        self.exps.as_slice().free_ids().union(self.block.free_ids())
    }
}

impl FreeIds for LetInstr {
    fn free_ids(&self) -> IdSet {
        self.exp_l
            .free_ids()
            .union(self.exp_r.free_ids())
            .union(self.block.free_ids())
    }
}

impl FreeIds for RuleInstr {
    fn free_ids(&self) -> IdSet {
        self.not_exp.free_ids().union(self.block.free_ids())
    }
}

impl FreeIds for ResultInstr {
    fn free_ids(&self) -> IdSet {
        self.exps.as_slice().free_ids()
    }
}

impl FreeIds for ReturnInstr {
    fn free_ids(&self) -> IdSet {
        self.exp.free_ids()
    }
}

impl FreeIds for DebugInstr {
    fn free_ids(&self) -> IdSet {
        self.exp.free_ids().union(self.instr.free_ids())
    }
}

// - Holding conditions

impl FreeIds for HoldCase {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Both(block_l, block_r) => block_l.free_ids().union(block_r.free_ids()),
            Self::Hold(block, _) | Self::NotHold(block, _) => block.free_ids(),
        }
    }
}

// - Case analysis

impl FreeIds for Guard {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Cmp(_, _, exp) | Self::Mem(exp) => exp.free_ids(),
            Self::Bool(_) | Self::Sub(..) | Self::Match(_) => IdSet::new(),
        }
    }
}

impl FreeIds for Case {
    fn free_ids(&self) -> IdSet {
        self.guard.free_ids().union(self.block.free_ids())
    }
}

// - Blocks

impl FreeIds for Block {
    fn free_ids(&self) -> IdSet {
        self.as_slice().free_ids()
    }
}

// - Table rows

impl FreeIds for TableRow {
    fn free_ids(&self) -> IdSet {
        self.exps_input
            .as_slice()
            .free_ids()
            .union(self.exp.free_ids())
            .union(self.block.free_ids())
    }
}

// == Type definitions

impl FreeIds for TypDef {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Extern(extern_typ) => extern_typ.free_ids(),
            Self::Defined(defined_typ) => defined_typ.free_ids(),
        }
    }
}

impl FreeIds for ExternTyp {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for DefinedTyp {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// == Meta-variable definitions

impl FreeIds for VarDef {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// == Relation definitions

impl FreeIds for RelDef {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Extern(extern_rel) => extern_rel.free_ids(),
            Self::Defined(defined_rel) => defined_rel.free_ids(),
        }
    }
}

impl FreeIds for ExternRel {
    fn free_ids(&self) -> IdSet {
        self.exps_input.as_slice().free_ids()
    }
}

impl FreeIds for DefinedRel {
    fn free_ids(&self) -> IdSet {
        self.exps_input
            .as_slice()
            .free_ids()
            .union(self.block.free_ids())
            .union(self.block_else.free_ids())
    }
}

impl FreeIds for RelSignature {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// == Meta-function definitions

impl FreeIds for MetaFuncDef {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Extern(extern_func) => extern_func.free_ids(),
            Self::Builtin(builtin_func) => builtin_func.free_ids(),
            Self::Table(table_func) => table_func.free_ids(),
            Self::Defined(defined_func) => defined_func.free_ids(),
        }
    }
}

impl FreeIds for ExternFunc {
    fn free_ids(&self) -> IdSet {
        self.params.as_slice().free_ids()
    }
}

impl FreeIds for BuiltinFunc {
    fn free_ids(&self) -> IdSet {
        self.params.as_slice().free_ids()
    }
}

impl FreeIds for TableFunc {
    fn free_ids(&self) -> IdSet {
        self.params
            .as_slice()
            .free_ids()
            .union(self.table_rows.as_slice().free_ids())
    }
}

impl FreeIds for DefinedFunc {
    fn free_ids(&self) -> IdSet {
        self.params
            .as_slice()
            .free_ids()
            .union(self.block.free_ids())
            .union(self.block_else.free_ids())
    }
}

// == Definitions

impl FreeIds for DefKind {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Typ(typ_def) => typ_def.free_ids(),
            Self::Var(var_def) => var_def.free_ids(),
            Self::Rel(rel_def) => rel_def.free_ids(),
            Self::MetaFunc(meta_func_def) => meta_func_def.free_ids(),
        }
    }
}

// == Specifications

impl FreeIds for Spec {
    fn free_ids(&self) -> IdSet {
        self.as_slice().free_ids()
    }
}
