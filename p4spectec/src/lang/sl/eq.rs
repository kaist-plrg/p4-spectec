//! Syntax equality for structured-language data
//!
//! Ignores source regions;
//! compares relation signatures with their input hints
//! and instruction identifiers,
//! since both decide how a definition runs.

use crate::lang::traits::eq::SyntaxEq;

use super::ast::*;

// == Syntax equality

// - Parameters

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for ParamKind<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ParamKind::Exp(typ_l, exp_l), ParamKind::Exp(typ_r, exp_r)) => {
                typ_l.syntax_eq(typ_r) && exp_l.syntax_eq(exp_r)
            }
            (
                ParamKind::Def(id_l, tparams_l, params_l, typ_l),
                ParamKind::Def(id_r, tparams_r, params_r, typ_r),
            ) => {
                id_l.syntax_eq(id_r)
                    && tparams_l.syntax_eq(tparams_r)
                    && params_l.syntax_eq(params_r)
                    && typ_l.syntax_eq(typ_r)
            }
            _ => false,
        }
    }
}

// - Instructions

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for InstrKind<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (InstrKind::If(instr_l), InstrKind::If(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Hold(instr_l), InstrKind::Hold(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Case(instr_l), InstrKind::Case(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Group(instr_l), InstrKind::Group(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Let(instr_l), InstrKind::Let(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Rule(instr_l), InstrKind::Rule(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Result(instr_l), InstrKind::Result(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Return(instr_l), InstrKind::Return(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Debug(instr_l), InstrKind::Debug(instr_r)) => instr_l.syntax_eq(instr_r),
            _ => false,
        }
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for IfInstr<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
            && self
                .iter_exps
                .as_slice()
                .syntax_eq(other.iter_exps.as_slice())
            && self.block.syntax_eq(&other.block)
            && self.dangle == other.dangle
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for HoldInstr<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_exp.syntax_eq(&other.not_exp)
            && self
                .iter_exps
                .as_slice()
                .syntax_eq(other.iter_exps.as_slice())
            && self.hold_case.syntax_eq(&other.hold_case)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for CaseInstr<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
            && self.cases.syntax_eq(&other.cases)
            && self.dangle == other.dangle
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for GroupInstr<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.rel_signature.syntax_eq(&other.rel_signature)
            && self.exps.syntax_eq(&other.exps)
            && self.block.syntax_eq(&other.block)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for LetInstr<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp_l.syntax_eq(&other.exp_l)
            && self.exp_r.syntax_eq(&other.exp_r)
            && self.iter_instrs.syntax_eq(&other.iter_instrs)
            && self.block.syntax_eq(&other.block)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for RuleInstr<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_exp.syntax_eq(&other.not_exp)
            && self.input_hint.syntax_eq(&other.input_hint)
            && self.iter_instrs.syntax_eq(&other.iter_instrs)
            && self.block.syntax_eq(&other.block)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for ResultInstr<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.rel_signature.syntax_eq(&other.rel_signature) && self.exps.syntax_eq(&other.exps)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for ReturnInstr<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for DebugInstr<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp) && self.instr.syntax_eq(&other.instr)
    }
}

// - Holding conditions

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for HoldCase<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                HoldCase::Both(block_hold_l, block_not_hold_l),
                HoldCase::Both(block_hold_r, block_not_hold_r),
            ) => {
                block_hold_l.syntax_eq(block_hold_r) && block_not_hold_l.syntax_eq(block_not_hold_r)
            }
            (HoldCase::Hold(block_l, dangle_l), HoldCase::Hold(block_r, dangle_r))
            | (HoldCase::NotHold(block_l, dangle_l), HoldCase::NotHold(block_r, dangle_r)) => {
                block_l.syntax_eq(block_r) && dangle_l == dangle_r
            }
            _ => false,
        }
    }
}

// - Case analysis

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for Guard<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Guard::Bool(value_l), Guard::Bool(value_r)) => value_l == value_r,
            (Guard::Cmp(op_l, typ_l, exp_l), Guard::Cmp(op_r, typ_r, exp_r)) => {
                op_l == op_r && typ_l == typ_r && exp_l.syntax_eq(exp_r)
            }
            (Guard::Sub(typ_l, _), Guard::Sub(typ_r, _)) => typ_l.syntax_eq(typ_r),
            (Guard::Match(pattern_l), Guard::Match(pattern_r)) => pattern_l.syntax_eq(pattern_r),
            (Guard::Mem(exp_l), Guard::Mem(exp_r)) => exp_l.syntax_eq(exp_r),
            _ => false,
        }
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for Case<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.guard.syntax_eq(&other.guard) && self.block.syntax_eq(&other.block)
    }
}

// - Blocks

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for Block<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.as_slice().syntax_eq(other.as_slice())
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for Option<ElseBlock<I, V>> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(block_l), Some(block_r)) => block_l.syntax_eq(block_r),
            (None, None) => true,
            _ => false,
        }
    }
}

// - Table rows

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for TableRow<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exps_input.syntax_eq(&other.exps_input)
            && self.exp.syntax_eq(&other.exp)
            && self.block.syntax_eq(&other.block)
    }
}

// == Type definitions

impl SyntaxEq for TypDef {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Extern(extern_typ_l), Self::Extern(extern_typ_r)) => {
                extern_typ_l.syntax_eq(extern_typ_r)
            }
            (Self::Defined(defined_typ_l), Self::Defined(defined_typ_r)) => {
                defined_typ_l.syntax_eq(defined_typ_r)
            }
            _ => false,
        }
    }
}

impl SyntaxEq for ExternTyp {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id) && self.hints.syntax_eq(&other.hints)
    }
}

impl SyntaxEq for DefinedTyp {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.def_typ.syntax_eq(&other.def_typ)
            && self.hints.syntax_eq(&other.hints)
    }
}

// == Meta-variable definitions

impl SyntaxEq for VarDef {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.typ.syntax_eq(&other.typ)
            && self.hints.syntax_eq(&other.hints)
    }
}

// == Relation definitions

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for RelDef<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Extern(extern_rel_l), Self::Extern(extern_rel_r)) => {
                extern_rel_l.syntax_eq(extern_rel_r)
            }
            (Self::Defined(defined_rel_l), Self::Defined(defined_rel_r)) => {
                defined_rel_l.syntax_eq(defined_rel_r)
            }
            _ => false,
        }
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for ExternRel<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.rel_signature.syntax_eq(&other.rel_signature)
            && self.exps_input.syntax_eq(&other.exps_input)
            && self.hints.syntax_eq(&other.hints)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for DefinedRel<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.rel_signature.syntax_eq(&other.rel_signature)
            && self.exps_input.syntax_eq(&other.exps_input)
            && self.block.syntax_eq(&other.block)
            && self.block_else.syntax_eq(&other.block_else)
            && self.hints.syntax_eq(&other.hints)
    }
}

impl SyntaxEq for RelSignature {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.not_typ.syntax_eq(&other.not_typ) && self.input_hint.syntax_eq(&other.input_hint)
    }
}

// == Meta-function definitions

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for MetaFuncDef<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Extern(extern_func_l), Self::Extern(extern_func_r)) => {
                extern_func_l.syntax_eq(extern_func_r)
            }
            (Self::Builtin(builtin_func_l), Self::Builtin(builtin_func_r)) => {
                builtin_func_l.syntax_eq(builtin_func_r)
            }
            (Self::Table(table_func_l), Self::Table(table_func_r)) => {
                table_func_l.syntax_eq(table_func_r)
            }
            (Self::Defined(defined_func_l), Self::Defined(defined_func_r)) => {
                defined_func_l.syntax_eq(defined_func_r)
            }
            _ => false,
        }
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for ExternFunc<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
            && self.hints.syntax_eq(&other.hints)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for BuiltinFunc<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
            && self.hints.syntax_eq(&other.hints)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for TableFunc<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
            && self.table_rows.syntax_eq(&other.table_rows)
            && self.hints.syntax_eq(&other.hints)
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for DefinedFunc<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
            && self.block.syntax_eq(&other.block)
            && self.block_else.syntax_eq(&other.block_else)
            && self.hints.syntax_eq(&other.hints)
    }
}

// == Definitions

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for DefKind<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DefKind::Typ(typ_def_l), DefKind::Typ(typ_def_r)) => typ_def_l.syntax_eq(typ_def_r),
            (DefKind::Var(var_def_l), DefKind::Var(var_def_r)) => var_def_l.syntax_eq(var_def_r),
            (DefKind::Rel(rel_def_l), DefKind::Rel(rel_def_r)) => rel_def_l.syntax_eq(rel_def_r),
            (DefKind::MetaFunc(meta_func_def_l), DefKind::MetaFunc(meta_func_def_r)) => {
                meta_func_def_l.syntax_eq(meta_func_def_r)
            }
            _ => false,
        }
    }
}

// == Specifications

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for Spec<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.as_slice().syntax_eq(other.as_slice())
    }
}
