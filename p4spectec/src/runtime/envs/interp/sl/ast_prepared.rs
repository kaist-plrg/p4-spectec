//! Slot instantiation of shared SL syntax
//!
//! The `Prepare` impls rebuild each node with slots in place of names,
//! reserving slots in the callable's `FrameLayout` as they go.
//! Extern and builtin definitions prepare their parameters only.

use crate::lang::data::var::{IdSlot, VarSlot};

use crate::lang::sl::ast as source;

pub use crate::lang::sl::ast::{DefinedTyp, ExternTyp, RelSignature, TypDef, VarDef};

use crate::runtime::envs::interp::shared::frame::FrameLayout;

use crate::interp::shared::prepare::Prepare;

pub use crate::interp::shared::prepare::ast::*;

// == Prepared syntax

// - Parameters

pub type Param = source::Param<IdSlot, VarSlot>;
pub type ParamKind = source::ParamKind<IdSlot, VarSlot>;

// - Dangling

pub type Dangle = source::Dangle;

// - Holding conditions

pub type HoldCase = source::HoldCase<IdSlot, VarSlot>;

// - Case analysis

pub type Guard = source::Guard<IdSlot, VarSlot>;
pub type Case = source::Case<IdSlot, VarSlot>;

// - Instructions

pub type Instr = source::Instr<IdSlot, VarSlot>;
pub type InstrKind = source::InstrKind<IdSlot, VarSlot>;
pub type IfInstr = source::IfInstr<IdSlot, VarSlot>;
pub type HoldInstr = source::HoldInstr<IdSlot, VarSlot>;
pub type CaseInstr = source::CaseInstr<IdSlot, VarSlot>;
pub type GroupInstr = source::GroupInstr<IdSlot, VarSlot>;
pub type LetInstr = source::LetInstr<IdSlot, VarSlot>;
pub type RuleInstr = source::RuleInstr<IdSlot, VarSlot>;
pub type ResultInstr = source::ResultInstr<IdSlot, VarSlot>;
pub type ReturnInstr = source::ReturnInstr<IdSlot, VarSlot>;
pub type DebugInstr = source::DebugInstr<IdSlot, VarSlot>;
pub type InstrIter = PremIter;

// - Blocks

pub type Block = source::Block<IdSlot, VarSlot>;
pub type ElseBlock = source::ElseBlock<IdSlot, VarSlot>;

// - Table rows

pub type TableRow = source::TableRow<IdSlot, VarSlot>;

// - Relation definitions

pub type RelDef = source::RelDef<IdSlot, VarSlot>;
pub type ExternRel = source::ExternRel<IdSlot, VarSlot>;
pub type DefinedRel = source::DefinedRel<IdSlot, VarSlot>;

// - Meta-function definitions

pub type MetaFuncDef = source::MetaFuncDef<IdSlot, VarSlot>;
pub type ExternFunc = source::ExternFunc<IdSlot, VarSlot>;
pub type BuiltinFunc = source::BuiltinFunc<IdSlot, VarSlot>;
pub type TableFunc = source::TableFunc<IdSlot, VarSlot>;
pub type DefinedFunc = source::DefinedFunc<IdSlot, VarSlot>;

// - Definitions

pub type Def = source::Def<IdSlot, VarSlot>;
pub type DefKind = source::DefKind<IdSlot, VarSlot>;
pub type Spec = Vec<Def>;

// == Preparation traversal

// - Parameters

impl Prepare for source::ParamKind {
    type Output = ParamKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            source::ParamKind::Exp(typ_inner, exp_inner) => {
                ParamKind::Exp(typ_inner, exp_inner.prepare(layout))
            }
            source::ParamKind::Def(id_inner, tparams_inner, params_inner, typ_inner) => {
                ParamKind::Def(id_inner, tparams_inner, params_inner.prepare(layout), typ_inner)
            }
        }
    }
}

// - Instructions

impl Prepare for source::InstrKind {
    type Output = InstrKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            source::InstrKind::If(instr_inner) => InstrKind::If(instr_inner.prepare(layout)),
            source::InstrKind::Hold(instr_inner) => InstrKind::Hold(instr_inner.prepare(layout)),
            source::InstrKind::Case(instr_inner) => InstrKind::Case(instr_inner.prepare(layout)),
            source::InstrKind::Group(instr_inner) => InstrKind::Group(instr_inner.prepare(layout)),
            source::InstrKind::Let(instr_inner) => InstrKind::Let(instr_inner.prepare(layout)),
            source::InstrKind::Rule(instr_inner) => InstrKind::Rule(instr_inner.prepare(layout)),
            source::InstrKind::Result(instr_inner) => {
                InstrKind::Result(instr_inner.prepare(layout))
            }
            source::InstrKind::Return(instr_inner) => {
                InstrKind::Return(instr_inner.prepare(layout))
            }
            source::InstrKind::Debug(instr_inner) => InstrKind::Debug(instr_inner.prepare(layout)),
        }
    }
}

// - If instruction

impl Prepare for source::IfInstr {
    type Output = IfInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        IfInstr {
            exp: self.exp.prepare(layout),
            iter_exps: self.iter_exps.prepare(layout),
            block: self.block.prepare(layout),
            dangle: self.dangle,
        }
    }
}

// - Hold instruction

impl Prepare for source::HoldInstr {
    type Output = HoldInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        HoldInstr {
            id: self.id,
            not_exp: self.not_exp.prepare(layout),
            iter_exps: self.iter_exps.prepare(layout),
            hold_case: self.hold_case.prepare(layout),
        }
    }
}

// - Case instruction

impl Prepare for source::CaseInstr {
    type Output = CaseInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        CaseInstr {
            exp: self.exp.prepare(layout),
            cases: self.cases.prepare(layout),
            dangle: self.dangle,
        }
    }
}

// - Group instruction

impl Prepare for source::GroupInstr {
    type Output = GroupInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        GroupInstr {
            id: self.id,
            rel_signature: self.rel_signature,
            exps: self.exps.prepare(layout),
            block: self.block.prepare(layout),
        }
    }
}

// - Let instruction

impl Prepare for source::LetInstr {
    type Output = LetInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        LetInstr {
            exp_l: self.exp_l.prepare(layout),
            exp_r: self.exp_r.prepare(layout),
            iter_instrs: self.iter_instrs.prepare(layout),
            block: self.block.prepare(layout),
        }
    }
}

// - Rule instruction

impl Prepare for source::RuleInstr {
    type Output = RuleInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        RuleInstr {
            id: self.id,
            not_exp: self.not_exp.prepare(layout),
            input_hint: self.input_hint,
            iter_instrs: self.iter_instrs.prepare(layout),
            block: self.block.prepare(layout),
        }
    }
}

// - Result instruction

impl Prepare for source::ResultInstr {
    type Output = ResultInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        ResultInstr { rel_signature: self.rel_signature, exps: self.exps.prepare(layout) }
    }
}

// - Return instruction

impl Prepare for source::ReturnInstr {
    type Output = ReturnInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        ReturnInstr { exp: self.exp.prepare(layout) }
    }
}

// - Debug instruction

impl Prepare for source::DebugInstr {
    type Output = DebugInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        DebugInstr { exp: self.exp.prepare(layout), instr: self.instr.prepare(layout) }
    }
}

// - Holding conditions

impl Prepare for source::HoldCase {
    type Output = HoldCase;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            source::HoldCase::Both(block_hold, block_not_hold) => {
                HoldCase::Both(block_hold.prepare(layout), block_not_hold.prepare(layout))
            }
            source::HoldCase::Hold(block_inner, dangle_inner) => {
                HoldCase::Hold(block_inner.prepare(layout), dangle_inner)
            }
            source::HoldCase::NotHold(block_inner, dangle_inner) => {
                HoldCase::NotHold(block_inner.prepare(layout), dangle_inner)
            }
        }
    }
}

// - Case analysis

impl Prepare for source::Guard {
    type Output = Guard;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            source::Guard::Bool(value_inner) => Guard::Bool(value_inner),
            source::Guard::Cmp(op_inner, typ_op_inner, exp_inner) => {
                Guard::Cmp(op_inner, typ_op_inner, exp_inner.prepare(layout))
            }
            source::Guard::Sub(typ_inner, subcheck_inner) => Guard::Sub(typ_inner, subcheck_inner),
            source::Guard::Match(pattern_inner) => Guard::Match(pattern_inner),
            source::Guard::Mem(exp_inner) => Guard::Mem(exp_inner.prepare(layout)),
        }
    }
}

impl Prepare for source::Case {
    type Output = Case;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        Case { guard: self.guard.prepare(layout), block: self.block.prepare(layout) }
    }
}

// - Table rows

impl Prepare for source::TableRow {
    type Output = TableRow;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        TableRow {
            exps_input: self.exps_input.prepare(layout),
            exp: self.exp.prepare(layout),
            block: self.block.prepare(layout),
        }
    }
}

// == Relation definitions

// - Relation definition

impl Prepare for source::RelDef {
    type Output = RelDef;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            source::RelDef::Extern(rel_inner) => RelDef::Extern(rel_inner.prepare(layout)),
            source::RelDef::Defined(rel_inner) => RelDef::Defined(rel_inner.prepare(layout)),
        }
    }
}

// - External relation definition

impl Prepare for source::ExternRel {
    type Output = ExternRel;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        ExternRel {
            id: self.id,
            rel_signature: self.rel_signature,
            exps_input: self.exps_input.prepare(layout),
            hints: self.hints,
        }
    }
}

// - Defined relation definition

impl Prepare for source::DefinedRel {
    type Output = DefinedRel;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        DefinedRel {
            id: self.id,
            rel_signature: self.rel_signature,
            exps_input: self.exps_input.prepare(layout),
            block: self.block.prepare(layout),
            block_else: self.block_else.prepare(layout),
            hints: self.hints,
        }
    }
}

// == Meta-function definitions

// - Meta-function definition

impl Prepare for source::MetaFuncDef {
    type Output = MetaFuncDef;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            source::MetaFuncDef::Extern(func_inner) => {
                MetaFuncDef::Extern(func_inner.prepare(layout))
            }
            source::MetaFuncDef::Builtin(func_inner) => {
                MetaFuncDef::Builtin(func_inner.prepare(layout))
            }
            source::MetaFuncDef::Table(func_inner) => {
                MetaFuncDef::Table(func_inner.prepare(layout))
            }
            source::MetaFuncDef::Defined(func_inner) => {
                MetaFuncDef::Defined(func_inner.prepare(layout))
            }
        }
    }
}

// - External function definition

impl Prepare for source::ExternFunc {
    type Output = ExternFunc;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        ExternFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params.prepare(layout),
            typ: self.typ,
            hints: self.hints,
        }
    }
}

// - Builtin function definition

impl Prepare for source::BuiltinFunc {
    type Output = BuiltinFunc;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        BuiltinFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params.prepare(layout),
            typ: self.typ,
            hints: self.hints,
        }
    }
}

// - Table function definition

impl Prepare for source::TableFunc {
    type Output = TableFunc;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        TableFunc {
            id: self.id,
            params: self.params.prepare(layout),
            typ: self.typ,
            table_rows: self.table_rows.prepare(layout),
            hints: self.hints,
        }
    }
}

// - Defined function definition

impl Prepare for source::DefinedFunc {
    type Output = DefinedFunc;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        DefinedFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params.prepare(layout),
            typ: self.typ,
            block: self.block.prepare(layout),
            block_else: self.block_else.prepare(layout),
            hints: self.hints,
        }
    }
}

// == Definitions

impl Prepare for source::DefKind {
    type Output = DefKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            source::DefKind::Typ(typdef) => DefKind::Typ(typdef),
            source::DefKind::Var(def_var) => DefKind::Var(def_var),
            source::DefKind::Rel(rel_inner) => DefKind::Rel(rel_inner.prepare(layout)),
            source::DefKind::MetaFunc(func_inner) => DefKind::MetaFunc(func_inner.prepare(layout)),
        }
    }
}
