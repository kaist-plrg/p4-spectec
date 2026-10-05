//! Free identifiers in prose-language data
//!
//! Only expressions contribute identifiers;
//! instructions of both tiers collect from their expressions and blocks,
//! definitions from their parameters, inputs, and blocks.

use crate::lang::{common::ds::set::IdSet, traits::free::FreeIds};

use super::ast::*;

// == Free identifiers

// - Expressions

impl FreeIds for ExpKind {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Bool(_) | Self::Num(_) | Self::Text(_) => IdSet::new(),
            Self::Id(id) => IdSet::from([id.clone()]),
            Self::Un(_, _, exp)
            | Self::UpCast(_, exp)
            | Self::DownCast(_, exp)
            | Self::Sub(exp, _, _)
            | Self::Match(exp, _)
            | Self::Len(exp)
            | Self::Dot(exp, _)
            | Self::Iter(exp, _) => exp.free_ids(),
            Self::Bin(_, _, exp_l, exp_r)
            | Self::Cmp(_, _, exp_l, exp_r)
            | Self::Cons(exp_l, exp_r)
            | Self::Cat(exp_l, exp_r)
            | Self::Mem(exp_l, exp_r)
            | Self::Idx(exp_l, exp_r) => exp_l.free_ids().union(exp_r.free_ids()),
            Self::Tuple(exps) | Self::List(exps) => exps.as_slice().free_ids(),
            Self::Case(not_exp) => not_exp.free_ids(),
            Self::Str(fields) => fields
                .iter()
                .fold(IdSet::new(), |free, (_, exp)| free.union(exp.free_ids())),
            Self::Opt(exp) => exp.free_ids(),
            Self::Slice(exp_base, exp_idx, exp_len) => exp_base
                .free_ids()
                .union(exp_idx.free_ids())
                .union(exp_len.free_ids()),
            Self::Upd(exp_base, path, exp_field) => exp_base
                .free_ids()
                .union(path.free_ids())
                .union(exp_field.free_ids()),
            Self::Call(_, _, args) => args.as_slice().free_ids(),
        }
    }
}

// - Paths

impl FreeIds for PathKind {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Root => IdSet::new(),
            Self::Idx(path, exp_idx) => path.free_ids().union(exp_idx.free_ids()),
            Self::Slice(path, exp_idx, exp_len) => path
                .free_ids()
                .union(exp_idx.free_ids())
                .union(exp_len.free_ids()),
            Self::Dot(path, _) => path.free_ids(),
        }
    }
}

// - Parameters

impl FreeIds for ParamKind {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Exp(_, exp) => exp.free_ids(),
            Self::Def(..) => IdSet::new(),
        }
    }
}

// - Arguments

impl FreeIds for ArgKind {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Exp(exp) => exp.free_ids(),
            Self::Def(_) => IdSet::new(),
        }
    }
}

// - Instructions

impl<Tier: FreeIds> FreeIds for InstrKind<Tier> {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::If(instr) => instr.free_ids(),
            Self::Hold(instr) => instr.free_ids(),
            Self::Case(instr) => instr.free_ids(),
            Self::Let(instr) => instr.free_ids(),
            Self::Debug(instr) => instr.free_ids(),
            Self::Destruct(instr) => instr.free_ids(),
            Self::CheckLetSub(instr) => instr.free_ids(),
            Self::CheckLetMatch(instr) => instr.free_ids(),
            Self::OptionGet(instr) => instr.free_ids(),
            Self::Tier(instr) => instr.free_ids(),
        }
    }
}

impl<Tier: FreeIds> FreeIds for IfInstr<Tier> {
    fn free_ids(&self) -> IdSet {
        self.exp.free_ids().union(self.block.free_ids())
    }
}

impl<Tier: FreeIds> FreeIds for HoldInstr<Tier> {
    fn free_ids(&self) -> IdSet {
        self.not_exp.free_ids()
    }
}

impl<Tier: FreeIds> FreeIds for CaseInstr<Tier> {
    fn free_ids(&self) -> IdSet {
        self.exp.free_ids().union(self.cases.as_slice().free_ids())
    }
}

impl FreeIds for LetInstr {
    fn free_ids(&self) -> IdSet {
        self.exp_l.free_ids().union(self.exp_r.free_ids())
    }
}

impl FreeIds for DebugInstr {
    fn free_ids(&self) -> IdSet {
        self.exp.free_ids()
    }
}

impl FreeIds for DestructInstr {
    fn free_ids(&self) -> IdSet {
        self.bindings
            .iter()
            .fold(IdSet::new(), |free, (_, exp)| free.union(exp.free_ids()))
            .union(self.exp.free_ids())
    }
}

impl<Tier: FreeIds> FreeIds for CheckLetSubInstr<Tier> {
    fn free_ids(&self) -> IdSet {
        self.exp_l
            .free_ids()
            .union(self.exp_r.free_ids())
            .union(self.block.free_ids())
    }
}

impl<Tier: FreeIds> FreeIds for CheckLetMatchInstr<Tier> {
    fn free_ids(&self) -> IdSet {
        self.exp_l
            .free_ids()
            .union(self.exp_r.free_ids())
            .union(self.block.free_ids())
    }
}

impl<Tier: FreeIds> FreeIds for OptionGetInstr<Tier> {
    fn free_ids(&self) -> IdSet {
        self.exp_l
            .free_ids()
            .union(self.exp_r.free_ids())
            .union(self.block.free_ids())
    }
}

impl<Tier: FreeIds> FreeIds for TierInstr<Tier> {
    fn free_ids(&self) -> IdSet {
        self.tier.free_ids()
    }
}

// - Group-body tier

impl FreeIds for GroupInstr {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Result(instr) => instr.free_ids(),
            Self::Return(instr) => instr.free_ids(),
            Self::Rule(instr) => instr.free_ids(),
            Self::Backtrack(instr) => instr.free_ids(),
        }
    }
}

impl FreeIds for ResultInstr {
    fn free_ids(&self) -> IdSet {
        self.exps_output.as_slice().free_ids()
    }
}

impl FreeIds for ReturnInstr {
    fn free_ids(&self) -> IdSet {
        self.exp.free_ids()
    }
}

impl FreeIds for RuleInstr {
    fn free_ids(&self) -> IdSet {
        self.not_exp.free_ids()
    }
}

impl FreeIds for BacktrackInstr {
    fn free_ids(&self) -> IdSet {
        self.blocks.as_slice().free_ids()
    }
}

// - Dispatch tier

impl FreeIds for DispatchInstr {
    fn free_ids(&self) -> IdSet {
        match self {
            Self::Group(instr) => instr.free_ids(),
            Self::Route(instr) => instr.free_ids(),
        }
    }
}

impl FreeIds for RuleGroupInstr {
    fn free_ids(&self) -> IdSet {
        self.exps_input
            .as_slice()
            .free_ids()
            .union(self.block.free_ids())
    }
}

impl FreeIds for RouteInstr {
    fn free_ids(&self) -> IdSet {
        self.blocks.as_slice().free_ids()
    }
}

// - Holding conditions

impl<Tier: FreeIds> FreeIds for HoldCase<Tier> {
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
            Self::CheckLetSub(_, _, exp) | Self::CheckLetMatch(_, exp) => exp.free_ids(),
            Self::Bool(_) | Self::Sub(..) | Self::Match(_) => IdSet::new(),
        }
    }
}

impl<Tier: FreeIds> FreeIds for Case<Tier> {
    fn free_ids(&self) -> IdSet {
        self.guard.free_ids().union(self.block.free_ids())
    }
}

// - Blocks

impl<Tier: FreeIds> FreeIds for Block<Tier> {
    fn free_ids(&self) -> IdSet {
        self.as_slice().free_ids()
    }
}

impl FreeIds for Fallthrough {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
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
            .union(self.block_else_opt.free_ids())
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
            .union(self.rows.as_slice().free_ids())
    }
}

impl FreeIds for DefinedFunc {
    fn free_ids(&self) -> IdSet {
        self.params
            .as_slice()
            .free_ids()
            .union(self.block.free_ids())
            .union(self.block_else_opt.free_ids())
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
