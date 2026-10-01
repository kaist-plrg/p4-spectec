//! Syntax equality for prose-language data
//!
//! Ignores source regions, inferred notes, and prose annotations,
//! so two PL trees are equal when they would run the same way.

use crate::lang::traits::eq::SyntaxEq;

use super::ast::*;

// == Syntax equality

// - Expressions

impl SyntaxEq for ExpKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ExpKind::Bool(value_l), ExpKind::Bool(value_r)) => value_l == value_r,
            (ExpKind::Num(value_l), ExpKind::Num(value_r)) => value_l == value_r,
            (ExpKind::Text(value_l), ExpKind::Text(value_r)) => value_l == value_r,
            (ExpKind::Id(id_l), ExpKind::Id(id_r)) => id_l.syntax_eq(id_r),
            (ExpKind::Un(op_l, typ_l, exp_l), ExpKind::Un(op_r, typ_r, exp_r)) => {
                op_l.syntax_eq(op_r) && typ_l.syntax_eq(typ_r) && exp_l.syntax_eq(exp_r)
            }
            (
                ExpKind::Bin(op_l, typ_l, exp_l_l, exp_r_l),
                ExpKind::Bin(op_r, typ_r, exp_l_r, exp_r_r),
            ) => {
                op_l.syntax_eq(op_r)
                    && typ_l.syntax_eq(typ_r)
                    && exp_l_l.syntax_eq(exp_l_r)
                    && exp_r_l.syntax_eq(exp_r_r)
            }
            (
                ExpKind::Cmp(op_l, typ_l, exp_l_l, exp_r_l),
                ExpKind::Cmp(op_r, typ_r, exp_l_r, exp_r_r),
            ) => {
                op_l.syntax_eq(op_r)
                    && typ_l.syntax_eq(typ_r)
                    && exp_l_l.syntax_eq(exp_l_r)
                    && exp_r_l.syntax_eq(exp_r_r)
            }
            (ExpKind::UpCast(typ_l, exp_l), ExpKind::UpCast(typ_r, exp_r))
            | (ExpKind::DownCast(typ_l, exp_l), ExpKind::DownCast(typ_r, exp_r)) => {
                typ_l.syntax_eq(typ_r) && exp_l.syntax_eq(exp_r)
            }
            (ExpKind::Sub(exp_l, typ_l, _), ExpKind::Sub(exp_r, typ_r, _)) => {
                exp_l.syntax_eq(exp_r) && typ_l.syntax_eq(typ_r)
            }
            (ExpKind::Match(exp_l, pattern_l), ExpKind::Match(exp_r, pattern_r)) => {
                exp_l.syntax_eq(exp_r) && pattern_l.syntax_eq(pattern_r)
            }
            (ExpKind::Tuple(exps_l), ExpKind::Tuple(exps_r))
            | (ExpKind::List(exps_l), ExpKind::List(exps_r)) => exps_l.syntax_eq(exps_r),
            (ExpKind::Case(not_exp_l), ExpKind::Case(not_exp_r)) => not_exp_l.syntax_eq(not_exp_r),
            (ExpKind::Str(fields_l), ExpKind::Str(fields_r)) => {
                fields_l.len() == fields_r.len()
                    && fields_l
                        .iter()
                        .zip(fields_r)
                        .all(|((atom_l, exp_l), (atom_r, exp_r))| {
                            atom_l.syntax_eq(atom_r) && exp_l.syntax_eq(exp_r)
                        })
            }
            (ExpKind::Opt(Some(exp_l)), ExpKind::Opt(Some(exp_r))) => exp_l.syntax_eq(exp_r),
            (ExpKind::Opt(None), ExpKind::Opt(None)) => true,
            (ExpKind::Cons(exp_l_l, exp_r_l), ExpKind::Cons(exp_l_r, exp_r_r))
            | (ExpKind::Cat(exp_l_l, exp_r_l), ExpKind::Cat(exp_l_r, exp_r_r))
            | (ExpKind::Mem(exp_l_l, exp_r_l), ExpKind::Mem(exp_l_r, exp_r_r)) => {
                exp_l_l.syntax_eq(exp_l_r) && exp_r_l.syntax_eq(exp_r_r)
            }
            (ExpKind::Len(exp_l), ExpKind::Len(exp_r)) => exp_l.syntax_eq(exp_r),
            (ExpKind::Dot(exp_l, atom_l), ExpKind::Dot(exp_r, atom_r)) => {
                exp_l.syntax_eq(exp_r) && atom_l.syntax_eq(atom_r)
            }
            (ExpKind::Idx(exp_base_l, exp_idx_l), ExpKind::Idx(exp_base_r, exp_idx_r)) => {
                exp_base_l.syntax_eq(exp_base_r) && exp_idx_l.syntax_eq(exp_idx_r)
            }
            (
                ExpKind::Slice(exp_base_l, exp_idx_l, exp_len_l),
                ExpKind::Slice(exp_base_r, exp_idx_r, exp_len_r),
            ) => {
                exp_base_l.syntax_eq(exp_base_r)
                    && exp_idx_l.syntax_eq(exp_idx_r)
                    && exp_len_l.syntax_eq(exp_len_r)
            }
            (
                ExpKind::Upd(exp_base_l, path_l, exp_field_l),
                ExpKind::Upd(exp_base_r, path_r, exp_field_r),
            ) => {
                exp_base_l.syntax_eq(exp_base_r)
                    && path_l.syntax_eq(path_r)
                    && exp_field_l.syntax_eq(exp_field_r)
            }
            (ExpKind::Call(id_l, targs_l, args_l), ExpKind::Call(id_r, targs_r, args_r)) => {
                id_l.syntax_eq(id_r) && targs_l.syntax_eq(targs_r) && args_l.syntax_eq(args_r)
            }
            (ExpKind::Iter(exp_l, iter_exp_l), ExpKind::Iter(exp_r, iter_exp_r)) => {
                exp_l.syntax_eq(exp_r) && iter_exp_l.syntax_eq(iter_exp_r)
            }
            _ => false,
        }
    }
}

// - Paths

impl SyntaxEq for PathKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PathKind::Root, PathKind::Root) => true,
            (PathKind::Idx(path_l, exp_l), PathKind::Idx(path_r, exp_r)) => {
                path_l.syntax_eq(path_r) && exp_l.syntax_eq(exp_r)
            }
            (
                PathKind::Slice(path_l, exp_idx_l, exp_len_l),
                PathKind::Slice(path_r, exp_idx_r, exp_len_r),
            ) => {
                path_l.syntax_eq(path_r)
                    && exp_idx_l.syntax_eq(exp_idx_r)
                    && exp_len_l.syntax_eq(exp_len_r)
            }
            (PathKind::Dot(path_l, atom_l), PathKind::Dot(path_r, atom_r)) => {
                path_l.syntax_eq(path_r) && atom_l.syntax_eq(atom_r)
            }
            _ => false,
        }
    }
}

// - Parameters

impl SyntaxEq for ParamKind {
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

// - Arguments

impl SyntaxEq for ArgKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ArgKind::Exp(exp_l), ArgKind::Exp(exp_r)) => exp_l.syntax_eq(exp_r),
            (ArgKind::Def(id_l), ArgKind::Def(id_r)) => id_l.syntax_eq(id_r),
            _ => false,
        }
    }
}

// - Instructions

impl<Tier> SyntaxEq for InstrKind<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (InstrKind::If(instr_l), InstrKind::If(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Hold(instr_l), InstrKind::Hold(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Case(instr_l), InstrKind::Case(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Let(instr_l), InstrKind::Let(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Debug(instr_l), InstrKind::Debug(instr_r)) => instr_l.syntax_eq(instr_r),
            (InstrKind::Destruct(instr_l), InstrKind::Destruct(instr_r)) => {
                instr_l.syntax_eq(instr_r)
            }
            (InstrKind::CheckLetSub(instr_l), InstrKind::CheckLetSub(instr_r)) => {
                instr_l.syntax_eq(instr_r)
            }
            (InstrKind::CheckLetMatch(instr_l), InstrKind::CheckLetMatch(instr_r)) => {
                instr_l.syntax_eq(instr_r)
            }
            (InstrKind::OptionGet(instr_l), InstrKind::OptionGet(instr_r)) => {
                instr_l.syntax_eq(instr_r)
            }
            (InstrKind::Tier(instr_l), InstrKind::Tier(instr_r)) => instr_l.syntax_eq(instr_r),
            _ => false,
        }
    }
}

impl<Tier> SyntaxEq for IfInstr<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
            && self.iter_exps.syntax_eq(&other.iter_exps)
            && self.block.syntax_eq(&other.block)
            && self.dangle == other.dangle
    }
}

impl<Tier> SyntaxEq for HoldInstr<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_exp.syntax_eq(&other.not_exp)
            && self.iter_exps.syntax_eq(&other.iter_exps)
            && self.hold_case.syntax_eq(&other.hold_case)
    }
}

impl<Tier> SyntaxEq for CaseInstr<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
            && self.cases.syntax_eq(&other.cases)
            && self.dangle == other.dangle
    }
}

impl SyntaxEq for LetInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp_l.syntax_eq(&other.exp_l)
            && self.exp_r.syntax_eq(&other.exp_r)
            && self.iter_instrs.syntax_eq(&other.iter_instrs)
    }
}

impl SyntaxEq for DebugInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
    }
}

impl SyntaxEq for DestructInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.bindings.len() == other.bindings.len()
            && self.bindings.iter().zip(&other.bindings).all(
                |((name_l, exp_l), (name_r, exp_r))| name_l == name_r && exp_l.syntax_eq(exp_r),
            )
            && self.exp.syntax_eq(&other.exp)
    }
}

impl<Tier> SyntaxEq for CheckLetSubInstr<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        self.typ.syntax_eq(&other.typ)
            && self.exp_l.syntax_eq(&other.exp_l)
            && self.exp_r.syntax_eq(&other.exp_r)
            && self.block.syntax_eq(&other.block)
    }
}

impl<Tier> SyntaxEq for CheckLetMatchInstr<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        self.pattern.syntax_eq(&other.pattern)
            && self.exp_l.syntax_eq(&other.exp_l)
            && self.exp_r.syntax_eq(&other.exp_r)
            && self.block.syntax_eq(&other.block)
    }
}

impl<Tier> SyntaxEq for OptionGetInstr<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp_l.syntax_eq(&other.exp_l)
            && self.exp_r.syntax_eq(&other.exp_r)
            && self.block.syntax_eq(&other.block)
    }
}

impl<Tier> SyntaxEq for TierInstr<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        self.tier.syntax_eq(&other.tier)
    }
}

// - Group-body tier

impl SyntaxEq for GroupInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (GroupInstr::Result(instr_l), GroupInstr::Result(instr_r)) => {
                instr_l.syntax_eq(instr_r)
            }
            (GroupInstr::Return(instr_l), GroupInstr::Return(instr_r)) => {
                instr_l.syntax_eq(instr_r)
            }
            (GroupInstr::Rule(instr_l), GroupInstr::Rule(instr_r)) => instr_l.syntax_eq(instr_r),
            (GroupInstr::Backtrack(instr_l), GroupInstr::Backtrack(instr_r)) => {
                instr_l.syntax_eq(instr_r)
            }
            _ => false,
        }
    }
}

impl SyntaxEq for ResultInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.rel_signature.syntax_eq(&other.rel_signature)
            && self.exps_output.syntax_eq(&other.exps_output)
    }
}

impl SyntaxEq for ReturnInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
    }
}

impl SyntaxEq for RuleInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_exp.syntax_eq(&other.not_exp)
            && self.input_hint == other.input_hint
            && self.iter_instrs.syntax_eq(&other.iter_instrs)
    }
}

impl SyntaxEq for BacktrackInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.blocks.syntax_eq(&other.blocks)
    }
}

// - Dispatch tier

impl SyntaxEq for DispatchInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DispatchInstr::Group(instr_l), DispatchInstr::Group(instr_r)) => {
                instr_l.syntax_eq(instr_r)
            }
            (DispatchInstr::Route(instr_l), DispatchInstr::Route(instr_r)) => {
                instr_l.syntax_eq(instr_r)
            }
            _ => false,
        }
    }
}

impl SyntaxEq for RuleGroupInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id_rel.syntax_eq(&other.id_rel)
            && self.id_group.syntax_eq(&other.id_group)
            && self.rel_signature.syntax_eq(&other.rel_signature)
            && self.exps_input.syntax_eq(&other.exps_input)
            && self.block.syntax_eq(&other.block)
    }
}

impl SyntaxEq for RouteInstr {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.blocks.syntax_eq(&other.blocks)
    }
}

// - Holding conditions

impl<Tier> SyntaxEq for HoldCase<Tier>
where
    Tier: SyntaxEq,
{
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

impl SyntaxEq for Guard {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Guard::Bool(value_l), Guard::Bool(value_r)) => value_l == value_r,
            (Guard::Cmp(op_l, typ_l, exp_l), Guard::Cmp(op_r, typ_r, exp_r)) => {
                op_l.syntax_eq(op_r) && typ_l.syntax_eq(typ_r) && exp_l.syntax_eq(exp_r)
            }
            (Guard::Sub(typ_l, _), Guard::Sub(typ_r, _)) => typ_l.syntax_eq(typ_r),
            (Guard::Match(pattern_l), Guard::Match(pattern_r)) => pattern_l.syntax_eq(pattern_r),
            (Guard::Mem(exp_l), Guard::Mem(exp_r)) => exp_l.syntax_eq(exp_r),
            (Guard::CheckLetSub(typ_l, _, exp_l), Guard::CheckLetSub(typ_r, _, exp_r)) => {
                typ_l.syntax_eq(typ_r) && exp_l.syntax_eq(exp_r)
            }
            (Guard::CheckLetMatch(pattern_l, exp_l), Guard::CheckLetMatch(pattern_r, exp_r)) => {
                pattern_l.syntax_eq(pattern_r) && exp_l.syntax_eq(exp_r)
            }
            _ => false,
        }
    }
}

impl<Tier> SyntaxEq for Case<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        self.guard.syntax_eq(&other.guard) && self.block.syntax_eq(&other.block)
    }
}

// - Blocks

impl<Tier> SyntaxEq for Block<Tier>
where
    Tier: SyntaxEq,
{
    fn syntax_eq(&self, other: &Self) -> bool {
        self.as_slice().syntax_eq(other.as_slice())
    }
}

// - Table rows

impl SyntaxEq for TableRow {
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
        self.id.syntax_eq(&other.id)
    }
}

impl SyntaxEq for DefinedTyp {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.def_typ.syntax_eq(&other.def_typ)
    }
}

// == Meta-variable definitions

impl SyntaxEq for VarDef {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id) && self.typ.syntax_eq(&other.typ)
    }
}

// == Relation definitions

impl SyntaxEq for RelDef {
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

impl SyntaxEq for ExternRel {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.rel_signature.syntax_eq(&other.rel_signature)
            && self.exps_input.syntax_eq(&other.exps_input)
    }
}

impl SyntaxEq for DefinedRel {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.rel_signature.syntax_eq(&other.rel_signature)
            && self.exps_input.syntax_eq(&other.exps_input)
            && self.block.syntax_eq(&other.block)
            && match (&self.block_else_opt, &other.block_else_opt) {
                (Some(block_l), Some(block_r)) => block_l.syntax_eq(block_r),
                (None, None) => true,
                _ => false,
            }
    }
}

// == Meta-function definitions

impl SyntaxEq for MetaFuncDef {
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

impl SyntaxEq for ExternFunc {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
    }
}

impl SyntaxEq for BuiltinFunc {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
    }
}

impl SyntaxEq for TableFunc {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
            && self.rows.syntax_eq(&other.rows)
    }
}

impl SyntaxEq for DefinedFunc {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
            && self.block.syntax_eq(&other.block)
            && match (&self.block_else_opt, &other.block_else_opt) {
                (Some(block_l), Some(block_r)) => block_l.syntax_eq(block_r),
                (None, None) => true,
                _ => false,
            }
    }
}

// == Definitions

impl SyntaxEq for DefKind {
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

impl SyntaxEq for Spec {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.as_slice().syntax_eq(other.as_slice())
    }
}
