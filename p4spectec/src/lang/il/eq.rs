//! Syntax equality for internal-language data
//!
//! Ignores source regions while comparing node contents.
//! Type notes on expressions are ignored too, as are subtype checks,
//! which are derived from the types they annotate.

use crate::lang::traits::eq::SyntaxEq;

use super::ast::*;

// == Syntax equality

// - Types

impl SyntaxEq for TypKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypKind::Bool, TypKind::Bool) | (TypKind::Text, TypKind::Text) => true,
            (TypKind::Num(num_typ_l), TypKind::Num(num_typ_r)) => num_typ_l == num_typ_r,
            (TypKind::Var(id_l, targs_l), TypKind::Var(id_r, targs_r)) => {
                id_l.syntax_eq(id_r) && targs_l.syntax_eq(targs_r)
            }
            (TypKind::Tuple(typs_l), TypKind::Tuple(typs_r)) => typs_l.syntax_eq(typs_r),
            (TypKind::Iter(typ_l, iter_l), TypKind::Iter(typ_r, iter_r)) => {
                typ_l.syntax_eq(typ_r) && iter_l == iter_r
            }
            (TypKind::Func(func_typ_l), TypKind::Func(func_typ_r)) => {
                func_typ_l.syntax_eq(func_typ_r)
            }
            _ => false,
        }
    }
}

impl SyntaxEq for FuncTyp {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.tparams.syntax_eq(&other.tparams)
            && self.typs_params.syntax_eq(&other.typs_params)
            && self.typ_ret.syntax_eq(&other.typ_ret)
    }
}

// - Defined types

impl SyntaxEq for DefTypKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DefTypKind::Plain(typ_l), DefTypKind::Plain(typ_r)) => typ_l.syntax_eq(typ_r),
            (DefTypKind::Struct(fields_l), DefTypKind::Struct(fields_r)) => {
                fields_l.syntax_eq(fields_r)
            }
            (DefTypKind::Variant(cases_l), DefTypKind::Variant(cases_r)) => {
                cases_l.syntax_eq(cases_r)
            }
            _ => false,
        }
    }
}

impl SyntaxEq for TypField {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.atom.syntax_eq(&other.atom) && self.typ.syntax_eq(&other.typ)
    }
}

impl SyntaxEq for TypOriginKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id) && self.targs.syntax_eq(&other.targs)
    }
}

impl SyntaxEq for TypCase {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.not_typ.syntax_eq(&other.not_typ)
            && self.typ_origin.syntax_eq(&other.typ_origin)
            && self.hints.syntax_eq(&other.hints)
    }
}

// - Subtype checks

impl SyntaxEq for Subcheck {
    fn syntax_eq(&self, _other: &Self) -> bool {
        true
    }

    fn slice_syntax_eq(_subchecks_l: &[Self], _subchecks_r: &[Self]) -> bool {
        true
    }
}

// - Operator types

impl SyntaxEq for OpTyp {
    fn syntax_eq(&self, other: &Self) -> bool {
        self == other
    }
}

// - Expressions

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for ExpKind<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ExpKind::Bool(value_l), ExpKind::Bool(value_r)) => value_l == value_r,
            (ExpKind::Num(value_l), ExpKind::Num(value_r)) => value_l == value_r,
            (ExpKind::Text(value_l), ExpKind::Text(value_r)) => value_l == value_r,
            (ExpKind::Id(id_l), ExpKind::Id(id_r)) => id_l.syntax_eq(id_r),
            (ExpKind::Un(op_l, typ_l, exp_l), ExpKind::Un(op_r, typ_r, exp_r)) => {
                op_l == op_r && typ_l == typ_r && exp_l.syntax_eq(exp_r)
            }
            (
                ExpKind::Bin(op_l, typ_l, exp_l_l, exp_r_l),
                ExpKind::Bin(op_r, typ_r, exp_l_r, exp_r_r),
            ) => {
                op_l == op_r
                    && typ_l == typ_r
                    && exp_l_l.syntax_eq(exp_l_r)
                    && exp_r_l.syntax_eq(exp_r_r)
            }
            (
                ExpKind::Cmp(op_l, typ_l, exp_l_l, exp_r_l),
                ExpKind::Cmp(op_r, typ_r, exp_l_r, exp_r_r),
            ) => {
                op_l == op_r
                    && typ_l == typ_r
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
            (ExpKind::Case(not_exp_l), ExpKind::Case(not_exp_r)) => {
                not_exp_l.eq_by(not_exp_r, SyntaxEq::syntax_eq)
            }
            (ExpKind::Str(fields_l), ExpKind::Str(fields_r)) => fields_l.syntax_eq(fields_r),
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
            (ExpKind::Iter(exp_l, exp_iter_l), ExpKind::Iter(exp_r, exp_iter_r)) => {
                exp_l.syntax_eq(exp_r) && exp_iter_l.syntax_eq(exp_iter_r)
            }
            _ => false,
        }
    }
}

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for ExpField<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.atom.syntax_eq(&other.atom) && self.exp.syntax_eq(&other.exp)
    }
}

impl<V: SyntaxEq> SyntaxEq for ExpIter<V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.iter == other.iter && self.vars.syntax_eq(&other.vars)
    }
}

// - Patterns

impl SyntaxEq for Pattern {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Pattern::Case(mixop_l), Pattern::Case(mixop_r)) => mixop_l == mixop_r,
            (Pattern::List(pattern_l), Pattern::List(pattern_r)) => pattern_l == pattern_r,
            (Pattern::Opt(pattern_l), Pattern::Opt(pattern_r)) => pattern_l == pattern_r,
            _ => false,
        }
    }
}

impl SyntaxEq for ListPattern {
    fn syntax_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SyntaxEq for OptPattern {
    fn syntax_eq(&self, other: &Self) -> bool {
        self == other
    }
}

// - Paths

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for PathKind<I, V> {
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
            (ParamKind::Exp(typ_l), ParamKind::Exp(typ_r)) => typ_l.syntax_eq(typ_r),
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

impl<I: SyntaxEq, V: SyntaxEq> SyntaxEq for ArgKind<I, V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ArgKind::Exp(exp_l), ArgKind::Exp(exp_r)) => exp_l.syntax_eq(exp_r),
            (ArgKind::Def(id_l), ArgKind::Def(id_r)) => id_l.syntax_eq(id_r),
            _ => false,
        }
    }
}

// - Premises

impl SyntaxEq for PremKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PremKind::Rule(prem_l), PremKind::Rule(prem_r)) => prem_l.syntax_eq(prem_r),
            (PremKind::If(prem_l), PremKind::If(prem_r)) => prem_l.syntax_eq(prem_r),
            (PremKind::IfHold(prem_l), PremKind::IfHold(prem_r)) => prem_l.syntax_eq(prem_r),
            (PremKind::IfNotHold(prem_l), PremKind::IfNotHold(prem_r)) => prem_l.syntax_eq(prem_r),
            (PremKind::Iter(prem_l), PremKind::Iter(prem_r)) => prem_l.syntax_eq(prem_r),
            (PremKind::Debug(prem_l), PremKind::Debug(prem_r)) => prem_l.syntax_eq(prem_r),
            _ => false,
        }
    }
}

impl SyntaxEq for RulePrem {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_exp.syntax_eq(&other.not_exp)
            && self.input_hint == other.input_hint
    }
}

impl SyntaxEq for IfPrem {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
    }
}

impl SyntaxEq for IfHoldPrem {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id) && self.not_exp.syntax_eq(&other.not_exp)
    }
}

impl SyntaxEq for IfNotHoldPrem {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id) && self.not_exp.syntax_eq(&other.not_exp)
    }
}

impl SyntaxEq for IterPrem {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.prem.syntax_eq(&other.prem) && self.prem_iter.syntax_eq(&other.prem_iter)
    }
}

impl SyntaxEq for DebugPrem {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.exp.syntax_eq(&other.exp)
    }
}

impl<V: SyntaxEq> SyntaxEq for PremIter<V> {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.iter.syntax_eq(&other.iter)
            && self.vars_bound.syntax_eq(&other.vars_bound)
            && self.vars_bind.syntax_eq(&other.vars_bind)
    }
}

// - Rules

impl SyntaxEq for RuleKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_exp.syntax_eq(&other.not_exp)
            && self.prems.syntax_eq(&other.prems)
    }
}

impl SyntaxEq for RuleGroupKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id) && self.rules.syntax_eq(&other.rules)
    }
}

impl SyntaxEq for ElseGroupKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id) && self.rule.syntax_eq(&other.rule)
    }
}

// - Clauses

impl SyntaxEq for ClauseKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.args.syntax_eq(&other.args)
            && self.exp.syntax_eq(&other.exp)
            && self.prems.syntax_eq(&other.prems)
    }
}

// - Table rows

impl SyntaxEq for TableRowKind {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.args.syntax_eq(&other.args) && self.exp.syntax_eq(&other.exp)
    }
}

// == Type definitions

impl SyntaxEq for TypDef {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypDef::Extern(extern_typ_l), TypDef::Extern(extern_typ_r)) => {
                extern_typ_l.syntax_eq(extern_typ_r)
            }
            (TypDef::Defined(defined_typ_l), TypDef::Defined(defined_typ_r)) => {
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

impl SyntaxEq for RelDef {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RelDef::Extern(extern_rel_l), RelDef::Extern(extern_rel_r)) => {
                extern_rel_l.syntax_eq(extern_rel_r)
            }
            (RelDef::Defined(defined_rel_l), RelDef::Defined(defined_rel_r)) => {
                defined_rel_l.syntax_eq(defined_rel_r)
            }
            _ => false,
        }
    }
}

impl SyntaxEq for ExternRel {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_typ.syntax_eq(&other.not_typ)
            && self.input_hint == other.input_hint
            && self.hints.syntax_eq(&other.hints)
    }
}

impl SyntaxEq for DefinedRel {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.not_typ.syntax_eq(&other.not_typ)
            && self.input_hint == other.input_hint
            && self.rule_groups.syntax_eq(&other.rule_groups)
            && match (&self.else_group, &other.else_group) {
                (Some(group_l), Some(group_r)) => group_l.syntax_eq(group_r),
                (None, None) => true,
                _ => false,
            }
            && self.hints.syntax_eq(&other.hints)
    }
}

// == Meta-function definitions

impl SyntaxEq for MetaFuncDef {
    fn syntax_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MetaFuncDef::Extern(extern_func_l), MetaFuncDef::Extern(extern_func_r)) => {
                extern_func_l.syntax_eq(extern_func_r)
            }
            (MetaFuncDef::Builtin(builtin_func_l), MetaFuncDef::Builtin(builtin_func_r)) => {
                builtin_func_l.syntax_eq(builtin_func_r)
            }
            (MetaFuncDef::Table(table_func_l), MetaFuncDef::Table(table_func_r)) => {
                table_func_l.syntax_eq(table_func_r)
            }
            (MetaFuncDef::Defined(defined_func_l), MetaFuncDef::Defined(defined_func_r)) => {
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
            && self.hints.syntax_eq(&other.hints)
    }
}

impl SyntaxEq for BuiltinFunc {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
            && self.hints.syntax_eq(&other.hints)
    }
}

impl SyntaxEq for TableFunc {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
            && self.rows.syntax_eq(&other.rows)
            && self.hints.syntax_eq(&other.hints)
    }
}

impl SyntaxEq for DefinedFunc {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id)
            && self.tparams.syntax_eq(&other.tparams)
            && self.params.syntax_eq(&other.params)
            && self.typ.syntax_eq(&other.typ)
            && self.clauses.syntax_eq(&other.clauses)
            && match (&self.else_clause, &other.else_clause) {
                (Some(clause_l), Some(clause_r)) => clause_l.syntax_eq(clause_r),
                (None, None) => true,
                _ => false,
            }
            && self.hints.syntax_eq(&other.hints)
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
