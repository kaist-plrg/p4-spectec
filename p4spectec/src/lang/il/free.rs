//! Free identifiers in internal-language data
//!
//! Only expressions contribute identifiers;
//! types, patterns, iteration binders, and signatures have none.
//! Rules, clauses, and table rows collect from their expressions and premises.

use crate::lang::{
    common::ds::set::IdSet,
    traits::{
        eq::SyntaxEq,
        free::{FreeIds, FreeVars},
    },
};

use super::ast::*;

// == Free identifiers

// - Types

impl FreeIds for TypKind {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Defined types

impl FreeIds for DefTypKind {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for TypField {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for TypOriginKind {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for TypCase {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Subtype checks

impl FreeIds for Subcheck {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Operator types

impl FreeIds for OpTyp {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Expressions

impl FreeIds for ExpKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Bool(_) | Self::Num(_) | Self::Text(_) => {}
            Self::Id(id) => {
                free.insert(id.clone());
            }
            Self::Un(_, _, exp)
            | Self::UpCast(_, exp)
            | Self::DownCast(_, exp)
            | Self::Sub(exp, _, _)
            | Self::Match(exp, _)
            | Self::Len(exp)
            | Self::Dot(exp, _)
            | Self::Iter(exp, _) => exp.free_ids_into(free),
            Self::Bin(_, _, exp_l, exp_r)
            | Self::Cmp(_, _, exp_l, exp_r)
            | Self::Cons(exp_l, exp_r)
            | Self::Cat(exp_l, exp_r)
            | Self::Mem(exp_l, exp_r)
            | Self::Idx(exp_l, exp_r) => {
                exp_l.free_ids_into(free);
                exp_r.free_ids_into(free);
            }
            Self::Tuple(exps) | Self::List(exps) => exps.as_slice().free_ids_into(free),
            Self::Case(not_exp) => not_exp.free_ids_into(free),
            Self::Str(fields) => {
                for ExpField { exp, .. } in fields {
                    exp.free_ids_into(free);
                }
            }
            Self::Opt(exp) => exp.free_ids_into(free),
            Self::Slice(exp_base, exp_idx, exp_len) => {
                exp_base.free_ids_into(free);
                exp_idx.free_ids_into(free);
                exp_len.free_ids_into(free);
            }
            Self::Upd(exp_base, path, exp_field) => {
                exp_base.free_ids_into(free);
                path.free_ids_into(free);
                exp_field.free_ids_into(free);
            }
            Self::Call(_, _, args) => args.as_slice().free_ids_into(free),
        }
    }
}

impl FreeVars for Exp {
    fn free_vars(&self) -> Vec<Var> {
        match &self.node {
            ExpKind::Bool(_) | ExpKind::Num(_) | ExpKind::Text(_) => Vec::new(),
            ExpKind::Id(id) => vec![Var {
                id: id.clone(),
                typ: crate::phrase! {
                    node: self.note.as_ref().clone(),
                    span: self.span.clone(),
                },
                iters: Vec::new(),
            }],
            ExpKind::Un(_, _, exp_inner)
            | ExpKind::UpCast(_, exp_inner)
            | ExpKind::DownCast(_, exp_inner)
            | ExpKind::Sub(exp_inner, _, _)
            | ExpKind::Match(exp_inner, _)
            | ExpKind::Len(exp_inner)
            | ExpKind::Dot(exp_inner, _) => exp_inner.free_vars(),
            ExpKind::Bin(_, _, exp_l, exp_r)
            | ExpKind::Cmp(_, _, exp_l, exp_r)
            | ExpKind::Cons(exp_l, exp_r)
            | ExpKind::Cat(exp_l, exp_r)
            | ExpKind::Mem(exp_l, exp_r)
            | ExpKind::Idx(exp_l, exp_r) => {
                let mut vars_free = exp_l.free_vars();
                exp_r.free_vars_into(&mut vars_free);
                vars_free
            }
            ExpKind::Tuple(exps) | ExpKind::List(exps) => {
                let mut vars_free = Vec::new();
                for exp in exps {
                    exp.free_vars_into(&mut vars_free);
                }
                vars_free
            }
            ExpKind::Case(not_exp) => {
                let mut vars_free = Vec::new();
                for exp in not_exp.args() {
                    exp.free_vars_into(&mut vars_free);
                }
                vars_free
            }
            ExpKind::Str(fields) => {
                let mut vars_free = Vec::new();
                for ExpField { exp, .. } in fields {
                    exp.free_vars_into(&mut vars_free);
                }
                vars_free
            }
            ExpKind::Opt(exp_opt) => exp_opt.free_vars(),
            ExpKind::Slice(exp_base, exp_idx, exp_len) => {
                let mut vars_free = exp_base.free_vars();
                exp_idx.free_vars_into(&mut vars_free);
                exp_len.free_vars_into(&mut vars_free);
                vars_free
            }
            ExpKind::Upd(exp_base, path, exp_field) => {
                let mut vars_free = exp_base.free_vars();
                path.free_vars_into(&mut vars_free);
                exp_field.free_vars_into(&mut vars_free);
                vars_free
            }
            ExpKind::Call(_, _, args) => args.as_slice().free_vars(),
            ExpKind::Iter(exp_inner, ExpIter { iter, vars: vars_bound }) => {
                let mut vars_free = exp_inner.free_vars();
                for var_free in &mut vars_free {
                    if vars_bound
                        .iter()
                        .any(|var_bound| var_bound.syntax_eq(var_free))
                    {
                        var_free.iters.push(*iter);
                    }
                }
                vars_free
            }
        }
    }
}

impl FreeIds for ExpIter {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Patterns

impl FreeIds for Pattern {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for ListPattern {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for OptPattern {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Paths

impl FreeIds for PathKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Root => {}
            Self::Idx(path, exp_idx) => {
                path.free_ids_into(free);
                exp_idx.free_ids_into(free);
            }
            Self::Slice(path, exp_idx, exp_len) => {
                path.free_ids_into(free);
                exp_idx.free_ids_into(free);
                exp_len.free_ids_into(free);
            }
            Self::Dot(path, _) => path.free_ids_into(free),
        }
    }
}

impl FreeVars for Path {
    fn free_vars(&self) -> Vec<Var> {
        match &self.node {
            PathKind::Root => Vec::new(),
            PathKind::Idx(path_inner, exp_idx) => {
                let mut vars_free = path_inner.free_vars();
                exp_idx.free_vars_into(&mut vars_free);
                vars_free
            }
            PathKind::Slice(path_inner, exp_idx, exp_len) => {
                let mut vars_free = path_inner.free_vars();
                exp_idx.free_vars_into(&mut vars_free);
                exp_len.free_vars_into(&mut vars_free);
                vars_free
            }
            PathKind::Dot(path_inner, _) => path_inner.free_vars(),
        }
    }
}

// - Parameters

impl FreeIds for ParamKind {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Arguments

impl FreeIds for ArgKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Exp(exp) => exp.free_ids_into(free),
            Self::Def(_) => {}
        }
    }
}

impl FreeVars for Arg {
    fn free_vars(&self) -> Vec<Var> {
        match &self.node {
            ArgKind::Exp(exp) => exp.free_vars(),
            ArgKind::Def(_) => Vec::new(),
        }
    }
}

// - Premises

impl FreeIds for PremKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Rule(prem) => prem.free_ids_into(free),
            Self::If(prem) => prem.free_ids_into(free),
            Self::IfHold(prem) => prem.free_ids_into(free),
            Self::IfNotHold(prem) => prem.free_ids_into(free),
            Self::Iter(prem) => prem.free_ids_into(free),
            Self::Debug(prem) => prem.free_ids_into(free),
        }
    }
}

impl FreeIds for RulePrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.not_exp.free_ids_into(free);
    }
}

impl FreeIds for IfPrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
    }
}

impl FreeIds for IfHoldPrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.not_exp.free_ids_into(free);
    }
}

impl FreeIds for IfNotHoldPrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.not_exp.free_ids_into(free);
    }
}

impl FreeIds for IterPrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.prem.free_ids_into(free);
    }
}

impl FreeIds for DebugPrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
    }
}

impl FreeIds for PremIter {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Rules

impl FreeIds for RuleKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.not_exp.free_ids_into(free);
        self.prems.as_slice().free_ids_into(free);
    }
}

impl FreeIds for RuleGroupKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.rules.as_slice().free_ids_into(free);
    }
}

impl FreeIds for ElseGroupKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.rule.free_ids_into(free);
    }
}

// - Clauses

impl FreeIds for ClauseKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.args.as_slice().free_ids_into(free);
        self.exp.free_ids_into(free);
        self.prems.as_slice().free_ids_into(free);
    }
}

// - Table rows

impl FreeIds for TableRowKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.args.as_slice().free_ids_into(free);
        self.exp.free_ids_into(free);
    }
}

// == Type definitions

impl FreeIds for TypDef {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Extern(extern_typ) => extern_typ.free_ids_into(free),
            Self::Defined(defined_typ) => defined_typ.free_ids_into(free),
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
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Extern(extern_rel) => extern_rel.free_ids_into(free),
            Self::Defined(defined_rel) => defined_rel.free_ids_into(free),
        }
    }
}

impl FreeIds for ExternRel {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for DefinedRel {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.rule_groups.as_slice().free_ids_into(free);
        self.else_group.free_ids_into(free);
    }
}

// == Meta-function definitions

impl FreeIds for MetaFuncDef {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Extern(extern_func) => extern_func.free_ids_into(free),
            Self::Builtin(builtin_func) => builtin_func.free_ids_into(free),
            Self::Table(table_func) => table_func.free_ids_into(free),
            Self::Defined(defined_func) => defined_func.free_ids_into(free),
        }
    }
}

impl FreeIds for ExternFunc {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for BuiltinFunc {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for TableFunc {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.rows.as_slice().free_ids_into(free);
    }
}

impl FreeIds for DefinedFunc {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.clauses.as_slice().free_ids_into(free);
        self.else_clause.free_ids_into(free);
    }
}

// == Definitions

impl FreeIds for DefKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Typ(typ_def) => typ_def.free_ids_into(free),
            Self::Var(var_def) => var_def.free_ids_into(free),
            Self::Rel(rel_def) => rel_def.free_ids_into(free),
            Self::MetaFunc(meta_func_def) => meta_func_def.free_ids_into(free),
        }
    }
}

// == Specifications

impl FreeIds for Spec {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.as_slice().free_ids_into(free);
    }
}
