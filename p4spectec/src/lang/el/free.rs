//! Free identifiers in elaboration-language data
//!
//! Only expressions contribute identifiers;
//! types, operators, and declarations have none.
//! Rules, table rows, and function clauses collect from their expressions
//! and premises.

use crate::lang::{common::ds::set::IdSet, traits::free::FreeIds};

use super::ast::*;

// == Free identifiers

// - Types

impl FreeIds for Typ {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Plain types

impl FreeIds for PlainTypKind {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Notation types

impl FreeIds for NotTypKind {
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

impl FreeIds for TypCase {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Operators

impl FreeIds for NumOp {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for UnOp {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for BinOp {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for CmpOp {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Expressions

impl FreeIds for ExpKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Bool(_)
            | Self::Num(_, _)
            | Self::Text(_)
            | Self::Eps
            | Self::Atom(_)
            | Self::Hole(_)
            | Self::Latex(_) => {}
            Self::Id(id) => {
                free.insert(id.clone());
            }
            Self::Un(_, exp)
            | Self::Arith(exp)
            | Self::Len(exp)
            | Self::Dot(exp, _)
            | Self::Paren(exp)
            | Self::Iter(exp, _)
            | Self::Sub(exp, _)
            | Self::Brack(_, exp, _)
            | Self::Unparen(exp) => exp.free_ids_into(free),
            Self::Bin(exp_l, _, exp_r)
            | Self::Cmp(exp_l, _, exp_r)
            | Self::Cons(exp_l, exp_r)
            | Self::Cat(exp_l, exp_r)
            | Self::Idx(exp_l, exp_r)
            | Self::Mem(exp_l, exp_r)
            | Self::Infix(exp_l, _, exp_r)
            | Self::Fuse(exp_l, _, exp_r) => {
                exp_l.free_ids_into(free);
                exp_r.free_ids_into(free);
            }
            Self::List(exps) | Self::Tuple(exps) | Self::Seq(exps) => {
                exps.as_slice().free_ids_into(free);
            }
            Self::Slice(exp_base, exp_idx, exp_len) => {
                exp_base.free_ids_into(free);
                exp_idx.free_ids_into(free);
                exp_len.free_ids_into(free);
            }
            Self::Str(fields) => {
                for (_, exp) in fields {
                    exp.free_ids_into(free);
                }
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

impl FreeIds for Hole {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// - Paths

impl FreeIds for PathKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Root => {}
            Self::Idx(path, exp) => {
                path.free_ids_into(free);
                exp.free_ids_into(free);
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

// - Hints

impl FreeIds for Hint {
    fn free_ids(&self) -> IdSet {
        self.exp.free_ids()
    }
}

// - Premises

impl FreeIds for PremKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Var(prem) => prem.free_ids_into(free),
            Self::Rule(prem) => prem.free_ids_into(free),
            Self::RuleNot(prem) => prem.free_ids_into(free),
            Self::If(prem) => prem.free_ids_into(free),
            Self::Else => {}
            Self::Iter(prem) => prem.free_ids_into(free),
            Self::Debug(prem) => prem.free_ids_into(free),
        }
    }
}

impl FreeIds for VarPrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        free.insert(self.id.clone());
    }
}

impl FreeIds for RulePrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
    }
}

impl FreeIds for RuleNotPrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
    }
}

impl FreeIds for IfPrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
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

// - Rules

impl FreeIds for RuleKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp.free_ids_into(free);
        self.prems.as_slice().free_ids_into(free);
    }
}

// - Table rows

impl FreeIds for TableRowKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp_pattern.free_ids_into(free);
        self.exp_body.free_ids_into(free);
    }
}

// == Syntax definitions

impl FreeIds for ExternSyntaxDef {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for SyntaxDef {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for SyntaxDefEntry {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

// == Type definitions

impl FreeIds for TypDef {
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

impl FreeIds for ExternRelDef {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for RelDef {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for RuleGroupDef {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.rules.as_slice().free_ids_into(free);
    }
}

// == Meta-function definitions

impl FreeIds for ExternDecDef {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for BuiltinDecDef {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for TableDecDef {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for FuncDecDef {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for TableDef {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.rows.as_slice().free_ids_into(free);
    }
}

impl FreeIds for FuncDef {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.args.as_slice().free_ids_into(free);
        self.exp.free_ids_into(free);
        self.prems.as_slice().free_ids_into(free);
    }
}

// == Definitions

impl FreeIds for DefKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::ExternSyntax(def) => def.free_ids_into(free),
            Self::Syntax(def) => def.free_ids_into(free),
            Self::Typ(def) => def.free_ids_into(free),
            Self::Var(def) => def.free_ids_into(free),
            Self::ExternRel(def) => def.free_ids_into(free),
            Self::Rel(def) => def.free_ids_into(free),
            Self::RuleGroup(def) => def.free_ids_into(free),
            Self::ExternDec(def) => def.free_ids_into(free),
            Self::BuiltinDec(def) => def.free_ids_into(free),
            Self::TableDec(def) => def.free_ids_into(free),
            Self::FuncDec(def) => def.free_ids_into(free),
            Self::TableDef(def) => def.free_ids_into(free),
            Self::FuncDef(def) => def.free_ids_into(free),
            Self::Sep => {}
        }
    }
}

// == Specifications

impl FreeIds for Spec {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.as_slice().free_ids_into(free);
    }
}
