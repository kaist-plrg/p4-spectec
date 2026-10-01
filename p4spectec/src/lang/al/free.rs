//! Free identifiers in algorithmic-language data
//!
//! Only expressions contribute identifiers;
//! matches, paths, clauses, and rows collect from their expressions
//! and premises.

use crate::lang::{common::ds::set::IdSet, traits::free::FreeIds};

use super::ast::*;

// == Free identifiers

// - Premises

impl FreeIds for PremKind {
    fn free_ids_into(&self, free: &mut IdSet) {
        match self {
            Self::Rule(prem) => prem.free_ids_into(free),
            Self::If(prem) => prem.free_ids_into(free),
            Self::IfHold(prem) => prem.free_ids_into(free),
            Self::IfNotHold(prem) => prem.free_ids_into(free),
            Self::Let(prem) => prem.free_ids_into(free),
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

impl FreeIds for LetPrem {
    fn free_ids_into(&self, free: &mut IdSet) {
        self.exp_l.free_ids_into(free);
        self.exp_r.free_ids_into(free);
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

impl FreeIds for RuleGroupKind {
    fn free_ids(&self) -> IdSet {
        self.rule_match
            .free_ids()
            .union(self.rule_paths.as_slice().free_ids())
    }
}

impl FreeIds for ElseGroupKind {
    fn free_ids(&self) -> IdSet {
        self.rule_match.free_ids().union(self.rule_path.free_ids())
    }
}

impl FreeIds for RuleMatch {
    fn free_ids(&self) -> IdSet {
        self.exps_signature
            .as_slice()
            .free_ids()
            .union(self.exps_input.as_slice().free_ids())
            .union(self.prems.as_slice().free_ids())
    }
}

impl FreeIds for RulePath {
    fn free_ids(&self) -> IdSet {
        self.prems
            .as_slice()
            .free_ids()
            .union(self.exps_output.as_slice().free_ids())
    }
}

// - Clauses

impl FreeIds for ClauseKind {
    fn free_ids(&self) -> IdSet {
        self.args
            .as_slice()
            .free_ids()
            .union(self.exp.free_ids())
            .union(self.prems.as_slice().free_ids())
    }
}

// - Table rows

impl FreeIds for TableRowKind {
    fn free_ids(&self) -> IdSet {
        self.args
            .as_slice()
            .free_ids()
            .union(self.exp.free_ids())
            .union(self.prems.as_slice().free_ids())
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
        IdSet::new()
    }
}

impl FreeIds for DefinedRel {
    fn free_ids(&self) -> IdSet {
        self.rule_groups
            .as_slice()
            .free_ids()
            .union(self.else_group.free_ids())
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
        IdSet::new()
    }
}

impl FreeIds for BuiltinFunc {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}

impl FreeIds for TableFunc {
    fn free_ids(&self) -> IdSet {
        self.table_rows.as_slice().free_ids()
    }
}

impl FreeIds for DefinedFunc {
    fn free_ids(&self) -> IdSet {
        self.clauses
            .as_slice()
            .free_ids()
            .union(self.else_clause.free_ids())
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
