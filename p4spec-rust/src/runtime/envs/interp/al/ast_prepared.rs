//! Slot instantiation of shared AL syntax
//!
//! The `Prepare` impls rebuild each node with slots in place of names,
//! reserving slots in the callable's `FrameLayout` as they go.
//! Extern and builtin definitions have no body and pass through unchanged.

use crate::lang::data::var::{IdSlot, VarSlot};

use crate::lang::al::ast as source;

pub use crate::lang::al::ast::{
    BuiltinFunc, DefinedTyp, ExternFunc, ExternRel, ExternTyp, TypDef, VarDef,
};

use crate::runtime::envs::interp::shared::frame::FrameLayout;

use crate::interp::shared::prepare::Prepare;

pub use crate::interp::shared::prepare::ast::*;

// == Prepared syntax

// - Premises

pub type Prem = source::Prem<IdSlot, VarSlot>;
pub type PremKind = source::PremKind<IdSlot, VarSlot>;
pub type RulePrem = source::RulePrem<IdSlot, VarSlot>;
pub type IfPrem = source::IfPrem<IdSlot, VarSlot>;
pub type IfHoldPrem = source::IfHoldPrem<IdSlot, VarSlot>;
pub type IfNotHoldPrem = source::IfNotHoldPrem<IdSlot, VarSlot>;
pub type LetPrem = source::LetPrem<IdSlot, VarSlot>;
pub type IterPrem = source::IterPrem<IdSlot, VarSlot>;
pub type DebugPrem = source::DebugPrem<IdSlot, VarSlot>;

// - Rules

pub type RuleGroup = source::RuleGroup<IdSlot, VarSlot>;
pub type RuleGroupKind = source::RuleGroupKind<IdSlot, VarSlot>;
pub type ElseGroup = source::ElseGroup<IdSlot, VarSlot>;
pub type ElseGroupKind = source::ElseGroupKind<IdSlot, VarSlot>;
pub type RuleMatch = source::RuleMatch<IdSlot, VarSlot>;
pub type RulePath = source::RulePath<IdSlot, VarSlot>;

// - Clauses

pub type Clause = source::Clause<IdSlot, VarSlot>;
pub type ClauseKind = source::ClauseKind<IdSlot, VarSlot>;
pub type ElseClause = source::ElseClause<IdSlot, VarSlot>;
pub type ElseClauseKind = source::ElseClauseKind<IdSlot, VarSlot>;

// - Table rows

pub type TableRow = source::TableRow<IdSlot, VarSlot>;
pub type TableRowKind = source::TableRowKind<IdSlot, VarSlot>;

// - Relation definitions

pub type RelDef = source::RelDef<IdSlot, VarSlot>;
pub type DefinedRel = source::DefinedRel<IdSlot, VarSlot>;

// - Meta-function definitions

pub type MetaFuncDef = source::MetaFuncDef<IdSlot, VarSlot>;
pub type TableFunc = source::TableFunc<IdSlot, VarSlot>;
pub type DefinedFunc = source::DefinedFunc<IdSlot, VarSlot>;

// - Definitions

pub type Def = source::Def<IdSlot, VarSlot>;
pub type DefKind = source::DefKind<IdSlot, VarSlot>;
pub type Spec = Vec<Def>;

// == Preparation traversal

// - Premises

impl Prepare for source::PremKind {
    type Output = PremKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            source::PremKind::Rule(prem_inner) => PremKind::Rule(prem_inner.prepare(layout)),
            source::PremKind::If(prem_inner) => PremKind::If(prem_inner.prepare(layout)),
            source::PremKind::IfHold(prem_inner) => PremKind::IfHold(prem_inner.prepare(layout)),
            source::PremKind::IfNotHold(prem_inner) => {
                PremKind::IfNotHold(prem_inner.prepare(layout))
            }
            source::PremKind::Let(prem_inner) => PremKind::Let(prem_inner.prepare(layout)),
            source::PremKind::Iter(prem_inner) => PremKind::Iter(prem_inner.prepare(layout)),
            source::PremKind::Debug(prem_inner) => PremKind::Debug(prem_inner.prepare(layout)),
        }
    }
}

// - Rule premise

impl Prepare for source::RulePrem {
    type Output = RulePrem;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        RulePrem { id: self.id, not_exp: self.not_exp.prepare(layout), input_hint: self.input_hint }
    }
}

// - If premise

impl Prepare for source::IfPrem {
    type Output = IfPrem;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        IfPrem { exp: self.exp.prepare(layout) }
    }
}

// - If-hold premise

impl Prepare for source::IfHoldPrem {
    type Output = IfHoldPrem;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        IfHoldPrem { id: self.id, not_exp: self.not_exp.prepare(layout) }
    }
}

// - If-not-hold premise

impl Prepare for source::IfNotHoldPrem {
    type Output = IfNotHoldPrem;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        IfNotHoldPrem { id: self.id, not_exp: self.not_exp.prepare(layout) }
    }
}

// - Let premise

impl Prepare for source::LetPrem {
    type Output = LetPrem;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        LetPrem { exp_l: self.exp_l.prepare(layout), exp_r: self.exp_r.prepare(layout) }
    }
}

// - Iterated premise

impl Prepare for source::IterPrem {
    type Output = IterPrem;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        IterPrem { prem: self.prem.prepare(layout), prem_iter: self.prem_iter.prepare(layout) }
    }
}

// - Debug premise

impl Prepare for source::DebugPrem {
    type Output = DebugPrem;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        DebugPrem { exp: self.exp.prepare(layout) }
    }
}

// - Rules

// - Rule group

impl Prepare for source::RuleGroupKind {
    type Output = RuleGroupKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        RuleGroupKind {
            id: self.id,
            rule_match: self.rule_match.prepare(layout),
            rule_paths: self.rule_paths.prepare(layout),
        }
    }
}

// - Else group

impl Prepare for source::ElseGroupKind {
    type Output = ElseGroupKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        ElseGroupKind {
            id: self.id,
            rule_match: self.rule_match.prepare(layout),
            rule_path: self.rule_path.prepare(layout),
        }
    }
}

// - Rule match

impl Prepare for source::RuleMatch {
    type Output = RuleMatch;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        RuleMatch {
            exps_signature: self.exps_signature.prepare(layout),
            exps_input: self.exps_input.prepare(layout),
            prems: self.prems.prepare(layout),
        }
    }
}

// - Rule path

impl Prepare for source::RulePath {
    type Output = RulePath;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        RulePath {
            id: self.id,
            prems: self.prems.prepare(layout),
            exps_output: self.exps_output.prepare(layout),
        }
    }
}

// - Clauses

impl Prepare for source::ClauseKind {
    type Output = ClauseKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        ClauseKind {
            args: self.args.prepare(layout),
            exp: self.exp.prepare(layout),
            prems: self.prems.prepare(layout),
        }
    }
}

// - Table rows

impl Prepare for source::TableRowKind {
    type Output = TableRowKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        TableRowKind {
            exps_signature: self.exps_signature.prepare(layout),
            args: self.args.prepare(layout),
            exp: self.exp.prepare(layout),
            prems: self.prems.prepare(layout),
        }
    }
}

// == Relation definitions

// - Relation definition

impl Prepare for source::RelDef {
    type Output = RelDef;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            source::RelDef::Extern(rel_inner) => RelDef::Extern(rel_inner),
            source::RelDef::Defined(rel_inner) => RelDef::Defined(rel_inner.prepare(layout)),
        }
    }
}

// - Defined relation definition

impl Prepare for source::DefinedRel {
    type Output = DefinedRel;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        DefinedRel {
            id: self.id,
            not_typ: self.not_typ,
            input_hint: self.input_hint,
            rule_groups: self.rule_groups.prepare(layout),
            else_group: self.else_group.prepare(layout),
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
            source::MetaFuncDef::Extern(func_inner) => MetaFuncDef::Extern(func_inner),
            source::MetaFuncDef::Builtin(func_inner) => MetaFuncDef::Builtin(func_inner),
            source::MetaFuncDef::Table(func_inner) => {
                MetaFuncDef::Table(func_inner.prepare(layout))
            }
            source::MetaFuncDef::Defined(func_inner) => {
                MetaFuncDef::Defined(func_inner.prepare(layout))
            }
        }
    }
}

// - Table function definition

impl Prepare for source::TableFunc {
    type Output = TableFunc;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        TableFunc {
            id: self.id,
            params: self.params,
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
            params: self.params,
            typ: self.typ,
            clauses: self.clauses.prepare(layout),
            else_clause: self.else_clause.prepare(layout),
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
