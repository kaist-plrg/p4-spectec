//! Resolve AL syntax to slots and interned mixops
//!
//! The `Prepare` implementations rebuild source nodes as prepared syntax,
//! reserving slots in the callable's frame layout.
//! Extern and builtin definitions have no body and pass through unchanged.

use crate::lang::al::{ast as source, prepared::*};

use crate::interp::shared::prepare::{Prepare, PrepareContext};

// - Premises

impl Prepare for source::PremKind {
    type Output = PremKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::PremKind::Rule(prem_inner) => PremKind::Rule(prem_inner.prepare(ctx)),
            source::PremKind::If(prem_inner) => PremKind::If(prem_inner.prepare(ctx)),
            source::PremKind::IfHold(prem_inner) => PremKind::IfHold(prem_inner.prepare(ctx)),
            source::PremKind::IfNotHold(prem_inner) => PremKind::IfNotHold(prem_inner.prepare(ctx)),
            source::PremKind::Let(prem_inner) => PremKind::Let(prem_inner.prepare(ctx)),
            source::PremKind::Iter(prem_inner) => PremKind::Iter(prem_inner.prepare(ctx)),
            source::PremKind::Debug(prem_inner) => PremKind::Debug(prem_inner.prepare(ctx)),
        }
    }
}

// - Rule premise

impl Prepare for source::RulePrem {
    type Output = RulePrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        RulePrem { id: self.id, not_exp: self.not_exp.prepare(ctx), input_hint: self.input_hint }
    }
}

// - If premise

impl Prepare for source::IfPrem {
    type Output = IfPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        IfPrem { exp: self.exp.prepare(ctx) }
    }
}

// - If-hold premise

impl Prepare for source::IfHoldPrem {
    type Output = IfHoldPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        IfHoldPrem { id: self.id, not_exp: self.not_exp.prepare(ctx) }
    }
}

// - If-not-hold premise

impl Prepare for source::IfNotHoldPrem {
    type Output = IfNotHoldPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        IfNotHoldPrem { id: self.id, not_exp: self.not_exp.prepare(ctx) }
    }
}

// - Let premise

impl Prepare for source::LetPrem {
    type Output = LetPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        LetPrem { exp_l: self.exp_l.prepare(ctx), exp_r: self.exp_r.prepare(ctx) }
    }
}

// - Iterated premise

impl Prepare for source::IterPrem {
    type Output = IterPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        IterPrem { prem: self.prem.prepare(ctx), prem_iter: self.prem_iter.prepare(ctx) }
    }
}

// - Debug premise

impl Prepare for source::DebugPrem {
    type Output = DebugPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        DebugPrem { exp: self.exp.prepare(ctx) }
    }
}

// - Rules

// - Rule group

impl Prepare for source::RuleGroupKind {
    type Output = RuleGroupKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        RuleGroupKind {
            id: self.id,
            rule_match: self.rule_match.prepare(ctx),
            rule_paths: self.rule_paths.prepare(ctx),
        }
    }
}

// - Else group

impl Prepare for source::ElseGroupKind {
    type Output = ElseGroupKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ElseGroupKind {
            id: self.id,
            rule_match: self.rule_match.prepare(ctx),
            rule_path: self.rule_path.prepare(ctx),
        }
    }
}

// - Rule match

impl Prepare for source::RuleMatch {
    type Output = RuleMatch;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        RuleMatch {
            exps_signature: self.exps_signature.prepare(ctx),
            exps_input: self.exps_input.prepare(ctx),
            prems: self.prems.prepare(ctx),
        }
    }
}

// - Rule path

impl Prepare for source::RulePath {
    type Output = RulePath;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        RulePath {
            id: self.id,
            prems: self.prems.prepare(ctx),
            exps_output: self.exps_output.prepare(ctx),
        }
    }
}

// - Clauses

impl Prepare for source::ClauseKind {
    type Output = ClauseKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ClauseKind {
            args: self.args.prepare(ctx),
            exp: self.exp.prepare(ctx),
            prems: self.prems.prepare(ctx),
        }
    }
}

// - Table rows

impl Prepare for source::TableRowKind {
    type Output = TableRowKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        TableRowKind {
            exps_signature: self.exps_signature.prepare(ctx),
            args: self.args.prepare(ctx),
            exp: self.exp.prepare(ctx),
            prems: self.prems.prepare(ctx),
        }
    }
}

// == Relation definitions

// - Relation definition

impl Prepare for source::RelDef {
    type Output = RelDef;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::RelDef::Extern(rel_inner) => RelDef::Extern(rel_inner),
            source::RelDef::Defined(rel_inner) => RelDef::Defined(rel_inner.prepare(ctx)),
        }
    }
}

// - Defined relation definition

impl Prepare for source::DefinedRel {
    type Output = DefinedRel;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        DefinedRel {
            id: self.id,
            not_typ: self.not_typ,
            input_hint: self.input_hint,
            rule_groups: self.rule_groups.prepare(ctx),
            else_group: self.else_group.prepare(ctx),
            hints: self.hints,
        }
    }
}

// == Meta-function definitions

// - Meta-function definition

impl Prepare for source::MetaFuncDef {
    type Output = MetaFuncDef;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::MetaFuncDef::Extern(func_inner) => MetaFuncDef::Extern(func_inner),
            source::MetaFuncDef::Builtin(func_inner) => MetaFuncDef::Builtin(func_inner),
            source::MetaFuncDef::Table(func_inner) => MetaFuncDef::Table(func_inner.prepare(ctx)),
            source::MetaFuncDef::Defined(func_inner) => {
                MetaFuncDef::Defined(func_inner.prepare(ctx))
            }
        }
    }
}

// - Table function definition

impl Prepare for source::TableFunc {
    type Output = TableFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        TableFunc {
            id: self.id,
            params: self.params,
            typ: self.typ,
            table_rows: self.table_rows.prepare(ctx),
            hints: self.hints,
        }
    }
}

// - Defined function definition

impl Prepare for source::DefinedFunc {
    type Output = DefinedFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        DefinedFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params,
            typ: self.typ,
            clauses: self.clauses.prepare(ctx),
            else_clause: self.else_clause.prepare(ctx),
            hints: self.hints,
        }
    }
}

// == Definitions

impl Prepare for source::DefKind {
    type Output = DefKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::DefKind::Typ(typdef) => DefKind::Typ(typdef),
            source::DefKind::Var(def_var) => DefKind::Var(def_var),
            source::DefKind::Rel(rel_inner) => DefKind::Rel(rel_inner.prepare(ctx)),
            source::DefKind::MetaFunc(func_inner) => DefKind::MetaFunc(func_inner.prepare(ctx)),
        }
    }
}
