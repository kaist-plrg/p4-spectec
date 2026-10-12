//! Resolve AL syntax to slots and interned mixops
//!
//! The `Prepare` implementations rebuild source nodes as prepared syntax,
//! reserving slots in the callable's frame layout.
//! Extern and builtin payloads have no body and pass through unchanged.
//! Top-level relations and functions return callables with their own layouts.

use std::{mem, rc::Rc};

use crate::lang::data::notation::MixopArena;

use crate::lang::al::{ast as source, prepared::*};

use crate::runtime::envs::interp::shared::{callable::Callable, frame::FrameLayout};

use crate::interp::shared::prepare::{Prepare, il};

// - Preparation context

/// Records AL slots and interned mixops.
pub struct PrepareContext<'a> {
    /// The specification's notation arena.
    pub arena_mixop: &'a mut MixopArena,
    /// The callable's frame layout.
    pub layout: &'a mut FrameLayout,
}

impl<'a> PrepareContext<'a> {
    /// Borrows the notation arena and callable layout for preparation.
    pub fn new(arena_mixop: &'a mut MixopArena, layout: &'a mut FrameLayout) -> Self {
        Self { arena_mixop, layout }
    }

    fn il(&mut self) -> il::PrepareContext<'_> {
        il::PrepareContext { arena_mixop: &mut *self.arena_mixop, layout: &mut *self.layout }
    }
}

// - Premises

impl Prepare<PrepareContext<'_>> for source::PremKind {
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

impl Prepare<PrepareContext<'_>> for source::RulePrem {
    type Output = RulePrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        RulePrem {
            id: self.id,
            not_exp: self.not_exp.prepare(&mut ctx.il()),
            input_hint: self.input_hint,
        }
    }
}

// - If premise

impl Prepare<PrepareContext<'_>> for source::IfPrem {
    type Output = IfPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        IfPrem { exp: self.exp.prepare(&mut ctx.il()) }
    }
}

// - If-hold premise

impl Prepare<PrepareContext<'_>> for source::IfHoldPrem {
    type Output = IfHoldPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        IfHoldPrem { id: self.id, not_exp: self.not_exp.prepare(&mut ctx.il()) }
    }
}

// - If-not-hold premise

impl Prepare<PrepareContext<'_>> for source::IfNotHoldPrem {
    type Output = IfNotHoldPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        IfNotHoldPrem { id: self.id, not_exp: self.not_exp.prepare(&mut ctx.il()) }
    }
}

// - Let premise

impl Prepare<PrepareContext<'_>> for source::LetPrem {
    type Output = LetPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        LetPrem {
            exp_l: self.exp_l.prepare(&mut ctx.il()),
            exp_r: self.exp_r.prepare(&mut ctx.il()),
        }
    }
}

// - Iterated premise

impl Prepare<PrepareContext<'_>> for source::IterPrem {
    type Output = IterPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        IterPrem { prem: self.prem.prepare(ctx), prem_iter: self.prem_iter.prepare(&mut ctx.il()) }
    }
}

// - Debug premise

impl Prepare<PrepareContext<'_>> for source::DebugPrem {
    type Output = DebugPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        DebugPrem { exp: self.exp.prepare(&mut ctx.il()) }
    }
}

// - Rules

// - Rule group

impl Prepare<PrepareContext<'_>> for source::RuleGroupKind {
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

impl Prepare<PrepareContext<'_>> for source::ElseGroupKind {
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

impl Prepare<PrepareContext<'_>> for source::RuleMatch {
    type Output = RuleMatch;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        RuleMatch {
            exps_signature: self.exps_signature.prepare(&mut ctx.il()),
            exps_input: self.exps_input.prepare(&mut ctx.il()),
            prems: self.prems.prepare(ctx),
        }
    }
}

// - Rule path

impl Prepare<PrepareContext<'_>> for source::RulePath {
    type Output = RulePath;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        RulePath {
            id: self.id,
            prems: self.prems.prepare(ctx),
            exps_output: self.exps_output.prepare(&mut ctx.il()),
        }
    }
}

// - Clauses

impl Prepare<PrepareContext<'_>> for source::ClauseKind {
    type Output = ClauseKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ClauseKind {
            args: self.args.prepare(&mut ctx.il()),
            exp: self.exp.prepare(&mut ctx.il()),
            prems: self.prems.prepare(ctx),
        }
    }
}

// - Table rows

impl Prepare<PrepareContext<'_>> for source::TableRowKind {
    type Output = TableRowKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        TableRowKind {
            exps_signature: self.exps_signature.prepare(&mut ctx.il()),
            args: self.args.prepare(&mut ctx.il()),
            exp: self.exp.prepare(&mut ctx.il()),
            prems: self.prems.prepare(ctx),
        }
    }
}

// == Relation definitions

// - Relation definition

impl Prepare<PrepareContext<'_>> for source::RelDef {
    type Output = Callable<RelDef>;

    /// Prepares the relation and transfers its callable layout.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let rel = match self {
            source::RelDef::Extern(rel_inner) => RelDef::Extern(rel_inner),
            source::RelDef::Defined(rel_inner) => {
                let rel_inner = rel_inner.prepare(ctx);
                RelDef::Defined(rel_inner)
            }
        };

        // Transfer this callable's layout and leave the next one empty
        let layout = mem::take(ctx.layout);
        let layout = Rc::new(layout);
        Callable { def: rel, layout }
    }
}

// - Defined relation definition

impl Prepare<PrepareContext<'_>> for source::DefinedRel {
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

impl Prepare<PrepareContext<'_>> for source::MetaFuncDef {
    type Output = Callable<MetaFuncDef>;

    /// Prepares the function and transfers its callable layout.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let func = match self {
            source::MetaFuncDef::Extern(func_inner) => MetaFuncDef::Extern(func_inner),
            source::MetaFuncDef::Builtin(func_inner) => MetaFuncDef::Builtin(func_inner),
            source::MetaFuncDef::Table(func_inner) => {
                let func_inner = func_inner.prepare(ctx);
                MetaFuncDef::Table(func_inner)
            }
            source::MetaFuncDef::Defined(func_inner) => {
                let func_inner = func_inner.prepare(ctx);
                MetaFuncDef::Defined(func_inner)
            }
        };

        // Transfer this callable's layout and leave the next one empty
        let layout = mem::take(ctx.layout);
        let layout = Rc::new(layout);
        Callable { def: func, layout }
    }
}

// - Table function definition

impl Prepare<PrepareContext<'_>> for source::TableFunc {
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

impl Prepare<PrepareContext<'_>> for source::DefinedFunc {
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
