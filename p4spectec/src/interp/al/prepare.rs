//! Resolve AL syntax to slots and interned mixops
//!
//! The `Prepare` implementations rebuild source nodes as prepared syntax,
//! reserving slots in the frame layout of the unit being prepared.
//! Extern and builtin payloads have no body and pass through unchanged.
//! Each rule group, table row, and clause binds its own variables,
//! so it becomes a callable with its own frame layout.

use std::{mem, rc::Rc};

use crate::lang::data::{callable::Callable, frame::FrameLayout, notation::MixopArena};

use crate::lang::al::{ast as source, prepared::*};

use crate::runtime::{envs::elab::TDEnv, typdef::TypeDef};

use crate::interp::shared::prepare::{Prepare, il};

// - Preparation context

/// Records AL slots and interned mixops.
pub struct PrepareContext<'a> {
    /// The specification's notation arena.
    pub arena_mixop: &'a mut MixopArena,
    /// Source type definitions, with the callable's type parameters.
    pub tdenv: &'a TDEnv,
    /// The frame layout of the unit being prepared.
    pub layout: &'a mut FrameLayout,
}

impl<'a> PrepareContext<'a> {
    /// Borrows the arena, type definitions, and layout for preparation.
    pub fn new(
        arena_mixop: &'a mut MixopArena,
        tdenv: &'a TDEnv,
        layout: &'a mut FrameLayout,
    ) -> Self {
        Self { arena_mixop, tdenv, layout }
    }

    fn il(&mut self) -> il::PrepareContext<'_> {
        il::PrepareContext {
            arena_mixop: &mut *self.arena_mixop,
            tdenv: self.tdenv,
            layout: &mut *self.layout,
        }
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
            otherwise: self.otherwise,
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

// - Arguments

/// Clause and table-row arguments, the only arguments outside expressions, bind.
impl Prepare<PrepareContext<'_>> for source::ArgKind {
    type Output = ArgKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::ArgKind::Exp(exp) => ArgKind::Exp(exp.prepare(&mut ctx.il())),
            source::ArgKind::Def(id) => {
                // The parameter holds its function value in a slot
                ctx.layout.resolve_func(&id);
                ArgKind::Def(id)
            }
        }
    }
}

// - Clauses

impl Prepare<PrepareContext<'_>> for source::ClauseKind {
    type Output = ClauseKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ClauseKind {
            args: self.args.prepare(ctx),
            exp: self.exp.prepare(&mut ctx.il()),
            prems: self.prems.prepare(ctx),
        }
    }
}

impl Prepare<PrepareContext<'_>> for source::ElseClauseKind {
    type Output = ElseClauseKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ElseClauseKind { otherwise: self.otherwise, clause: self.clause.prepare(ctx) }
    }
}

// - Table rows

impl Prepare<PrepareContext<'_>> for source::TableRowKind {
    type Output = TableRowKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        TableRowKind {
            exps_signature: self.exps_signature.prepare(&mut ctx.il()),
            args: self.args.prepare(ctx),
            exp: self.exp.prepare(&mut ctx.il()),
            prems: self.prems.prepare(ctx),
        }
    }
}

// == Relation definitions

// - Relation definition

impl Prepare<PrepareContext<'_>> for source::RelDef {
    type Output = RelDef;

    /// Prepares the relation; each of its rule groups runs in a frame of its own.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::RelDef::Extern(rel_inner) => RelDef::Extern(rel_inner),
            source::RelDef::Defined(rel_inner) => RelDef::Defined(rel_inner.prepare(ctx)),
        }
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
            // Each group runs in its own frame: prepare it, then transfer the layout it filled
            rule_groups: (self.rule_groups.into_iter())
                .map(|group| {
                    let def = group.prepare(ctx);
                    let layout = Rc::new(mem::take(ctx.layout));
                    Callable { def, layout }
                })
                .collect(),
            else_group: (self.else_group).map(|group| {
                let def = group.prepare(ctx);
                let layout = Rc::new(mem::take(ctx.layout));
                Callable { def, layout }
            }),
            hints: self.hints,
        }
    }
}

// == Meta-function definitions

// - Meta-function definition

impl Prepare<PrepareContext<'_>> for source::MetaFuncDef {
    type Output = MetaFuncDef;

    /// Prepares the function; each of its rows or clauses runs in a frame of its own.
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

impl Prepare<PrepareContext<'_>> for source::TableFunc {
    type Output = TableFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        TableFunc {
            id: self.id,
            params: self.params,
            typ: self.typ,
            // Each row runs in its own frame: prepare it, then transfer the layout it filled
            table_rows: (self.table_rows.into_iter())
                .map(|row| {
                    let def = row.prepare(ctx);
                    let layout = Rc::new(mem::take(ctx.layout));
                    Callable { def, layout }
                })
                .collect(),
            hints: self.hints,
        }
    }
}

// - Defined function definition

impl Prepare<PrepareContext<'_>> for source::DefinedFunc {
    type Output = DefinedFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Type parameters are opaque types in the body
        let mut tdenv = ctx.tdenv.clone();
        for tparam in &self.tparams {
            tdenv.insert(tparam.clone(), TypeDef::Parameter);
        }
        let ctx = &mut PrepareContext::new(&mut *ctx.arena_mixop, &tdenv, &mut *ctx.layout);
        DefinedFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params,
            typ: self.typ,
            // Each clause runs in its own frame: prepare it, then transfer the layout it filled
            clauses: (self.clauses.into_iter())
                .map(|clause| {
                    let def = clause.prepare(ctx);
                    let layout = Rc::new(mem::take(ctx.layout));
                    Callable { def, layout }
                })
                .collect(),
            else_clause: (self.else_clause).map(|clause| {
                let def = clause.prepare(ctx);
                let layout = Rc::new(mem::take(ctx.layout));
                Callable { def, layout }
            }),
            hints: self.hints,
        }
    }
}
