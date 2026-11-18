//! Resolve AL syntax to slots and interned mixops
//!
//! The `Prepare` implementations rebuild source nodes as prepared syntax,
//! reserving slots in the frame layout of the unit being prepared.
//! Extern and builtin payloads have no body and pass through unchanged.
//! Each rule group, table row, and clause binds its own variables,
//! so it becomes a callable with its own frame layout.
//! Under `tco`, a candidate in tail position concludes with a tail call:
//! a call it returns, or a final relation call whose outputs it concludes with,
//! which leaves the premises for the rule path's result.
//! A candidate is in tail position when no other can conclude after it mismatches:
//! it is tried last, or under deterministic choice it is the only one.

use std::{mem, rc::Rc};

use crate::lang::{
    data::{callable::Callable, frame::FrameLayout, notation::MixopArena},
    hints::input,
    traits::eq::SyntaxEq,
};

use crate::lang::al::{ast as source, prepared::*};

use crate::runtime::{envs::elab::TDEnv, typdef::TypeDef};

use crate::interp::shared::prepare::{
    Prepare,
    il::{self, ExpIterFor},
};

// - Preparation context

/// Records AL slots and interned mixops.
pub struct PrepareContext<'a> {
    /// The specification's notation arena.
    pub arena_mixop: &'a mut MixopArena,
    /// Source type definitions, with the callable's type parameters.
    pub tdenv: &'a TDEnv,
    /// The frame layout of the unit being prepared.
    pub layout: &'a mut FrameLayout,
    /// The runner's candidate-selection mode.
    pub det: bool,
    /// Make calls in tail position as tail calls.
    pub tco: bool,
    /// Whether the candidate being prepared ends with a tail call.
    tail: bool,
}

impl<'a> PrepareContext<'a> {
    /// Borrows the arena, type definitions, and layout for preparation.
    pub fn new(
        arena_mixop: &'a mut MixopArena,
        tdenv: &'a TDEnv,
        layout: &'a mut FrameLayout,
        det: bool,
        tco: bool,
    ) -> Self {
        Self { arena_mixop, tdenv, layout, det, tco, tail: false }
    }

    /// Reborrows the context for a candidate that ends with a tail call or not.
    fn at(&mut self, tail: bool) -> PrepareContext<'_> {
        PrepareContext {
            arena_mixop: &mut *self.arena_mixop,
            tdenv: self.tdenv,
            layout: &mut *self.layout,
            det: self.det,
            tco: self.tco,
            tail,
        }
    }

    /// Whether the candidate `idx` of `len`, followed by no otherwise candidate, is in tail position.
    fn tail_at(&self, idx: usize, len: usize) -> bool {
        self.tco && idx + 1 == len && (!self.det || len == 1)
    }

    fn il(&mut self) -> il::PrepareContext<'_> {
        self.il_exp_iter_for(ExpIterFor::Eval)
    }

    fn il_exp_iter_for(&mut self, exp_iter_for: ExpIterFor) -> il::PrepareContext<'_> {
        il::PrepareContext {
            arena_mixop: &mut *self.arena_mixop,
            tdenv: self.tdenv,
            layout: &mut *self.layout,
            exp_iter_for,
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
        // The input hint separates the arguments once, before any call
        let (exps_input, exps_output) = input::split(&self.input_hint, self.not_exp.into_args())
            .expect("input hint must fit relation");
        let exps_input = exps_input.prepare(&mut ctx.il());
        // The outputs are patterns the call binds
        let exps_output = exps_output.prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        RulePrem { id: self.id, exps_input, exps_output }
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
        // The left side is a pattern the value binds
        let exp_l = self
            .exp_l
            .prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        let exp_r = self.exp_r.prepare(&mut ctx.il());
        LetPrem { exp_l, exp_r }
    }
}

// - Iterated premise

impl Prepare<PrepareContext<'_>> for source::IterPrem {
    type Output = IterPrem;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let prem = self.prem.prepare(ctx);
        let prem_iter = self.prem_iter.prepare(&mut ctx.il());
        IterPrem { prem, prem_iter }
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
        let rule_match = self.rule_match.prepare(&mut ctx.at(false));
        // Only the group's last path can be its relation's last candidate
        let tail = ctx.tail;
        let len = self.rule_paths.len();
        let mut rule_paths = Vec::with_capacity(len);
        for (idx, path) in self.rule_paths.into_iter().enumerate() {
            let tail_path = tail && idx + 1 == len;
            rule_paths.push(path.prepare(&mut ctx.at(tail_path)));
        }
        RuleGroupKind { id: self.id, rule_match, rule_paths }
    }
}

// - Else group

impl Prepare<PrepareContext<'_>> for source::ElseGroupKind {
    type Output = ElseGroupKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let rule_match = self.rule_match.prepare(ctx);
        let rule_path = self.rule_path.prepare(ctx);
        ElseGroupKind { otherwise: self.otherwise, id: self.id, rule_match, rule_path }
    }
}

// - Rule match

impl Prepare<PrepareContext<'_>> for source::RuleMatch {
    type Output = RuleMatch;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let exps_signature = self.exps_signature.prepare(&mut ctx.il());
        // The inputs are patterns the call's arguments bind
        let exps_input = self
            .exps_input
            .prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        let prems = self.prems.prepare(ctx);
        RuleMatch { exps_signature, exps_input, prems }
    }
}

// - Rule path

impl Prepare<PrepareContext<'_>> for source::RulePath {
    type Output = RulePath;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Compare source expressions before their mixops become arena handles
        let tail = matches!(
            self.prems.last().map(|prem| &prem.node),
            Some(source::PremKind::Rule(prem)) if prem.is_tail_call(ctx.tail, &self.res)
        );
        let mut prems = self.prems;
        // The last premise's call concludes the path in place of its outputs
        let prem_tail = tail.then(|| prems.pop().expect("a tail call ends a premise list"));
        let prems = prems.prepare(ctx);
        let res = match prem_tail {
            // The tail call's inputs evaluate after the remaining premises
            Some(prem) => {
                let source::PremKind::Rule(prem) = prem.node else {
                    unreachable!("a tail call is a rule premise")
                };
                let (exps_input, _) = input::split(&prem.input_hint, prem.not_exp.into_args())
                    .expect("input hint must fit relation");
                let exps_input = exps_input.prepare(&mut ctx.il());
                RelResult::Tail(TailRel { id: prem.id, exps_input })
            }
            // Otherwise the path concludes with its outputs
            None => RelResult::Values(self.res.prepare(&mut ctx.il())),
        };
        RulePath { id: self.id, prems, res }
    }
}

impl source::RulePrem {
    /// Checks tail position and whether its rule path concludes with exactly its outputs.
    fn is_tail_call(&self, tail: bool, exps_res: &[source::Exp]) -> bool {
        // A candidate not in tail position must retain its outputs
        if !tail {
            return false;
        }
        // Output and result expressions must agree in order, ignoring source spans
        let (_, exps_output) = input::split(&self.input_hint, self.not_exp.args().iter().collect())
            .expect("input hint must fit relation");
        exps_output.len() == exps_res.len()
            && exps_output
                .iter()
                .zip(exps_res)
                .all(|(exp_l, exp_r)| exp_l.syntax_eq(exp_r))
    }
}

// - Arguments

/// Clause and table-row arguments, the only arguments outside expressions, bind.
impl Prepare<PrepareContext<'_>> for source::ArgKind {
    type Output = ArgKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            // An expression argument is a pattern the call's argument binds
            source::ArgKind::Exp(exp) => {
                let exp = exp.prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
                ArgKind::Exp(exp)
            }
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
        let tail = ctx.tail;
        let args = self.args.prepare(ctx);
        let ret = FuncReturn::prepare(self.ret, &mut ctx.il(), tail);
        let prems = self.prems.prepare(ctx);
        ClauseKind { args, ret, prems }
    }
}

impl Prepare<PrepareContext<'_>> for source::ElseClauseKind {
    type Output = ElseClauseKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let clause = self.clause.prepare(ctx);
        ElseClauseKind { otherwise: self.otherwise, clause }
    }
}

// - Table rows

impl Prepare<PrepareContext<'_>> for source::TableRowKind {
    type Output = TableRowKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let tail = ctx.tail;
        let exps_signature = self.exps_signature.prepare(&mut ctx.il());
        let args = self.args.prepare(ctx);
        let ret = FuncReturn::prepare(self.ret, &mut ctx.il(), tail);
        let prems = self.prems.prepare(ctx);
        TableRowKind { exps_signature, args, ret, prems }
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
        // Paths are candidates across groups; the otherwise group is tried last
        let len_paths = (self.rule_groups.iter())
            .map(|group| group.node.rule_paths.len())
            .sum();
        let tail = self.else_group.is_none() && ctx.tail_at(len_paths - 1, len_paths);
        // Only the last group holds the relation's last path
        let len_groups = self.rule_groups.len();
        let mut rule_groups = Vec::with_capacity(len_groups);
        for (idx, group) in self.rule_groups.into_iter().enumerate() {
            let tail_group = tail && idx + 1 == len_groups;
            // Each group runs in its own frame: prepare it, then transfer the layout it filled
            let def = group.prepare(&mut ctx.at(tail_group));
            let layout = Rc::new(mem::take(ctx.layout));
            rule_groups.push(Callable { def, layout });
        }
        // The otherwise group concludes the relation when it runs
        let tco = ctx.tco;
        let mut else_group = None;
        if let Some(group) = self.else_group {
            let def = group.prepare(&mut ctx.at(tco));
            let layout = Rc::new(mem::take(ctx.layout));
            else_group = Some(Callable { def, layout });
        }
        DefinedRel {
            id: self.id,
            not_typ: self.not_typ,
            input_hint: self.input_hint,
            rule_groups,
            else_group,
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
        // Rows are always tried in order, so only the last row is in tail position
        let len = self.table_rows.len();
        let mut table_rows = Vec::with_capacity(len);
        for (idx, row) in self.table_rows.into_iter().enumerate() {
            let tail = ctx.tco && idx + 1 == len;
            // Each row runs in its own frame: prepare it, then transfer the layout it filled
            let def = row.prepare(&mut ctx.at(tail));
            let layout = Rc::new(mem::take(ctx.layout));
            table_rows.push(Callable { def, layout });
        }
        TableFunc { id: self.id, params: self.params, typ: self.typ, table_rows, hints: self.hints }
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
        let ctx = &mut PrepareContext::new(
            &mut *ctx.arena_mixop,
            &tdenv,
            &mut *ctx.layout,
            ctx.det,
            ctx.tco,
        );
        // Clauses are candidates; the otherwise clause is tried last
        let len = self.clauses.len();
        let tail_last = self.else_clause.is_none();
        let mut clauses = Vec::with_capacity(len);
        for (idx, clause) in self.clauses.into_iter().enumerate() {
            let tail = tail_last && ctx.tail_at(idx, len);
            // Each clause runs in its own frame: prepare it, then transfer the layout it filled
            let def = clause.prepare(&mut ctx.at(tail));
            let layout = Rc::new(mem::take(ctx.layout));
            clauses.push(Callable { def, layout });
        }
        // The otherwise clause concludes the function when it runs
        let tco = ctx.tco;
        let mut else_clause = None;
        if let Some(clause) = self.else_clause {
            let def = clause.prepare(&mut ctx.at(tco));
            let layout = Rc::new(mem::take(ctx.layout));
            else_clause = Some(Callable { def, layout });
        }
        DefinedFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params,
            typ: self.typ,
            clauses,
            else_clause,
            hints: self.hints,
        }
    }
}
