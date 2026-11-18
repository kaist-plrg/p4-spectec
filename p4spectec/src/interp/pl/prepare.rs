//! Load-time preparation of PL control flow and expressions
//!
//! Prosification produces annotated blocks with name-based expressions.
//! `Prepare` consumes those nodes to build executable blocks:
//! instructions retain their hints, while expressions drop theirs
//! and become IL's prepared expressions, with identifiers resolved to slots.
//! Each callable collects its frame layout during the same traversal.
//! Shared container and phrase implementations preserve metadata and grow stacks.
//! Defined relations, table functions, and defined functions become callables
//! with their own frame layouts.

use std::{mem, rc::Rc};

use crate::lang::data::{
    callable::Callable,
    frame::FrameLayout,
    notation::{self, MixopArena, flat, tree},
};

use crate::lang::{
    hints::input,
    pl::{annot::Annotated, ast as pl, prepared as ast},
};

use crate::runtime::{envs::elab::TDEnv, typdef::TypeDef};

use crate::interp::shared::prepare::{Prepare, il, zipper};

use super::strip;

// - Preparation context

/// Records PL slots and interned mixops.
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

// = Annotations

impl<C, T: Prepare<C>> Prepare<C> for Annotated<T> {
    type Output = Annotated<T::Output>;

    fn prepare(self, ctx: &mut C) -> Self::Output {
        Annotated { node: self.node.prepare(ctx), hints: self.hints }
    }
}

// = Expressions

impl Prepare<PrepareContext<'_>> for pl::Exp {
    type Output = ast::Exp;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Prose hints only render, so execution prepares the IL expression
        strip::strip_exp(self).prepare(&mut ctx.il())
    }
}

/// Prepares an expression that a value binds, planning it as a pattern.
fn prepare_pattern(ctx: &mut PrepareContext<'_>, exp: pl::Exp) -> ast::Exp {
    let mut exp = exp.prepare(ctx);
    zipper::plan_pattern(&mut exp);
    exp
}

// = Parameters

impl Prepare<PrepareContext<'_>> for pl::ParamKind {
    type Output = ast::ParamKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            // The parameter is a pattern the argument binds
            pl::ParamKind::Exp(typ, exp) => {
                ast::ParamKind::Exp(typ, Box::new(prepare_pattern(ctx, *exp)))
            }
            pl::ParamKind::Def(id, tparams, params, typ) => {
                // The parameter holds its function value in a slot
                ctx.layout.resolve_func(&id);
                ast::ParamKind::Def(id, tparams, params.prepare(ctx), typ)
            }
        }
    }
}

// = Blocks

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::Block<Tier> {
    type Output = ast::Block<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::Block { instrs: self.instrs.prepare(ctx) }
    }
}

// = Holding conditions

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::HoldCase<Tier> {
    type Output = ast::HoldCase<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        match self {
            pl::HoldCase::Both(block_l, block_r) => {
                ast::HoldCase::Both(block_l.prepare(ctx), block_r.prepare(ctx))
            }
            pl::HoldCase::Hold(block, dangle) => ast::HoldCase::Hold(block.prepare(ctx), dangle),
            pl::HoldCase::NotHold(block, dangle) => {
                ast::HoldCase::NotHold(block.prepare(ctx), dangle)
            }
        }
    }
}

// = Case analysis

impl ast::Guard {
    /// Prepares a guard on a scrutinee of type `typ_scrut`.
    fn prepare(ctx: &mut PrepareContext<'_>, guard: pl::Guard, typ_scrut: &pl::Typ) -> Self {
        match guard {
            pl::Guard::Bool(cond) => ast::Guard::Bool(cond),
            pl::Guard::Cmp(op, typ, exp) => ast::Guard::Cmp(op, typ, exp.prepare(ctx)),
            pl::Guard::Sub(typ) => {
                ast::Guard::Sub(ast::Subcheck::prepare(&mut ctx.il(), typ_scrut, &typ))
            }
            // The bound expression is a pattern the scrutinee binds
            pl::Guard::LetSub(typ, exp_l) => {
                let subcheck = ast::Subcheck::prepare(&mut ctx.il(), typ_scrut, &typ);
                ast::Guard::LetSub(ast::SubBind { typ, subcheck }, prepare_pattern(ctx, exp_l))
            }
            pl::Guard::LetMatch(pattern, exp_l) => {
                ast::Guard::LetMatch(pattern.prepare(&mut ctx.il()), prepare_pattern(ctx, exp_l))
            }
            pl::Guard::Match(pattern) => ast::Guard::Match(pattern.prepare(&mut ctx.il())),
            pl::Guard::Mem(exp) => ast::Guard::Mem(exp.prepare(ctx)),
        }
    }
}

// = Instructions

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::InstrKind<Tier> {
    type Output = ast::InstrKind<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        match self {
            pl::InstrKind::If(instr) => ast::InstrKind::If(instr.prepare(ctx)),
            pl::InstrKind::IfLetSub(instr) => ast::InstrKind::IfLetSub(instr.prepare(ctx)),
            pl::InstrKind::IfLetMatch(instr) => ast::InstrKind::IfLetMatch(instr.prepare(ctx)),
            pl::InstrKind::Check(instr) => ast::InstrKind::Check(instr.prepare(ctx)),
            pl::InstrKind::Hold(instr) => ast::InstrKind::Hold(instr.prepare(ctx)),
            pl::InstrKind::Case(instr) => ast::InstrKind::Case(instr.prepare(ctx)),
            pl::InstrKind::Let(instr) => ast::InstrKind::Let(instr.prepare(ctx)),
            pl::InstrKind::Debug(instr) => ast::InstrKind::Debug(instr.prepare(ctx)),
            pl::InstrKind::Destruct(instr) => ast::InstrKind::Destruct(instr.prepare(ctx)),
            pl::InstrKind::Tier(instr) => ast::InstrKind::Tier(instr.prepare(ctx)),
        }
    }
}

// - If instruction

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::IfInstr<Tier> {
    type Output = ast::IfInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::IfInstr {
            exp: self.exp.prepare(ctx),
            iter_exps: self.iter_exps.prepare(&mut ctx.il()),
            block: self.block.prepare(ctx),
            dangle: self.dangle,
        }
    }
}

// - Conditional bindings

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>>
    for pl::IfLetSubInstr<Tier>
{
    type Output = ast::IfLetSubInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        // The check starts from the tested expression's type
        let typ_source =
            crate::phrase!(node: self.exp_r.node.note.as_ref().clone(), span: self.exp_r.node.span);
        let subcheck = ast::Subcheck::prepare(&mut ctx.il(), &typ_source, &self.target);
        ast::IfLetSubInstr {
            target: ast::SubBind { typ: self.target, subcheck },
            exp_l: prepare_pattern(ctx, self.exp_l),
            exp_r: self.exp_r.prepare(ctx),
            block: self.block.prepare(ctx),
            dangle: self.dangle,
        }
    }
}

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>>
    for pl::IfLetMatchInstr<Tier>
{
    type Output = ast::IfLetMatchInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::IfLetMatchInstr {
            pattern: self.pattern.prepare(&mut ctx.il()),
            exp_l: prepare_pattern(ctx, self.exp_l),
            exp_r: self.exp_r.prepare(ctx),
            block: self.block.prepare(ctx),
            dangle: self.dangle,
        }
    }
}

// - Check instruction

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::CheckInstr<Tier> {
    type Output = ast::CheckInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::CheckInstr {
            exp: self.exp.prepare(ctx),
            iter_exps: self.iter_exps.prepare(&mut ctx.il()),
            block: self.block.prepare(ctx),
        }
    }
}

// - Hold instruction

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::HoldInstr<Tier> {
    type Output = ast::HoldInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::HoldInstr {
            id: self.id,
            not_exp: self.not_exp.prepare(ctx),
            iter_exps: self.iter_exps.prepare(&mut ctx.il()),
            hold_case: self.hold_case.prepare(ctx),
        }
    }
}

// - Case instruction

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::CaseInstr<Tier> {
    type Output = ast::CaseInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        let exp = self.exp.prepare(ctx);
        // Guards test the scrutinee, so their checks start from its type
        let typ_scrut = crate::phrase!(node: exp.note.as_ref().clone(), span: exp.span);
        let cases = self
            .cases
            .into_iter()
            .map(|case| ast::Case {
                guard: ast::Guard::prepare(ctx, case.guard, &typ_scrut),
                block: case.block.prepare(ctx),
                fallthrough: case.fallthrough,
            })
            .collect();
        ast::CaseInstr { exp, cases, totality: self.totality, dangle: self.dangle }
    }
}

// - Let instruction

impl Prepare<PrepareContext<'_>> for pl::LetInstr {
    type Output = ast::LetInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::LetInstr {
            exp_l: prepare_pattern(ctx, self.exp_l),
            exp_r: self.exp_r.prepare(ctx),
            iter_instrs: self.iter_instrs.prepare(&mut ctx.il()),
        }
    }
}

// - Debug instruction

impl Prepare<PrepareContext<'_>> for pl::DebugInstr {
    type Output = ast::DebugInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::DebugInstr { exp: self.exp.prepare(ctx) }
    }
}

// - Destruct instruction

impl Prepare<PrepareContext<'_>> for pl::DestructInstr {
    type Output = ast::DestructInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::DestructInstr {
            bindings: self
                .bindings
                .into_iter()
                .map(|(name, exp)| (name, prepare_pattern(ctx, exp)))
                .collect(),
            exp: self.exp.prepare(ctx),
        }
    }
}

// - Tier instruction

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::TierInstr<Tier> {
    type Output = ast::TierInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::TierInstr { tier: self.tier.prepare(ctx) }
    }
}

// = Group-body tier

impl Prepare<PrepareContext<'_>> for pl::GroupInstr {
    type Output = ast::GroupInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::GroupInstr::Result(instr) => ast::GroupInstr::Result(instr.prepare(ctx)),
            pl::GroupInstr::Return(instr) => ast::GroupInstr::Return(instr.prepare(ctx)),
            pl::GroupInstr::Rule(instr) => ast::GroupInstr::Rule(instr.prepare(ctx)),
            pl::GroupInstr::Backtrack(instr) => ast::GroupInstr::Backtrack(instr.prepare(ctx)),
        }
    }
}

// - Result instruction

impl Prepare<PrepareContext<'_>> for pl::ResultInstr {
    type Output = ast::ResultInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::ResultInstr {
            rel_signature: self.rel_signature,
            exps_output: self.exps_output.prepare(ctx),
        }
    }
}

// - Return instruction

impl Prepare<PrepareContext<'_>> for pl::ReturnInstr {
    type Output = ast::ReturnInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::ReturnInstr { exp: self.exp.prepare(ctx) }
    }
}

// - Rule instruction

impl Prepare<PrepareContext<'_>> for pl::RuleInstr {
    type Output = ast::RuleInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let mut not_exp = self.not_exp.prepare(ctx);
        // The outputs are patterns the call binds
        let (_, exps_output) =
            input::split(&self.input_hint, not_exp.args_mut().iter_mut().collect())
                .expect("input hint must fit relation");
        exps_output.into_iter().for_each(zipper::plan_pattern);
        ast::RuleInstr {
            id: self.id,
            not_exp,
            input_hint: self.input_hint,
            iter_instrs: self.iter_instrs.prepare(&mut ctx.il()),
        }
    }
}

// - Backtrack instruction

impl Prepare<PrepareContext<'_>> for pl::BacktrackInstr {
    type Output = ast::BacktrackInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::BacktrackInstr { blocks: self.blocks.prepare(ctx) }
    }
}

// = Dispatch tier

impl Prepare<PrepareContext<'_>> for pl::DispatchInstr {
    type Output = ast::DispatchInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::DispatchInstr::Group(instr) => ast::DispatchInstr::Group(instr.prepare(ctx)),
            pl::DispatchInstr::Route(instr) => ast::DispatchInstr::Route(instr.prepare(ctx)),
        }
    }
}

// - Rule group instruction

impl Prepare<PrepareContext<'_>> for pl::RuleGroupInstr {
    type Output = ast::RuleGroupInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::RuleGroupInstr {
            id_rel: self.id_rel,
            id_group: self.id_group,
            rel_signature: self.rel_signature,
            exps_input: self.exps_input.prepare(ctx),
            block: self.block.prepare(ctx),
        }
    }
}

// - Route instruction

impl Prepare<PrepareContext<'_>> for pl::RouteInstr {
    type Output = ast::RouteInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::RouteInstr { blocks: self.blocks.prepare(ctx) }
    }
}

// = Table rows

impl Prepare<PrepareContext<'_>> for pl::TableRow {
    type Output = ast::TableRow;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::TableRow {
            exps_input: self.exps_input.prepare(ctx),
            exp: self.exp.prepare(ctx),
            block: self.block.prepare(ctx),
        }
    }
}

// = Relation definitions

impl Prepare<PrepareContext<'_>> for pl::RelDef {
    type Output = ast::RelDef;

    /// Prepares the relation; a defined one runs in a frame of its own.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::RelDef::Extern(rel) => {
                let rel = rel.prepare(ctx);
                *ctx.layout = FrameLayout::default();
                ast::RelDef::Extern(rel)
            }
            pl::RelDef::Defined(rel) => {
                // Prepare the relation, then transfer the layout it filled
                let def = rel.prepare(ctx);
                let layout = Rc::new(mem::take(ctx.layout));
                let rel = Callable { def, layout };
                ast::RelDef::Defined(rel)
            }
        }
    }
}

// - External relation

impl Prepare<PrepareContext<'_>> for pl::ExternRel {
    type Output = ast::ExternRel;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::ExternRel {
            id: self.id,
            rel_signature: self.rel_signature,
            exps_input: self.exps_input.prepare(ctx),
        }
    }
}

// - Defined relation

impl Prepare<PrepareContext<'_>> for pl::DefinedRel {
    type Output = ast::DefinedRel;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // The inputs are patterns the call's arguments bind
        let mut exps_input = self.exps_input.prepare(ctx);
        zipper::plan_patterns(&mut exps_input);
        ast::DefinedRel {
            id: self.id,
            rel_signature: self.rel_signature,
            exps_input,
            block: self.block.prepare(ctx),
            block_else_opt: self.block_else_opt.prepare(ctx),
        }
    }
}

// = Meta-function definitions

impl Prepare<PrepareContext<'_>> for pl::MetaFuncDef {
    type Output = ast::MetaFuncDef;

    /// Prepares the function; a table or defined one runs in a frame of its own.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::MetaFuncDef::Extern(func) => {
                let func = func.prepare(ctx);
                *ctx.layout = FrameLayout::default();
                ast::MetaFuncDef::Extern(func)
            }
            pl::MetaFuncDef::Builtin(func) => {
                let func = func.prepare(ctx);
                *ctx.layout = FrameLayout::default();
                ast::MetaFuncDef::Builtin(func)
            }
            pl::MetaFuncDef::Table(func) => {
                // Prepare the function, then transfer the layout it filled
                let def = func.prepare(ctx);
                let layout = Rc::new(mem::take(ctx.layout));
                let func = Callable { def, layout };
                ast::MetaFuncDef::Table(func)
            }
            pl::MetaFuncDef::Defined(func) => {
                let def = func.prepare(ctx);
                let layout = Rc::new(mem::take(ctx.layout));
                let func = Callable { def, layout };
                ast::MetaFuncDef::Defined(func)
            }
        }
    }
}

// - External function

impl Prepare<PrepareContext<'_>> for pl::ExternFunc {
    type Output = ast::ExternFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::ExternFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params.prepare(ctx),
            typ: self.typ,
        }
    }
}

// - Builtin function

impl Prepare<PrepareContext<'_>> for pl::BuiltinFunc {
    type Output = ast::BuiltinFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::BuiltinFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params.prepare(ctx),
            typ: self.typ,
        }
    }
}

// - Table function

impl Prepare<PrepareContext<'_>> for pl::TableFunc {
    type Output = ast::TableFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::TableFunc {
            id: self.id,
            params: self.params.prepare(ctx),
            typ: self.typ,
            rows: self.rows.prepare(ctx),
        }
    }
}

// - Defined function

impl Prepare<PrepareContext<'_>> for pl::DefinedFunc {
    type Output = ast::DefinedFunc;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Type parameters are opaque types in the body
        let mut tdenv = ctx.tdenv.clone();
        for tparam in &self.tparams {
            tdenv.insert(tparam.clone(), TypeDef::Parameter);
        }
        let ctx = &mut PrepareContext::new(&mut *ctx.arena_mixop, &tdenv, &mut *ctx.layout);
        ast::DefinedFunc {
            id: self.id,
            tparams: self.tparams,
            params: self.params.prepare(ctx),
            typ: self.typ,
            block: self.block.prepare(ctx),
            block_else_opt: self.block_else_opt.prepare(ctx),
        }
    }
}

// = Notation

impl<'a, T: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for tree::Mixfix<T> {
    type Output = flat::Mixfix<T::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        let (mixop, args) = self.into_parts();
        let mixop = mixop.prepare(ctx.arena_mixop);
        let args = args.prepare(ctx);
        notation::flat::Mixfix::new(ctx.arena_mixop, mixop, args)
            .expect("a mixfix fills every position")
    }
}
