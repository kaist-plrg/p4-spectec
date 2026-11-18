//! Load-time preparation of PL control flow and expressions
//!
//! Prosification produces annotated blocks with name-based expressions.
//! `Prepare` consumes those nodes to build executable blocks:
//! instructions retain their hints, while expressions drop theirs
//! and prepare in IL's context as IL expressions,
//! for `ExpIterFor::Assign` where a value binds them.
//! Each callable collects its frame layout during the same traversal.
//! Shared container and phrase implementations preserve metadata and grow stacks.
//! Defined relations, table functions, and defined functions become callables
//! with their own frame layouts.
//! Under `tco`, a returned call or a relation call whose outputs conclude
//! becomes a tail call in tail position (`Position`).

use std::{mem, rc::Rc};

use crate::lang::data::{callable::Callable, frame::FrameLayout, notation::MixopArena};

use crate::lang::{
    common::source::NotePhrase,
    hints::input,
    il::ast as il_source,
    pl::{annot::Annotated, ast as pl, prepared as ast},
    traits::eq::SyntaxEq,
};

use crate::runtime::{envs::elab::TDEnv, typdef::TypeDef};

use crate::interp::shared::prepare::{
    Prepare,
    il::{self, ExpIterFor},
};

// - Preparation context

/// Records PL slots and interned mixops.
pub struct PrepareContext<'a> {
    /// The specification's notation arena.
    pub arena_mixop: &'a mut MixopArena,
    /// Source type definitions, with the callable's type parameters.
    pub tdenv: &'a TDEnv,
    /// The frame layout of the unit being prepared.
    pub layout: &'a mut FrameLayout,
    /// The runner's alternative-selection mode.
    pub det: bool,
    /// Make calls in tail position as tail calls.
    pub tco: bool,
    /// Where the block or instruction being prepared sits in its callable.
    pos: Position,
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
        Self { arena_mixop, tdenv, layout, det, tco, pos: Position::NonTail }
    }

    /// Reborrows the context for a block or instruction at `pos`.
    fn at(&mut self, pos: Position) -> PrepareContext<'_> {
        PrepareContext {
            arena_mixop: &mut *self.arena_mixop,
            tdenv: self.tdenv,
            layout: &mut *self.layout,
            det: self.det,
            tco: self.tco,
            pos,
        }
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

// - Position

/// Where a block sits in its callable, for deciding tail calls, as in SL.
///
/// In tail position, nothing else in the callable can conclude
/// once the block mismatches or falls through,
/// so a call that concludes the block can be made in place of the callable.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Position {
    Tail,
    NonTail,
}

impl Position {
    /// Places one of `len` alternatives of an instruction placed here.
    ///
    /// Another alternative may conclude after one mismatches or falls through,
    /// unless it is tried last, or under deterministic choice it is the only one.
    fn alternative(self, det: bool, idx: usize, len: usize) -> Self {
        let alone = if det { len == 1 } else { idx + 1 == len };
        if alone { self } else { Position::NonTail }
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

impl Prepare<il::PrepareContext<'_>> for pl::Exp {
    type Output = ast::Exp;

    fn prepare(self, ctx: &mut il::PrepareContext<'_>) -> Self::Output {
        // Prose hints only render, so execution prepares the IL expression
        il_source::Exp::from(self).prepare(ctx)
    }
}

// = Parameters

impl Prepare<PrepareContext<'_>> for pl::ParamKind {
    type Output = ast::ParamKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            // The parameter is a pattern the argument binds
            pl::ParamKind::Exp(typ, exp) => {
                let exp = exp.prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
                ast::ParamKind::Exp(typ, exp)
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

impl Prepare<PrepareContext<'_>> for pl::DispatchBlock {
    type Output = ast::DispatchBlock;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let pos = ctx.pos;
        let len = self.instrs.len();
        let mut instrs = Vec::with_capacity(len);
        for (idx, instr) in self.instrs.into_iter().enumerate() {
            // Each instruction is placed by whether it is the block's last
            let pos_instr = if idx + 1 == len { pos } else { Position::NonTail };
            instrs.push(instr.prepare(&mut ctx.at(pos_instr)));
        }
        ast::Block { instrs }
    }
}

impl Prepare<PrepareContext<'_>> for pl::GroupBlock {
    type Output = ast::GroupBlock;

    /// Prepares the block; in tail position under `tco`, a relation call
    /// followed by a final result of its outputs becomes one result making the call.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let pos = ctx.pos;
        let mut instrs_source = self.instrs;
        let mut instr_tail = None;
        // Compare source expressions before their mixops become arena handles
        if ctx.tco
            && let [.., instr_before_final, instr_final] = instrs_source.as_slice()
            && let pl::InstrKind::Tier(pl::TierInstr { tier: pl::GroupInstr::Rule(instr_rule) }) =
                &instr_before_final.node.node
            && let pl::InstrKind::Tier(pl::TierInstr { tier: pl::GroupInstr::Result(instr_result) }) =
                &instr_final.node.node
            && instr_rule.is_tail_call(pos, instr_result)
        {
            // The call replaces the final result
            instrs_source.pop();
            let Some(Annotated { node: NotePhrase { node, note, span }, hints }) =
                instrs_source.pop()
            else {
                unreachable!("the call precedes the final result")
            };
            let pl::InstrKind::Tier(pl::TierInstr { tier: pl::GroupInstr::Rule(instr_rule) }) =
                node
            else {
                unreachable!("the call is a relation call")
            };
            instr_tail = Some((instr_rule, note, span, hints));
        }
        let len = instrs_source.len();
        let mut instrs = Vec::with_capacity(len + 1);
        for (idx, instr) in instrs_source.into_iter().enumerate() {
            // Each instruction is placed by whether it is the block's last
            let last = idx + 1 == len && instr_tail.is_none();
            let pos_instr = if last { pos } else { Position::NonTail };
            instrs.push(instr.prepare(&mut ctx.at(pos_instr)));
        }
        // The call is prepared as usual, then concludes in place of the relation with its span
        if let Some((instr_rule, note, span, hints)) = instr_tail {
            let instr_rule = instr_rule.prepare(&mut ctx.at(pos));
            let call = ast::TailRel { id: instr_rule.id, exps_input: instr_rule.exps_input };
            let tier = ast::GroupInstr::Result(ast::ResultInstr::Tail(call));
            let node = ast::InstrKind::Tier(ast::TierInstr { tier });
            instrs.push(Annotated { node: NotePhrase { node, note, span }, hints });
        }
        ast::Block { instrs }
    }
}

// = Holding conditions

impl<'a, Tier> Prepare<PrepareContext<'a>> for pl::HoldCase<Tier>
where
    Tier: Prepare<PrepareContext<'a>>,
    pl::Block<Tier>: Prepare<PrepareContext<'a>, Output = ast::Block<Tier::Output>>,
{
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
            pl::Guard::Cmp(op, typ, exp) => ast::Guard::Cmp(op, typ, exp.prepare(&mut ctx.il())),
            pl::Guard::Sub(typ) => {
                ast::Guard::Sub(ast::Subcheck::prepare(&mut ctx.il(), typ_scrut, &typ))
            }
            // The bound expression is a pattern the scrutinee binds
            pl::Guard::LetSub(typ, exp_l) => {
                let subcheck = ast::Subcheck::prepare(&mut ctx.il(), typ_scrut, &typ);
                let exp_l = exp_l.prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
                ast::Guard::LetSub(ast::SubBind { typ, subcheck }, exp_l)
            }
            // Likewise for a pattern match
            pl::Guard::LetMatch(pattern, exp_l) => {
                let pattern = pattern.prepare(&mut ctx.il());
                let exp_l = exp_l.prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
                ast::Guard::LetMatch(pattern, exp_l)
            }
            pl::Guard::Match(pattern) => ast::Guard::Match(pattern.prepare(&mut ctx.il())),
            pl::Guard::Mem(exp) => ast::Guard::Mem(exp.prepare(&mut ctx.il())),
        }
    }
}

// = Instructions

impl<'a, Tier> Prepare<PrepareContext<'a>> for pl::InstrKind<Tier>
where
    Tier: Prepare<PrepareContext<'a>>,
    pl::Block<Tier>: Prepare<PrepareContext<'a>, Output = ast::Block<Tier::Output>>,
{
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

impl<'a, Tier> Prepare<PrepareContext<'a>> for pl::IfInstr<Tier>
where
    Tier: Prepare<PrepareContext<'a>>,
    pl::Block<Tier>: Prepare<PrepareContext<'a>, Output = ast::Block<Tier::Output>>,
{
    type Output = ast::IfInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        let exp = self.exp.prepare(&mut ctx.il());
        let iter_exps = self.iter_exps.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        ast::IfInstr { exp, iter_exps, block, dangle: self.dangle }
    }
}

// - Conditional bindings

impl<'a, Tier> Prepare<PrepareContext<'a>> for pl::IfLetSubInstr<Tier>
where
    Tier: Prepare<PrepareContext<'a>>,
    pl::Block<Tier>: Prepare<PrepareContext<'a>, Output = ast::Block<Tier::Output>>,
{
    type Output = ast::IfLetSubInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        // The check starts from the tested expression's type
        let typ_source =
            crate::phrase!(node: self.exp_r.node.note.as_ref().clone(), span: self.exp_r.node.span);
        let subcheck = ast::Subcheck::prepare(&mut ctx.il(), &typ_source, &self.target);
        let target = ast::SubBind { typ: self.target, subcheck };
        // The left side is a pattern the value binds
        let exp_l = self
            .exp_l
            .prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        let exp_r = self.exp_r.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        ast::IfLetSubInstr { target, exp_l, exp_r, block, dangle: self.dangle }
    }
}

impl<'a, Tier> Prepare<PrepareContext<'a>> for pl::IfLetMatchInstr<Tier>
where
    Tier: Prepare<PrepareContext<'a>>,
    pl::Block<Tier>: Prepare<PrepareContext<'a>, Output = ast::Block<Tier::Output>>,
{
    type Output = ast::IfLetMatchInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        let pattern = self.pattern.prepare(&mut ctx.il());
        // The left side is a pattern the value binds
        let exp_l = self
            .exp_l
            .prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        let exp_r = self.exp_r.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        ast::IfLetMatchInstr { pattern, exp_l, exp_r, block, dangle: self.dangle }
    }
}

// - Check instruction

impl<'a, Tier> Prepare<PrepareContext<'a>> for pl::CheckInstr<Tier>
where
    Tier: Prepare<PrepareContext<'a>>,
    pl::Block<Tier>: Prepare<PrepareContext<'a>, Output = ast::Block<Tier::Output>>,
{
    type Output = ast::CheckInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        let exp = self.exp.prepare(&mut ctx.il());
        let iter_exps = self.iter_exps.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        ast::CheckInstr { exp, iter_exps, block }
    }
}

// - Hold instruction

impl<'a, Tier> Prepare<PrepareContext<'a>> for pl::HoldInstr<Tier>
where
    Tier: Prepare<PrepareContext<'a>>,
    pl::Block<Tier>: Prepare<PrepareContext<'a>, Output = ast::Block<Tier::Output>>,
{
    type Output = ast::HoldInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        let not_exp = self.not_exp.prepare(&mut ctx.il());
        let iter_exps = self.iter_exps.prepare(&mut ctx.il());
        let hold_case = self.hold_case.prepare(ctx);
        ast::HoldInstr { id: self.id, not_exp, iter_exps, hold_case }
    }
}

// - Case instruction

impl<'a, Tier> Prepare<PrepareContext<'a>> for pl::CaseInstr<Tier>
where
    Tier: Prepare<PrepareContext<'a>>,
    pl::Block<Tier>: Prepare<PrepareContext<'a>, Output = ast::Block<Tier::Output>>,
{
    type Output = ast::CaseInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        let exp = self.exp.prepare(&mut ctx.il());
        // Guards test the scrutinee, so their checks start from its type
        let typ_scrut = crate::phrase!(node: exp.note.as_ref().clone(), span: exp.span);
        let mut cases = Vec::with_capacity(self.cases.len());
        for case in self.cases {
            let guard = ast::Guard::prepare(ctx, case.guard, &typ_scrut);
            let block = case.block.prepare(ctx);
            cases.push(ast::Case { guard, block, fallthrough: case.fallthrough });
        }
        ast::CaseInstr { exp, cases, totality: self.totality, dangle: self.dangle }
    }
}

// - Let instruction

impl Prepare<PrepareContext<'_>> for pl::LetInstr {
    type Output = ast::LetInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // The left side is a pattern the value binds
        let exp_l = self
            .exp_l
            .prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        let exp_r = self.exp_r.prepare(&mut ctx.il());
        let iter_instrs = self.iter_instrs.prepare(&mut ctx.il());
        ast::LetInstr { exp_l, exp_r, iter_instrs }
    }
}

// - Debug instruction

impl Prepare<PrepareContext<'_>> for pl::DebugInstr {
    type Output = ast::DebugInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::DebugInstr { exp: self.exp.prepare(&mut ctx.il()) }
    }
}

// - Destruct instruction

impl Prepare<PrepareContext<'_>> for pl::DestructInstr {
    type Output = ast::DestructInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Each field binds a pattern
        let mut bindings = Vec::with_capacity(self.bindings.len());
        for (name, exp) in self.bindings {
            let exp = exp.prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
            bindings.push((name, exp));
        }
        let exp = self.exp.prepare(&mut ctx.il());
        ast::DestructInstr { bindings, exp }
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

impl pl::RuleInstr {
    /// Checks tail position and whether the next result concludes with exactly its outputs.
    fn is_tail_call(&self, pos: Position, instr_result: &pl::ResultInstr) -> bool {
        // Non-tail positions and iterated calls must retain their outputs
        if pos != Position::Tail || !self.iter_instrs.is_empty() {
            return false;
        }
        // Output and result expressions must agree in order, ignoring source spans
        let (_, exps_output) = input::split(&self.input_hint, self.not_exp.args().iter().collect())
            .expect("input hint must fit relation");
        exps_output.len() == instr_result.exps_output.len()
            && exps_output
                .iter()
                .zip(&instr_result.exps_output)
                .all(|(exp_l, exp_r)| exp_l.syntax_eq(exp_r))
    }
}

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
        ast::ResultInstr::Values(self.exps_output.prepare(&mut ctx.il()))
    }
}

// - Return instruction

impl Prepare<PrepareContext<'_>> for pl::ReturnInstr {
    type Output = ast::ReturnInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // A call returned in tail position is made in place of the function
        let tail = ctx.tco && ctx.pos == Position::Tail;
        ast::ReturnInstr::prepare(il_source::Exp::from(self.exp), &mut ctx.il(), tail)
    }
}

// - Rule instruction

impl Prepare<PrepareContext<'_>> for pl::RuleInstr {
    type Output = ast::RuleInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // The input hint separates the arguments once, before any call
        let (exps_input, exps_output) = input::split(&self.input_hint, self.not_exp.into_args())
            .expect("input hint must fit relation");
        let exps_input = exps_input.prepare(&mut ctx.il());
        // The outputs are patterns the call binds
        let exps_output = exps_output.prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        let iter_instrs = self.iter_instrs.prepare(&mut ctx.il());
        ast::RuleInstr { id: self.id, exps_input, exps_output, iter_instrs }
    }
}

// - Backtrack instruction

impl Prepare<PrepareContext<'_>> for pl::BacktrackInstr {
    type Output = ast::BacktrackInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let (pos, det, len) = (ctx.pos, ctx.det, self.blocks.len());
        let mut blocks = Vec::with_capacity(len);
        for (idx, block) in self.blocks.into_iter().enumerate() {
            // Each alternative is placed by what may still run after it
            let pos_block = pos.alternative(det, idx, len);
            blocks.push(block.prepare(&mut ctx.at(pos_block)));
        }
        ast::BacktrackInstr { blocks }
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
        let exps_input = self.exps_input.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        ast::RuleGroupInstr {
            id_rel: self.id_rel,
            id_group: self.id_group,
            rel_signature: self.rel_signature,
            exps_input,
            block,
        }
    }
}

// - Route instruction

impl Prepare<PrepareContext<'_>> for pl::RouteInstr {
    type Output = ast::RouteInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let (pos, det, len) = (ctx.pos, ctx.det, self.blocks.len());
        let mut blocks = Vec::with_capacity(len);
        for (idx, block) in self.blocks.into_iter().enumerate() {
            // Each alternative is placed by what may still run after it
            let pos_block = pos.alternative(det, idx, len);
            blocks.push(block.prepare(&mut ctx.at(pos_block)));
        }
        ast::RouteInstr { blocks }
    }
}

// = Table rows

impl Prepare<PrepareContext<'_>> for pl::TableRow {
    type Output = ast::TableRow;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let exps_input = self.exps_input.prepare(&mut ctx.il());
        let exp = self.exp.prepare(&mut ctx.il());
        let block = self.block.prepare(ctx);
        ast::TableRow { exps_input, exp, block }
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
            exps_input: self.exps_input.prepare(&mut ctx.il()),
        }
    }
}

// - Defined relation

impl Prepare<PrepareContext<'_>> for pl::DefinedRel {
    type Output = ast::DefinedRel;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // The inputs are patterns the call's arguments bind
        let exps_input = self
            .exps_input
            .prepare(&mut ctx.il_exp_iter_for(ExpIterFor::Assign));
        // An otherwise block may still run after the body
        let pos = if self.block_else_opt.is_some() { Position::NonTail } else { Position::Tail };
        let block = self.block.prepare(&mut ctx.at(pos));
        let block_else_opt = self.block_else_opt.prepare(&mut ctx.at(Position::Tail));
        ast::DefinedRel {
            id: self.id,
            rel_signature: self.rel_signature,
            exps_input,
            block,
            block_else_opt,
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
        let params = self.params.prepare(ctx);
        // Rows are mutually exclusive, as algorithmic conversion checks,
        // so no other row can conclude after a row that applies
        let mut rows = Vec::with_capacity(self.rows.len());
        for row in self.rows {
            rows.push(row.prepare(&mut ctx.at(Position::Tail)));
        }
        ast::TableFunc { id: self.id, params, typ: self.typ, rows }
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
        let ctx = &mut PrepareContext::new(
            &mut *ctx.arena_mixop,
            &tdenv,
            &mut *ctx.layout,
            ctx.det,
            ctx.tco,
        );
        let params = self.params.prepare(ctx);
        // An otherwise block may still run after the body
        let pos = if self.block_else_opt.is_some() { Position::NonTail } else { Position::Tail };
        let block = self.block.prepare(&mut ctx.at(pos));
        let block_else_opt = self.block_else_opt.prepare(&mut ctx.at(Position::Tail));
        ast::DefinedFunc {
            id: self.id,
            tparams: self.tparams,
            params,
            typ: self.typ,
            block,
            block_else_opt,
        }
    }
}
