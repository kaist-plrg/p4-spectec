//! Load-time preparation of PL control flow and expressions
//!
//! Prosification produces annotated blocks with name-based expressions.
//! `Prepare` consumes those nodes to build executable blocks:
//! expressions retain PL syntax and hints while identifiers resolve to slots.
//! Each callable collects its frame layout during the same traversal.
//! Shared container and phrase implementations preserve metadata and grow stacks.
//! Top-level relations and functions return callables with their own layouts.

use std::{mem, rc::Rc};

use crate::lang::data::notation::{self, MixopArena, flat, tree};

use crate::lang::pl::{annot::Annotated, ast as pl, prepared as ast};

use crate::runtime::envs::interp::shared::{callable::Callable, frame::FrameLayout};

use crate::interp::shared::prepare::{Prepare, il};

// - Preparation context

/// Records PL slots and interned mixops.
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

// = Annotations

impl<C, T: Prepare<C>> Prepare<C> for Annotated<T> {
    type Output = Annotated<T::Output>;

    fn prepare(self, ctx: &mut C) -> Self::Output {
        Annotated { node: self.node.prepare(ctx), hints: self.hints }
    }
}

// = Expressions

impl Prepare<PrepareContext<'_>> for pl::ExpKind {
    type Output = ast::ExpKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        use ast::ExpKind as E;
        use pl::ExpKind as P;
        match self {
            P::Bool(value) => E::Bool(value),
            P::Num(num) => E::Num(num),
            P::Text(text) => E::Text(text),
            P::Id(id) => E::Id(id.prepare(&mut ctx.il())),
            P::Un(op, typ, exp) => E::Un(op, typ, exp.prepare(ctx)),
            P::Bin(op, typ, exp_l, exp_r) => {
                E::Bin(op, typ, exp_l.prepare(ctx), exp_r.prepare(ctx))
            }
            P::Cmp(op, typ, exp_l, exp_r) => {
                E::Cmp(op, typ, exp_l.prepare(ctx), exp_r.prepare(ctx))
            }
            P::UpCast(typ, exp) => E::UpCast(typ, exp.prepare(ctx)),
            P::DownCast(typ, exp) => E::DownCast(typ, exp.prepare(ctx)),
            P::Sub(exp, typ, check) => E::Sub(exp.prepare(ctx), typ, check.prepare(&mut ctx.il())),
            P::Match(exp, pattern) => E::Match(exp.prepare(ctx), pattern.prepare(&mut ctx.il())),
            P::Tuple(exps) => E::Tuple(exps.prepare(ctx)),
            P::Case(not_exp) => E::Case(not_exp.prepare(ctx)),
            P::Str(fields) => E::Str(
                fields
                    .into_iter()
                    .map(|(atom, exp)| (atom, exp.prepare(ctx)))
                    .collect(),
            ),
            P::Opt(exp) => E::Opt(exp.prepare(ctx)),
            P::List(exps) => E::List(exps.prepare(ctx)),
            P::Cons(exp_head, exp_tail) => E::Cons(exp_head.prepare(ctx), exp_tail.prepare(ctx)),
            P::Cat(exp_l, exp_r) => E::Cat(exp_l.prepare(ctx), exp_r.prepare(ctx)),
            P::Mem(exp_elem, exp_list) => E::Mem(exp_elem.prepare(ctx), exp_list.prepare(ctx)),
            P::Len(exp) => E::Len(exp.prepare(ctx)),
            P::Dot(exp, atom) => E::Dot(exp.prepare(ctx), atom),
            P::Idx(exp_base, exp_idx) => E::Idx(exp_base.prepare(ctx), exp_idx.prepare(ctx)),
            P::Slice(exp_base, exp_idx, exp_len) => {
                E::Slice(exp_base.prepare(ctx), exp_idx.prepare(ctx), exp_len.prepare(ctx))
            }
            P::Upd(exp_base, path, exp_new) => {
                E::Upd(exp_base.prepare(ctx), path.prepare(ctx), exp_new.prepare(ctx))
            }
            P::Call(id, targs, args) => E::Call(id, targs, args.prepare(ctx)),
            P::Iter(exp, iter) => E::Iter(exp.prepare(ctx), iter.prepare(&mut ctx.il())),
        }
    }
}

// = Paths

impl Prepare<PrepareContext<'_>> for pl::PathKind {
    type Output = ast::PathKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::PathKind::Root => ast::PathKind::Root,
            pl::PathKind::Idx(path, exp) => ast::PathKind::Idx(path.prepare(ctx), exp.prepare(ctx)),
            pl::PathKind::Slice(path, exp_idx, exp_len) => {
                ast::PathKind::Slice(path.prepare(ctx), exp_idx.prepare(ctx), exp_len.prepare(ctx))
            }
            pl::PathKind::Dot(path, atom) => ast::PathKind::Dot(path.prepare(ctx), atom),
        }
    }
}

// = Arguments

impl Prepare<PrepareContext<'_>> for pl::ArgKind {
    type Output = ast::ArgKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::ArgKind::Exp(exp) => ast::ArgKind::Exp(exp.prepare(ctx)),
            pl::ArgKind::Def(id) => ast::ArgKind::Def(id),
        }
    }
}

// = Parameters

impl Prepare<PrepareContext<'_>> for pl::ParamKind {
    type Output = ast::ParamKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::ParamKind::Exp(typ, exp) => ast::ParamKind::Exp(typ, exp.prepare(ctx)),
            pl::ParamKind::Def(id, tparams, params, typ) => {
                ast::ParamKind::Def(id, tparams, params.prepare(ctx), typ)
            }
        }
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

impl Prepare<PrepareContext<'_>> for pl::Guard {
    type Output = ast::Guard;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::Guard::Bool(cond) => ast::Guard::Bool(cond),
            pl::Guard::Cmp(op, typ, exp) => ast::Guard::Cmp(op, typ, exp.prepare(ctx)),
            pl::Guard::Sub(typ, check) => ast::Guard::Sub(typ, check.prepare(&mut ctx.il())),
            pl::Guard::Match(pattern) => ast::Guard::Match(pattern.prepare(&mut ctx.il())),
            pl::Guard::Mem(exp) => ast::Guard::Mem(exp.prepare(ctx)),
            pl::Guard::CheckLetSub(typ, check, exp) => {
                ast::Guard::CheckLetSub(typ, check.prepare(&mut ctx.il()), exp.prepare(ctx))
            }
            pl::Guard::CheckLetMatch(pattern, exp) => {
                ast::Guard::CheckLetMatch(pattern.prepare(&mut ctx.il()), exp.prepare(ctx))
            }
        }
    }
}

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::Case<Tier> {
    type Output = ast::Case<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::Case { guard: self.guard.prepare(ctx), block: self.block.prepare(ctx) }
    }
}

// = Instructions

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>> for pl::InstrKind<Tier> {
    type Output = ast::InstrKind<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        match self {
            pl::InstrKind::If(instr) => ast::InstrKind::If(instr.prepare(ctx)),
            pl::InstrKind::Hold(instr) => ast::InstrKind::Hold(instr.prepare(ctx)),
            pl::InstrKind::Case(instr) => ast::InstrKind::Case(instr.prepare(ctx)),
            pl::InstrKind::Let(instr) => ast::InstrKind::Let(instr.prepare(ctx)),
            pl::InstrKind::Debug(instr) => ast::InstrKind::Debug(instr.prepare(ctx)),
            pl::InstrKind::Destruct(instr) => ast::InstrKind::Destruct(instr.prepare(ctx)),
            pl::InstrKind::CheckLetSub(instr) => ast::InstrKind::CheckLetSub(instr.prepare(ctx)),
            pl::InstrKind::CheckLetMatch(instr) => {
                ast::InstrKind::CheckLetMatch(instr.prepare(ctx))
            }
            pl::InstrKind::OptionGet(instr) => ast::InstrKind::OptionGet(instr.prepare(ctx)),
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
        ast::CaseInstr {
            exp: self.exp.prepare(ctx),
            cases: self.cases.prepare(ctx),
            dangle: self.dangle,
        }
    }
}

// - Let instruction

impl Prepare<PrepareContext<'_>> for pl::LetInstr {
    type Output = ast::LetInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::LetInstr {
            exp_l: self.exp_l.prepare(ctx),
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
                .map(|(name, exp)| (name, exp.prepare(ctx)))
                .collect(),
            exp: self.exp.prepare(ctx),
        }
    }
}

// - Check let sub instruction

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>>
    for pl::CheckLetSubInstr<Tier>
{
    type Output = ast::CheckLetSubInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::CheckLetSubInstr {
            typ: self.typ,
            subcheck: self.subcheck.prepare(&mut ctx.il()),
            exp_l: self.exp_l.prepare(ctx),
            exp_r: self.exp_r.prepare(ctx),
            block: self.block.prepare(ctx),
        }
    }
}

// - Check let match instruction

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>>
    for pl::CheckLetMatchInstr<Tier>
{
    type Output = ast::CheckLetMatchInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::CheckLetMatchInstr {
            pattern: self.pattern.prepare(&mut ctx.il()),
            exp_l: self.exp_l.prepare(ctx),
            exp_r: self.exp_r.prepare(ctx),
            block: self.block.prepare(ctx),
        }
    }
}

// - Option get instruction

impl<'a, Tier: Prepare<PrepareContext<'a>>> Prepare<PrepareContext<'a>>
    for pl::OptionGetInstr<Tier>
{
    type Output = ast::OptionGetInstr<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'a>) -> Self::Output {
        ast::OptionGetInstr {
            exp_l: self.exp_l.prepare(ctx),
            exp_r: self.exp_r.prepare(ctx),
            block: self.block.prepare(ctx),
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
        ast::RuleInstr {
            id: self.id,
            not_exp: self.not_exp.prepare(ctx),
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
    type Output = Callable<ast::RelDef>;

    /// Prepares the relation and transfers its callable layout.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let rel = match self {
            pl::RelDef::Extern(rel) => {
                let rel = rel.prepare(ctx);
                ast::RelDef::Extern(rel)
            }
            pl::RelDef::Defined(rel) => {
                let rel = rel.prepare(ctx);
                ast::RelDef::Defined(rel)
            }
        };

        // Transfer this callable's layout and leave the next one empty
        let layout = mem::take(ctx.layout);
        let layout = Rc::new(layout);
        Callable { def: rel, layout }
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
        ast::DefinedRel {
            id: self.id,
            rel_signature: self.rel_signature,
            exps_input: self.exps_input.prepare(ctx),
            block: self.block.prepare(ctx),
            block_else_opt: self.block_else_opt.prepare(ctx),
        }
    }
}

// = Meta-function definitions

impl Prepare<PrepareContext<'_>> for pl::MetaFuncDef {
    type Output = Callable<ast::MetaFuncDef>;

    /// Prepares the function and transfers its callable layout.
    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let func = match self {
            pl::MetaFuncDef::Extern(func) => {
                let func = func.prepare(ctx);
                ast::MetaFuncDef::Extern(func)
            }
            pl::MetaFuncDef::Builtin(func) => {
                let func = func.prepare(ctx);
                ast::MetaFuncDef::Builtin(func)
            }
            pl::MetaFuncDef::Table(func) => {
                let func = func.prepare(ctx);
                ast::MetaFuncDef::Table(func)
            }
            pl::MetaFuncDef::Defined(func) => {
                let func = func.prepare(ctx);
                ast::MetaFuncDef::Defined(func)
            }
        };

        // Transfer this callable's layout and leave the next one empty
        let layout = mem::take(ctx.layout);
        let layout = Rc::new(layout);
        Callable { def: func, layout }
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
