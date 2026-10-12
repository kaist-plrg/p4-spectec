//! Resolve IL syntax to slots and interned mixops
//!
//! The `Prepare` implementations rebuild source nodes as prepared syntax.
//! Iterations register the outer variable `x*` for every iterated `x`,
//! so evaluation can find its slot.
//! Type definitions prepare their mixops without allocating frame slots.

use crate::lang::{
    common::Id,
    data::{
        notation::{self, MixopArena, flat, tree},
        var::{IdSlot, Var, VarSlot},
    },
};

use crate::lang::il::{ast as source, prepared::*};

use crate::runtime::envs::interp::shared::frame::FrameLayout;

use super::{Prepare, mixop};

// - Preparation context

/// Records IL slots and interned mixops.
pub struct PrepareContext<'a> {
    /// The specification's notation arena.
    pub arena_mixop: &'a mut MixopArena,
    /// The callable's frame layout.
    pub layout: &'a mut FrameLayout,
}

impl PrepareContext<'_> {
    /// Registers the outer variable of every iterated variable.
    fn prepare_outer_vars(&mut self, vars: &[VarSlot], iter: Iter) {
        for var in vars {
            let mut var_outer = var.var.clone();
            var_outer.iters.push(iter);
            self.layout.resolve_var(var_outer);
        }
    }
}

// - Names

impl Prepare<PrepareContext<'_>> for Id {
    type Output = IdSlot;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ctx.layout.resolve_id(self)
    }
}

impl Prepare<PrepareContext<'_>> for Var {
    type Output = VarSlot;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ctx.layout.resolve_var(self)
    }
}

// - Type definitions

impl Prepare<mixop::PrepareContext> for source::DefTypKind {
    type Output = DefTypKind;

    fn prepare(self, arena_mixop: &mut mixop::PrepareContext) -> Self::Output {
        match self {
            source::DefTypKind::Plain(typ) => DefTypKind::Plain(typ),
            source::DefTypKind::Struct(typ_fields) => DefTypKind::Struct(typ_fields),
            source::DefTypKind::Variant(typ_cases) => {
                DefTypKind::Variant(typ_cases.prepare(arena_mixop))
            }
        }
    }
}

impl Prepare<mixop::PrepareContext> for source::TypCase {
    type Output = TypCase;

    fn prepare(self, arena_mixop: &mut mixop::PrepareContext) -> Self::Output {
        let source::TypCase { not_typ, typ_origin, hints } = self;
        let (mixop, typs) = not_typ.node.into_parts();
        let mixop = mixop.prepare(arena_mixop);
        let not_typ_kind = notation::flat::Mixfix::new(arena_mixop, mixop, typs)
            .expect("a notation type fills every position");
        TypCase {
            not_typ: crate::phrase!(node: not_typ_kind, span: not_typ.span),
            typ_origin,
            hints,
        }
    }
}

// - Patterns and subtype checks

impl Prepare<PrepareContext<'_>> for source::Pattern {
    type Output = Pattern;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::Pattern::Case(mixop) => Pattern::Case(mixop.prepare(ctx.arena_mixop)),
            source::Pattern::List(pattern) => Pattern::List(pattern),
            source::Pattern::Opt(pattern) => Pattern::Opt(pattern),
        }
    }
}

impl Prepare<PrepareContext<'_>> for source::Subcheck {
    type Output = Subcheck;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::Subcheck::Skip => Subcheck::Skip,
            source::Subcheck::Mixop(mixops) => Subcheck::Mixop(mixops.prepare(ctx.arena_mixop)),
            source::Subcheck::Tuple(subchecks) => Subcheck::Tuple(subchecks.prepare(ctx)),
            source::Subcheck::Iter(iter, subcheck) => Subcheck::Iter(iter, subcheck.prepare(ctx)),
            source::Subcheck::Recurse(typ) => Subcheck::Recurse(typ),
        }
    }
}

// - Expressions

impl Prepare<PrepareContext<'_>> for source::ExpKind {
    type Output = ExpKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::ExpKind::Bool(value_inner) => ExpKind::Bool(value_inner),
            source::ExpKind::Num(num_inner) => ExpKind::Num(num_inner),
            source::ExpKind::Text(text_inner) => ExpKind::Text(text_inner),
            source::ExpKind::Id(id_inner) => ExpKind::Id(id_inner.prepare(ctx)),
            source::ExpKind::Un(op_inner, typ_op_inner, exp_inner) => {
                ExpKind::Un(op_inner, typ_op_inner, exp_inner.prepare(ctx))
            }
            source::ExpKind::Bin(op, typ_op, exp_l, exp_r) => {
                ExpKind::Bin(op, typ_op, exp_l.prepare(ctx), exp_r.prepare(ctx))
            }
            source::ExpKind::Cmp(op, typ_op, exp_l, exp_r) => {
                ExpKind::Cmp(op, typ_op, exp_l.prepare(ctx), exp_r.prepare(ctx))
            }
            source::ExpKind::UpCast(typ_inner, exp_inner) => {
                ExpKind::UpCast(typ_inner, exp_inner.prepare(ctx))
            }
            source::ExpKind::DownCast(typ_inner, exp_inner) => {
                ExpKind::DownCast(typ_inner, exp_inner.prepare(ctx))
            }
            source::ExpKind::Sub(exp_inner, typ_inner, check_inner) => {
                ExpKind::Sub(exp_inner.prepare(ctx), typ_inner, check_inner.prepare(ctx))
            }
            source::ExpKind::Match(exp_inner, pattern_inner) => {
                ExpKind::Match(exp_inner.prepare(ctx), pattern_inner.prepare(ctx))
            }
            source::ExpKind::Tuple(exps_inner) => ExpKind::Tuple(exps_inner.prepare(ctx)),
            source::ExpKind::Case(not_exp_inner) => ExpKind::Case(not_exp_inner.prepare(ctx)),
            source::ExpKind::Str(exp_fields_inner) => ExpKind::Str(exp_fields_inner.prepare(ctx)),
            source::ExpKind::Opt(exp_opt_inner) => ExpKind::Opt(exp_opt_inner.prepare(ctx)),
            source::ExpKind::List(exps_inner) => ExpKind::List(exps_inner.prepare(ctx)),
            source::ExpKind::Cons(exp_head, exp_tail) => {
                ExpKind::Cons(exp_head.prepare(ctx), exp_tail.prepare(ctx))
            }
            source::ExpKind::Cat(exp_l, exp_r) => {
                ExpKind::Cat(exp_l.prepare(ctx), exp_r.prepare(ctx))
            }
            source::ExpKind::Mem(exp_elem, exp_list) => {
                ExpKind::Mem(exp_elem.prepare(ctx), exp_list.prepare(ctx))
            }
            source::ExpKind::Len(exp_inner) => ExpKind::Len(exp_inner.prepare(ctx)),
            source::ExpKind::Dot(exp_inner, atom_inner) => {
                ExpKind::Dot(exp_inner.prepare(ctx), atom_inner)
            }
            source::ExpKind::Idx(exp_base, exp_idx) => {
                ExpKind::Idx(exp_base.prepare(ctx), exp_idx.prepare(ctx))
            }
            source::ExpKind::Slice(exp_base, exp_idx, exp_len) => {
                ExpKind::Slice(exp_base.prepare(ctx), exp_idx.prepare(ctx), exp_len.prepare(ctx))
            }
            source::ExpKind::Upd(exp_base, path_inner, exp_new) => {
                ExpKind::Upd(exp_base.prepare(ctx), path_inner.prepare(ctx), exp_new.prepare(ctx))
            }
            source::ExpKind::Call(id_inner, targs_inner, args_inner) => {
                ExpKind::Call(id_inner, targs_inner, args_inner.prepare(ctx))
            }
            source::ExpKind::Iter(exp_inner, exp_iter_inner) => {
                let exp_inner = exp_inner.prepare(ctx);
                let exp_iter_inner = exp_iter_inner.prepare(ctx);
                ExpKind::Iter(exp_inner, exp_iter_inner)
            }
        }
    }
}

impl Prepare<PrepareContext<'_>> for source::ExpField {
    type Output = ExpField;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ExpField { atom: self.atom, exp: self.exp.prepare(ctx) }
    }
}

impl Prepare<PrepareContext<'_>> for source::ExpIter {
    type Output = ExpIter;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        let source::ExpIter { iter, vars } = self;
        // Register `x*` for every iterated `x` so its slot exists
        let vars = vars.prepare(ctx);
        ctx.prepare_outer_vars(&vars, iter);
        ExpIter { iter, vars }
    }
}

// - Paths

impl Prepare<PrepareContext<'_>> for source::PathKind {
    type Output = PathKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::PathKind::Root => PathKind::Root,
            source::PathKind::Idx(path_base, exp_idx) => {
                PathKind::Idx(path_base.prepare(ctx), exp_idx.prepare(ctx))
            }
            source::PathKind::Slice(path_base, exp_idx, exp_len) => {
                PathKind::Slice(path_base.prepare(ctx), exp_idx.prepare(ctx), exp_len.prepare(ctx))
            }
            source::PathKind::Dot(path_inner, atom_inner) => {
                PathKind::Dot(path_inner.prepare(ctx), atom_inner)
            }
        }
    }
}

// - Arguments

impl Prepare<PrepareContext<'_>> for source::ArgKind {
    type Output = ArgKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            source::ArgKind::Exp(exp_inner) => ArgKind::Exp(exp_inner.prepare(ctx)),
            source::ArgKind::Def(id_inner) => ArgKind::Def(id_inner),
        }
    }
}

// - Premises

impl Prepare<PrepareContext<'_>> for source::PremIter {
    type Output = PremIter;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        // Register the outer variables of the bound and binding variables
        let vars_bound = self.vars_bound.prepare(ctx);
        let vars_bind = self.vars_bind.prepare(ctx);
        ctx.prepare_outer_vars(&vars_bound, self.iter);
        ctx.prepare_outer_vars(&vars_bind, self.iter);
        PremIter { iter: self.iter, vars_bound, vars_bind }
    }
}

// - Notation

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
