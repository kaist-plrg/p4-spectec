//! Load-time preparation of PL control flow and expressions
//!
//! Prosification produces annotated blocks with name-based expressions.
//! `Prepare` consumes those nodes to build executable blocks:
//! expressions retain PL syntax and hints while identifiers resolve to slots.
//! Each callable collects its frame layout during the same traversal.
//! Shared container and phrase implementations preserve metadata and grow stacks.

use crate::lang::pl::{annot::Annotated, ast as pl, prepared as ast};

use crate::interp::shared::prepare::{Prepare, PrepareContext};

// = Annotations

impl<T: Prepare> Prepare for Annotated<T> {
    type Output = Annotated<T::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        Annotated { node: self.node.prepare(ctx), hints: self.hints }
    }
}

// = Expressions

impl Prepare for pl::ExpKind {
    type Output = ast::ExpKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        use ast::ExpKind as E;
        use pl::ExpKind as P;
        match self {
            P::Bool(value) => E::Bool(value),
            P::Num(num) => E::Num(num),
            P::Text(text) => E::Text(text),
            P::Id(id) => E::Id(id.prepare(ctx)),
            P::Un(op, typ, exp) => E::Un(op, typ, exp.prepare(ctx)),
            P::Bin(op, typ, exp_l, exp_r) => {
                E::Bin(op, typ, exp_l.prepare(ctx), exp_r.prepare(ctx))
            }
            P::Cmp(op, typ, exp_l, exp_r) => {
                E::Cmp(op, typ, exp_l.prepare(ctx), exp_r.prepare(ctx))
            }
            P::UpCast(typ, exp) => E::UpCast(typ, exp.prepare(ctx)),
            P::DownCast(typ, exp) => E::DownCast(typ, exp.prepare(ctx)),
            P::Sub(exp, typ, check) => E::Sub(exp.prepare(ctx), typ, check.prepare(ctx)),
            P::Match(exp, pattern) => E::Match(exp.prepare(ctx), pattern.prepare(ctx)),
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
            P::Iter(exp, iter) => E::Iter(exp.prepare(ctx), iter.prepare(ctx)),
        }
    }
}

// = Paths

impl Prepare for pl::PathKind {
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

impl Prepare for pl::ArgKind {
    type Output = ast::ArgKind;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::ArgKind::Exp(exp) => ast::ArgKind::Exp(exp.prepare(ctx)),
            pl::ArgKind::Def(id) => ast::ArgKind::Def(id),
        }
    }
}

// = Parameters

impl Prepare for pl::ParamKind {
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

impl<Tier: Prepare> Prepare for pl::HoldCase<Tier> {
    type Output = ast::HoldCase<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
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

impl Prepare for pl::Guard {
    type Output = ast::Guard;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::Guard::Bool(cond) => ast::Guard::Bool(cond),
            pl::Guard::Cmp(op, typ, exp) => ast::Guard::Cmp(op, typ, exp.prepare(ctx)),
            pl::Guard::Sub(typ, check) => ast::Guard::Sub(typ, check.prepare(ctx)),
            pl::Guard::Match(pattern) => ast::Guard::Match(pattern.prepare(ctx)),
            pl::Guard::Mem(exp) => ast::Guard::Mem(exp.prepare(ctx)),
            pl::Guard::CheckLetSub(typ, check, exp) => {
                ast::Guard::CheckLetSub(typ, check.prepare(ctx), exp.prepare(ctx))
            }
            pl::Guard::CheckLetMatch(pattern, exp) => {
                ast::Guard::CheckLetMatch(pattern.prepare(ctx), exp.prepare(ctx))
            }
        }
    }
}

impl<Tier: Prepare> Prepare for pl::Case<Tier> {
    type Output = ast::Case<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        ast::Case { guard: self.guard.prepare(ctx), block: self.block.prepare(ctx) }
    }
}

// = Instructions

impl<Tier: Prepare> Prepare for pl::InstrKind<Tier> {
    type Output = ast::InstrKind<Tier::Output>;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::InstrKind::If(instr) => ast::InstrKind::If(ast::IfInstr {
                exp: instr.exp.prepare(ctx),
                iter_exps: instr.iter_exps.prepare(ctx),
                block: instr.block.prepare(ctx),
                dangle: instr.dangle,
            }),
            pl::InstrKind::Hold(instr) => ast::InstrKind::Hold(ast::HoldInstr {
                id: instr.id,
                not_exp: instr.not_exp.prepare(ctx),
                iter_exps: instr.iter_exps.prepare(ctx),
                hold_case: instr.hold_case.prepare(ctx),
            }),
            pl::InstrKind::Case(instr) => ast::InstrKind::Case(ast::CaseInstr {
                exp: instr.exp.prepare(ctx),
                cases: instr.cases.prepare(ctx),
                dangle: instr.dangle,
            }),
            pl::InstrKind::Let(instr) => ast::InstrKind::Let(ast::LetInstr {
                exp_l: instr.exp_l.prepare(ctx),
                exp_r: instr.exp_r.prepare(ctx),
                iter_instrs: instr.iter_instrs.prepare(ctx),
            }),
            pl::InstrKind::Debug(instr) => {
                ast::InstrKind::Debug(ast::DebugInstr { exp: instr.exp.prepare(ctx) })
            }
            pl::InstrKind::Destruct(instr) => ast::InstrKind::Destruct(ast::DestructInstr {
                bindings: instr
                    .bindings
                    .into_iter()
                    .map(|(name, exp)| (name, exp.prepare(ctx)))
                    .collect(),
                exp: instr.exp.prepare(ctx),
            }),
            pl::InstrKind::CheckLetSub(instr) => {
                ast::InstrKind::CheckLetSub(ast::CheckLetSubInstr {
                    typ: instr.typ,
                    subcheck: instr.subcheck.prepare(ctx),
                    exp_l: instr.exp_l.prepare(ctx),
                    exp_r: instr.exp_r.prepare(ctx),
                    block: instr.block.prepare(ctx),
                })
            }
            pl::InstrKind::CheckLetMatch(instr) => {
                ast::InstrKind::CheckLetMatch(ast::CheckLetMatchInstr {
                    pattern: instr.pattern.prepare(ctx),
                    exp_l: instr.exp_l.prepare(ctx),
                    exp_r: instr.exp_r.prepare(ctx),
                    block: instr.block.prepare(ctx),
                })
            }
            pl::InstrKind::OptionGet(instr) => ast::InstrKind::OptionGet(ast::OptionGetInstr {
                exp_l: instr.exp_l.prepare(ctx),
                exp_r: instr.exp_r.prepare(ctx),
                block: instr.block.prepare(ctx),
            }),
            pl::InstrKind::Tier(instr) => {
                ast::InstrKind::Tier(ast::TierInstr { tier: instr.tier.prepare(ctx) })
            }
        }
    }
}

// = Group-body tier

impl Prepare for pl::GroupInstr {
    type Output = ast::GroupInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::GroupInstr::Result(instr) => ast::GroupInstr::Result(ast::ResultInstr {
                rel_signature: instr.rel_signature,
                exps_output: instr.exps_output.prepare(ctx),
            }),
            pl::GroupInstr::Return(instr) => {
                ast::GroupInstr::Return(ast::ReturnInstr { exp: instr.exp.prepare(ctx) })
            }
            pl::GroupInstr::Rule(instr) => ast::GroupInstr::Rule(ast::RuleInstr {
                id: instr.id,
                not_exp: instr.not_exp.prepare(ctx),
                input_hint: instr.input_hint,
                iter_instrs: instr.iter_instrs.prepare(ctx),
            }),
            pl::GroupInstr::Backtrack(instr) => ast::GroupInstr::Backtrack(ast::BacktrackInstr {
                blocks: instr.blocks.prepare(ctx),
            }),
        }
    }
}

// = Dispatch tier

impl Prepare for pl::DispatchInstr {
    type Output = ast::DispatchInstr;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::DispatchInstr::Group(instr) => ast::DispatchInstr::Group(ast::RuleGroupInstr {
                id_rel: instr.id_rel,
                id_group: instr.id_group,
                rel_signature: instr.rel_signature,
                exps_input: instr.exps_input.prepare(ctx),
                block: instr.block.prepare(ctx),
            }),
            pl::DispatchInstr::Route(instr) => {
                ast::DispatchInstr::Route(ast::RouteInstr { blocks: instr.blocks.prepare(ctx) })
            }
        }
    }
}

// = Table rows

impl Prepare for pl::TableRow {
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

impl Prepare for pl::RelDef {
    type Output = ast::RelDef;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::RelDef::Extern(rel) => ast::RelDef::Extern(ast::ExternRel {
                id: rel.id,
                rel_signature: rel.rel_signature,
                exps_input: rel.exps_input.prepare(ctx),
            }),
            pl::RelDef::Defined(rel) => ast::RelDef::Defined(ast::DefinedRel {
                id: rel.id,
                rel_signature: rel.rel_signature,
                exps_input: rel.exps_input.prepare(ctx),
                block: rel.block.prepare(ctx),
                block_else_opt: rel.block_else_opt.prepare(ctx),
            }),
        }
    }
}

// = Meta-function definitions

impl Prepare for pl::MetaFuncDef {
    type Output = ast::MetaFuncDef;

    fn prepare(self, ctx: &mut PrepareContext<'_>) -> Self::Output {
        match self {
            pl::MetaFuncDef::Extern(func) => ast::MetaFuncDef::Extern(ast::ExternFunc {
                id: func.id,
                tparams: func.tparams,
                params: func.params.prepare(ctx),
                typ: func.typ,
            }),
            pl::MetaFuncDef::Builtin(func) => ast::MetaFuncDef::Builtin(ast::BuiltinFunc {
                id: func.id,
                tparams: func.tparams,
                params: func.params.prepare(ctx),
                typ: func.typ,
            }),
            pl::MetaFuncDef::Table(func) => ast::MetaFuncDef::Table(ast::TableFunc {
                id: func.id,
                params: func.params.prepare(ctx),
                typ: func.typ,
                rows: func.rows.prepare(ctx),
            }),
            pl::MetaFuncDef::Defined(func) => ast::MetaFuncDef::Defined(ast::DefinedFunc {
                id: func.id,
                tparams: func.tparams,
                params: func.params.prepare(ctx),
                typ: func.typ,
                block: func.block.prepare(ctx),
                block_else_opt: func.block_else_opt.prepare(ctx),
            }),
        }
    }
}
