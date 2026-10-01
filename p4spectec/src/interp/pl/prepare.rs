//! Load-time preparation of PL control flow and expressions
//!
//! Prosification produces annotated blocks with name-based expressions.
//! `Prepare` consumes those nodes to build executable blocks:
//! expressions retain PL syntax and hints while identifiers resolve to slots.
//! Each callable collects its frame layout during the same traversal.
//! Shared container and phrase implementations preserve metadata and grow stacks.

use crate::lang::pl::{annot::Annotated, ast as pl};

use crate::runtime::envs::interp::{pl::ast_prepared as ast, shared::frame::FrameLayout};

use crate::interp::shared::prepare::Prepare;

// = Annotations

impl<T: Prepare> Prepare for Annotated<T> {
    type Output = Annotated<T::Output>;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        Annotated { node: self.node.prepare(layout), hints: self.hints }
    }
}

// = Expressions

impl Prepare for pl::ExpKind {
    type Output = ast::ExpKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        use ast::ExpKind as E;
        use pl::ExpKind as P;
        match self {
            P::Bool(value) => E::Bool(value),
            P::Num(num) => E::Num(num),
            P::Text(text) => E::Text(text),
            P::Id(id) => E::Id(id.prepare(layout)),
            P::Un(op, typ, exp) => E::Un(op, typ, exp.prepare(layout)),
            P::Bin(op, typ, exp_l, exp_r) => {
                E::Bin(op, typ, exp_l.prepare(layout), exp_r.prepare(layout))
            }
            P::Cmp(op, typ, exp_l, exp_r) => {
                E::Cmp(op, typ, exp_l.prepare(layout), exp_r.prepare(layout))
            }
            P::UpCast(typ, exp) => E::UpCast(typ, exp.prepare(layout)),
            P::DownCast(typ, exp) => E::DownCast(typ, exp.prepare(layout)),
            P::Sub(exp, typ, check) => E::Sub(exp.prepare(layout), typ, check),
            P::Match(exp, pattern) => E::Match(exp.prepare(layout), pattern),
            P::Tuple(exps) => E::Tuple(exps.prepare(layout)),
            P::Case(not_exp) => E::Case(not_exp.prepare(layout)),
            P::Str(fields) => E::Str(
                fields
                    .into_iter()
                    .map(|(atom, exp)| (atom, exp.prepare(layout)))
                    .collect(),
            ),
            P::Opt(exp) => E::Opt(exp.prepare(layout)),
            P::List(exps) => E::List(exps.prepare(layout)),
            P::Cons(exp_head, exp_tail) => {
                E::Cons(exp_head.prepare(layout), exp_tail.prepare(layout))
            }
            P::Cat(exp_l, exp_r) => E::Cat(exp_l.prepare(layout), exp_r.prepare(layout)),
            P::Mem(exp_elem, exp_list) => {
                E::Mem(exp_elem.prepare(layout), exp_list.prepare(layout))
            }
            P::Len(exp) => E::Len(exp.prepare(layout)),
            P::Dot(exp, atom) => E::Dot(exp.prepare(layout), atom),
            P::Idx(exp_base, exp_idx) => E::Idx(exp_base.prepare(layout), exp_idx.prepare(layout)),
            P::Slice(exp_base, exp_idx, exp_len) => {
                E::Slice(exp_base.prepare(layout), exp_idx.prepare(layout), exp_len.prepare(layout))
            }
            P::Upd(exp_base, path, exp_new) => {
                E::Upd(exp_base.prepare(layout), path.prepare(layout), exp_new.prepare(layout))
            }
            P::Call(id, targs, args) => E::Call(id, targs, args.prepare(layout)),
            P::Iter(exp, iter) => E::Iter(exp.prepare(layout), iter.prepare(layout)),
        }
    }
}

// = Paths

impl Prepare for pl::PathKind {
    type Output = ast::PathKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::PathKind::Root => ast::PathKind::Root,
            pl::PathKind::Idx(path, exp) => {
                ast::PathKind::Idx(path.prepare(layout), exp.prepare(layout))
            }
            pl::PathKind::Slice(path, exp_idx, exp_len) => ast::PathKind::Slice(
                path.prepare(layout),
                exp_idx.prepare(layout),
                exp_len.prepare(layout),
            ),
            pl::PathKind::Dot(path, atom) => ast::PathKind::Dot(path.prepare(layout), atom),
        }
    }
}

// = Arguments

impl Prepare for pl::ArgKind {
    type Output = ast::ArgKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::ArgKind::Exp(exp) => ast::ArgKind::Exp(exp.prepare(layout)),
            pl::ArgKind::Def(id) => ast::ArgKind::Def(id),
        }
    }
}

// = Parameters

impl Prepare for pl::ParamKind {
    type Output = ast::ParamKind;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::ParamKind::Exp(typ, exp) => ast::ParamKind::Exp(typ, exp.prepare(layout)),
            pl::ParamKind::Def(id, tparams, params, typ) => {
                ast::ParamKind::Def(id, tparams, params.prepare(layout), typ)
            }
        }
    }
}

// = Holding conditions

impl<Tier: Prepare> Prepare for pl::HoldCase<Tier> {
    type Output = ast::HoldCase<Tier::Output>;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::HoldCase::Both(block_l, block_r) => {
                ast::HoldCase::Both(block_l.prepare(layout), block_r.prepare(layout))
            }
            pl::HoldCase::Hold(block, dangle) => ast::HoldCase::Hold(block.prepare(layout), dangle),
            pl::HoldCase::NotHold(block, dangle) => {
                ast::HoldCase::NotHold(block.prepare(layout), dangle)
            }
        }
    }
}

// = Case analysis

impl Prepare for pl::Guard {
    type Output = ast::Guard;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::Guard::Bool(cond) => ast::Guard::Bool(cond),
            pl::Guard::Cmp(op, typ, exp) => ast::Guard::Cmp(op, typ, exp.prepare(layout)),
            pl::Guard::Sub(typ, check) => ast::Guard::Sub(typ, check),
            pl::Guard::Match(pattern) => ast::Guard::Match(pattern),
            pl::Guard::Mem(exp) => ast::Guard::Mem(exp.prepare(layout)),
            pl::Guard::CheckLetSub(typ, check, exp) => {
                ast::Guard::CheckLetSub(typ, check, exp.prepare(layout))
            }
            pl::Guard::CheckLetMatch(pattern, exp) => {
                ast::Guard::CheckLetMatch(pattern, exp.prepare(layout))
            }
        }
    }
}

impl<Tier: Prepare> Prepare for pl::Case<Tier> {
    type Output = ast::Case<Tier::Output>;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        ast::Case { guard: self.guard.prepare(layout), block: self.block.prepare(layout) }
    }
}

// = Instructions

impl<Tier: Prepare> Prepare for pl::InstrKind<Tier> {
    type Output = ast::InstrKind<Tier::Output>;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::InstrKind::If(instr) => ast::InstrKind::If(ast::IfInstr {
                exp: instr.exp.prepare(layout),
                iter_exps: instr.iter_exps.prepare(layout),
                block: instr.block.prepare(layout),
                dangle: instr.dangle,
            }),
            pl::InstrKind::Hold(instr) => ast::InstrKind::Hold(ast::HoldInstr {
                id: instr.id,
                not_exp: instr.not_exp.prepare(layout),
                iter_exps: instr.iter_exps.prepare(layout),
                hold_case: instr.hold_case.prepare(layout),
            }),
            pl::InstrKind::Case(instr) => ast::InstrKind::Case(ast::CaseInstr {
                exp: instr.exp.prepare(layout),
                cases: instr.cases.prepare(layout),
                dangle: instr.dangle,
            }),
            pl::InstrKind::Let(instr) => ast::InstrKind::Let(ast::LetInstr {
                exp_l: instr.exp_l.prepare(layout),
                exp_r: instr.exp_r.prepare(layout),
                iter_instrs: instr.iter_instrs.prepare(layout),
            }),
            pl::InstrKind::Debug(instr) => {
                ast::InstrKind::Debug(ast::DebugInstr { exp: instr.exp.prepare(layout) })
            }
            pl::InstrKind::Destruct(instr) => ast::InstrKind::Destruct(ast::DestructInstr {
                bindings: instr
                    .bindings
                    .into_iter()
                    .map(|(name, exp)| (name, exp.prepare(layout)))
                    .collect(),
                exp: instr.exp.prepare(layout),
            }),
            pl::InstrKind::CheckLetSub(instr) => {
                ast::InstrKind::CheckLetSub(ast::CheckLetSubInstr {
                    typ: instr.typ,
                    subcheck: instr.subcheck,
                    exp_l: instr.exp_l.prepare(layout),
                    exp_r: instr.exp_r.prepare(layout),
                    block: instr.block.prepare(layout),
                })
            }
            pl::InstrKind::CheckLetMatch(instr) => {
                ast::InstrKind::CheckLetMatch(ast::CheckLetMatchInstr {
                    pattern: instr.pattern,
                    exp_l: instr.exp_l.prepare(layout),
                    exp_r: instr.exp_r.prepare(layout),
                    block: instr.block.prepare(layout),
                })
            }
            pl::InstrKind::OptionGet(instr) => ast::InstrKind::OptionGet(ast::OptionGetInstr {
                exp_l: instr.exp_l.prepare(layout),
                exp_r: instr.exp_r.prepare(layout),
                block: instr.block.prepare(layout),
            }),
            pl::InstrKind::Tier(instr) => {
                ast::InstrKind::Tier(ast::TierInstr { tier: instr.tier.prepare(layout) })
            }
        }
    }
}

// = Group-body tier

impl Prepare for pl::GroupInstr {
    type Output = ast::GroupInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::GroupInstr::Result(instr) => ast::GroupInstr::Result(ast::ResultInstr {
                rel_signature: instr.rel_signature,
                exps_output: instr.exps_output.prepare(layout),
            }),
            pl::GroupInstr::Return(instr) => {
                ast::GroupInstr::Return(ast::ReturnInstr { exp: instr.exp.prepare(layout) })
            }
            pl::GroupInstr::Rule(instr) => ast::GroupInstr::Rule(ast::RuleInstr {
                id: instr.id,
                not_exp: instr.not_exp.prepare(layout),
                input_hint: instr.input_hint,
                iter_instrs: instr.iter_instrs.prepare(layout),
            }),
            pl::GroupInstr::Backtrack(instr) => ast::GroupInstr::Backtrack(ast::BacktrackInstr {
                blocks: instr.blocks.prepare(layout),
            }),
        }
    }
}

// = Dispatch tier

impl Prepare for pl::DispatchInstr {
    type Output = ast::DispatchInstr;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::DispatchInstr::Group(instr) => ast::DispatchInstr::Group(ast::RuleGroupInstr {
                id_rel: instr.id_rel,
                id_group: instr.id_group,
                rel_signature: instr.rel_signature,
                exps_input: instr.exps_input.prepare(layout),
                block: instr.block.prepare(layout),
            }),
            pl::DispatchInstr::Route(instr) => {
                ast::DispatchInstr::Route(ast::RouteInstr { blocks: instr.blocks.prepare(layout) })
            }
        }
    }
}

// = Table rows

impl Prepare for pl::TableRow {
    type Output = ast::TableRow;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        ast::TableRow {
            exps_input: self.exps_input.prepare(layout),
            exp: self.exp.prepare(layout),
            block: self.block.prepare(layout),
        }
    }
}

// = Relation definitions

impl Prepare for pl::RelDef {
    type Output = ast::RelDef;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::RelDef::Extern(rel) => ast::RelDef::Extern(ast::ExternRel {
                id: rel.id,
                rel_signature: rel.rel_signature,
                exps_input: rel.exps_input.prepare(layout),
            }),
            pl::RelDef::Defined(rel) => ast::RelDef::Defined(ast::DefinedRel {
                id: rel.id,
                rel_signature: rel.rel_signature,
                exps_input: rel.exps_input.prepare(layout),
                block: rel.block.prepare(layout),
                block_else_opt: rel.block_else_opt.prepare(layout),
            }),
        }
    }
}

// = Meta-function definitions

impl Prepare for pl::MetaFuncDef {
    type Output = ast::MetaFuncDef;

    fn prepare(self, layout: &mut FrameLayout) -> Self::Output {
        match self {
            pl::MetaFuncDef::Extern(func) => ast::MetaFuncDef::Extern(ast::ExternFunc {
                id: func.id,
                tparams: func.tparams,
                params: func.params.prepare(layout),
                typ: func.typ,
            }),
            pl::MetaFuncDef::Builtin(func) => ast::MetaFuncDef::Builtin(ast::BuiltinFunc {
                id: func.id,
                tparams: func.tparams,
                params: func.params.prepare(layout),
                typ: func.typ,
            }),
            pl::MetaFuncDef::Table(func) => ast::MetaFuncDef::Table(ast::TableFunc {
                id: func.id,
                params: func.params.prepare(layout),
                typ: func.typ,
                rows: func.rows.prepare(layout),
            }),
            pl::MetaFuncDef::Defined(func) => ast::MetaFuncDef::Defined(ast::DefinedFunc {
                id: func.id,
                tparams: func.tparams,
                params: func.params.prepare(layout),
                typ: func.typ,
                block: func.block.prepare(layout),
                block_else_opt: func.block_else_opt.prepare(layout),
            }),
        }
    }
}
