//! Calls contained in prose-language syntax
//!
//! Expressions, paths, and case guards collect located calls in preorder,
//! including calls nested in arguments and update paths.

use crate::lang::traits::has_call::HasCall;

use super::ast::{ArgKind, Exp, ExpKind, Guard, Path, PathKind};

// == Expressions

impl HasCall for Exp {
    type Exp = Self;

    fn nested_call(&self) -> Vec<&Self::Exp> {
        match &self.node.node {
            ExpKind::Bool(_) | ExpKind::Num(_) | ExpKind::Text(_) | ExpKind::Id(_) => vec![],
            ExpKind::Un(_, _, exp)
            | ExpKind::UpCast(_, exp)
            | ExpKind::DownCast(_, exp)
            | ExpKind::Sub(exp, _, _)
            | ExpKind::Match(exp, _)
            | ExpKind::Len(exp)
            | ExpKind::Dot(exp, _)
            | ExpKind::Iter(exp, _) => exp.nested_call(),
            ExpKind::Bin(_, _, exp_l, exp_r)
            | ExpKind::Cmp(_, _, exp_l, exp_r)
            | ExpKind::Cons(exp_l, exp_r)
            | ExpKind::Cat(exp_l, exp_r)
            | ExpKind::Mem(exp_l, exp_r)
            | ExpKind::Idx(exp_l, exp_r) => [exp_l, exp_r]
                .into_iter()
                .flat_map(|exp| exp.nested_call())
                .collect(),
            ExpKind::Tuple(exps) | ExpKind::List(exps) => {
                exps.iter().flat_map(HasCall::nested_call).collect()
            }
            ExpKind::Case(not_exp) => not_exp
                .args()
                .into_iter()
                .flat_map(HasCall::nested_call)
                .collect(),
            ExpKind::Str(fields) => fields
                .iter()
                .flat_map(|(_, exp)| exp.nested_call())
                .collect(),
            ExpKind::Opt(exp_opt) => exp_opt.iter().flat_map(|exp| exp.nested_call()).collect(),
            ExpKind::Slice(exp_base, exp_idx, exp_len) => [exp_base, exp_idx, exp_len]
                .into_iter()
                .flat_map(|exp| exp.nested_call())
                .collect(),
            ExpKind::Upd(exp_base, path, exp_field) => exp_base
                .nested_call()
                .into_iter()
                .chain(path.nested_call())
                .chain(exp_field.nested_call())
                .collect(),
            ExpKind::Call(_, _, args) => std::iter::once(self)
                .chain(args.iter().flat_map(|arg| match &arg.node {
                    ArgKind::Exp(exp) => exp.nested_call(),
                    ArgKind::Def(_) => vec![],
                }))
                .collect(),
        }
    }
}

// == Paths

impl HasCall for Path {
    type Exp = Exp;

    fn nested_call(&self) -> Vec<&Self::Exp> {
        match &self.node {
            PathKind::Root => vec![],
            PathKind::Idx(path, exp_idx) => path
                .nested_call()
                .into_iter()
                .chain(exp_idx.nested_call())
                .collect(),
            PathKind::Slice(path, exp_idx, exp_len) => path
                .nested_call()
                .into_iter()
                .chain(exp_idx.nested_call())
                .chain(exp_len.nested_call())
                .collect(),
            PathKind::Dot(path, _) => path.nested_call(),
        }
    }
}

// == Guards

impl HasCall for Guard {
    type Exp = Exp;

    fn nested_call(&self) -> Vec<&Self::Exp> {
        match self {
            Guard::Bool(_) | Guard::Sub(..) | Guard::Match(_) => vec![],
            Guard::Cmp(_, _, exp)
            | Guard::Mem(exp)
            | Guard::CheckLetSub(_, _, exp)
            | Guard::CheckLetMatch(_, exp) => exp.nested_call(),
        }
    }
}
