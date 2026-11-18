//! PL source expressions without prose hints
//!
//! Prose hints only render, so preparation strips them
//! and prepares the IL expression that AL and SL also execute:
//! `x + 1` with a prose hint becomes the IL expression `x + 1`.

use crate::lang::il::ast as il;

use crate::lang::pl::ast as pl;

// = Expressions

/// Removes the prose hints of an expression and its subexpressions.
impl From<pl::Exp> for il::Exp {
    fn from(exp: pl::Exp) -> Self {
        stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
            use il::ExpKind as E;
            use pl::ExpKind as P;
            let exp = exp.node;
            let node = match exp.node {
                P::Bool(value) => E::Bool(value),
                P::Num(num) => E::Num(num),
                P::Text(text) => E::Text(text),
                P::Id(id) => E::Id(id),
                P::Un(op, typ, exp) => E::Un(op, typ, boxed(*exp)),
                P::Bin(op, typ, exp_l, exp_r) => E::Bin(op, typ, boxed(*exp_l), boxed(*exp_r)),
                P::Cmp(op, typ, exp_l, exp_r) => E::Cmp(op, typ, boxed(*exp_l), boxed(*exp_r)),
                P::UpCast(typ, exp) => E::UpCast(Box::new(typ), boxed(*exp)),
                P::DownCast(typ, exp) => E::DownCast(Box::new(typ), boxed(*exp)),
                P::Sub(exp, typ) => E::Sub(boxed(*exp), Box::new(typ)),
                P::Match(exp, pattern) => E::Match(boxed(*exp), pattern),
                P::Tuple(exps) => E::Tuple(exps.into_iter().map(Self::from).collect()),
                P::Case(not_exp) => E::Case(Box::new(not_exp.into_map(Self::from))),
                P::Str(fields) => E::Str(
                    fields
                        .into_iter()
                        .map(|(atom, exp)| il::ExpField { atom, exp: exp.into() })
                        .collect(),
                ),
                P::Opt(exp) => E::Opt(exp.map(|exp| boxed(*exp))),
                P::List(exps) => E::List(exps.into_iter().map(Self::from).collect()),
                P::Cons(exp_head, exp_tail) => E::Cons(boxed(*exp_head), boxed(*exp_tail)),
                P::Cat(exp_l, exp_r) => E::Cat(boxed(*exp_l), boxed(*exp_r)),
                P::Mem(exp_elem, exp_list) => E::Mem(boxed(*exp_elem), boxed(*exp_list)),
                P::Len(exp) => E::Len(boxed(*exp)),
                P::Dot(exp, atom) => E::Dot(boxed(*exp), atom),
                P::Idx(exp_base, exp_idx) => E::Idx(boxed(*exp_base), boxed(*exp_idx)),
                P::Slice(exp_base, exp_idx, exp_len) => {
                    E::Slice(boxed(*exp_base), boxed(*exp_idx), boxed(*exp_len))
                }
                P::Upd(exp_base, path, exp_new) => {
                    E::Upd(boxed(*exp_base), Box::new((*path).into()), boxed(*exp_new))
                }
                P::Call(id, targs, args) => {
                    E::Call(id, targs, args.into_iter().map(il::Arg::from).collect())
                }
                P::Iter(exp, exp_iter) => E::Iter(boxed(*exp), exp_iter),
            };
            crate::note_phrase!(node: node, note: exp.note, span: exp.span)
        })
    }
}

/// Removes the prose hints of an expression to box.
fn boxed(exp: pl::Exp) -> Box<il::Exp> {
    Box::new(exp.into())
}

// = Paths

/// Removes the prose hints of a path's expressions.
impl From<pl::Path> for il::Path {
    fn from(path: pl::Path) -> Self {
        let node = match path.node {
            pl::PathKind::Root => il::PathKind::Root,
            pl::PathKind::Idx(path, exp) => {
                il::PathKind::Idx(Box::new((*path).into()), boxed(*exp))
            }
            pl::PathKind::Slice(path, exp_idx, exp_len) => {
                il::PathKind::Slice(Box::new((*path).into()), boxed(*exp_idx), boxed(*exp_len))
            }
            pl::PathKind::Dot(path, atom) => il::PathKind::Dot(Box::new((*path).into()), atom),
        };
        crate::note_phrase!(node: node, note: path.note, span: path.span)
    }
}

// = Arguments

/// Removes the prose hints of an argument's expression.
impl From<pl::Arg> for il::Arg {
    fn from(arg: pl::Arg) -> Self {
        let node = match arg.node {
            pl::ArgKind::Exp(exp) => il::ArgKind::Exp(boxed(*exp)),
            pl::ArgKind::Def(id) => il::ArgKind::Def(id),
        };
        crate::phrase!(node: node, span: arg.span)
    }
}
