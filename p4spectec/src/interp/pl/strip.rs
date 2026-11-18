//! PL source expressions without prose hints
//!
//! Prose hints only render, so preparation strips them
//! and prepares the IL expression that AL and SL also execute:
//! `x + 1` with a prose hint becomes the IL expression `x + 1`.

use crate::lang::il::ast as il;

use crate::lang::pl::ast as pl;

// = Expressions

/// Removes the prose hints of an expression and its subexpressions.
pub(super) fn strip_exp(exp: pl::Exp) -> il::Exp {
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
        use il::ExpKind as E;
        use pl::ExpKind as P;
        let exp = exp.node;
        let strip_box = |exp: Box<pl::Exp>| Box::new(strip_exp(*exp));
        let node = match exp.node {
            P::Bool(value) => E::Bool(value),
            P::Num(num) => E::Num(num),
            P::Text(text) => E::Text(text),
            P::Id(id) => E::Id(id),
            P::Un(op, typ, exp) => E::Un(op, typ, strip_box(exp)),
            P::Bin(op, typ, exp_l, exp_r) => E::Bin(op, typ, strip_box(exp_l), strip_box(exp_r)),
            P::Cmp(op, typ, exp_l, exp_r) => E::Cmp(op, typ, strip_box(exp_l), strip_box(exp_r)),
            P::UpCast(typ, exp) => E::UpCast(Box::new(typ), strip_box(exp)),
            P::DownCast(typ, exp) => E::DownCast(Box::new(typ), strip_box(exp)),
            P::Sub(exp, typ) => E::Sub(strip_box(exp), Box::new(typ)),
            P::Match(exp, pattern) => E::Match(strip_box(exp), pattern),
            P::Tuple(exps) => E::Tuple(exps.into_iter().map(strip_exp).collect()),
            P::Case(not_exp) => E::Case(Box::new(not_exp.into_map(strip_exp))),
            P::Str(fields) => E::Str(
                fields
                    .into_iter()
                    .map(|(atom, exp)| il::ExpField { atom, exp: strip_exp(exp) })
                    .collect(),
            ),
            P::Opt(exp) => E::Opt(exp.map(strip_box)),
            P::List(exps) => E::List(exps.into_iter().map(strip_exp).collect()),
            P::Cons(exp_head, exp_tail) => E::Cons(strip_box(exp_head), strip_box(exp_tail)),
            P::Cat(exp_l, exp_r) => E::Cat(strip_box(exp_l), strip_box(exp_r)),
            P::Mem(exp_elem, exp_list) => E::Mem(strip_box(exp_elem), strip_box(exp_list)),
            P::Len(exp) => E::Len(strip_box(exp)),
            P::Dot(exp, atom) => E::Dot(strip_box(exp), atom),
            P::Idx(exp_base, exp_idx) => E::Idx(strip_box(exp_base), strip_box(exp_idx)),
            P::Slice(exp_base, exp_idx, exp_len) => {
                E::Slice(strip_box(exp_base), strip_box(exp_idx), strip_box(exp_len))
            }
            P::Upd(exp_base, path, exp_new) => {
                E::Upd(strip_box(exp_base), Box::new(strip_path(*path)), strip_box(exp_new))
            }
            P::Call(id, targs, args) => {
                E::Call(id, targs, args.into_iter().map(strip_arg).collect())
            }
            P::Iter(exp, exp_iter) => E::Iter(strip_box(exp), exp_iter),
        };
        crate::note_phrase!(node: node, note: exp.note, span: exp.span)
    })
}

// = Paths

fn strip_path(path: pl::Path) -> il::Path {
    let node = match path.node {
        pl::PathKind::Root => il::PathKind::Root,
        pl::PathKind::Idx(path, exp) => {
            il::PathKind::Idx(Box::new(strip_path(*path)), Box::new(strip_exp(*exp)))
        }
        pl::PathKind::Slice(path, exp_idx, exp_len) => il::PathKind::Slice(
            Box::new(strip_path(*path)),
            Box::new(strip_exp(*exp_idx)),
            Box::new(strip_exp(*exp_len)),
        ),
        pl::PathKind::Dot(path, atom) => il::PathKind::Dot(Box::new(strip_path(*path)), atom),
    };
    crate::note_phrase!(node: node, note: path.note, span: path.span)
}

// = Arguments

fn strip_arg(arg: pl::Arg) -> il::Arg {
    let node = match arg.node {
        pl::ArgKind::Exp(exp) => il::ArgKind::Exp(Box::new(strip_exp(*exp))),
        pl::ArgKind::Def(id) => il::ArgKind::Def(id),
    };
    crate::phrase!(node: node, span: arg.span)
}
