//! Prepared PL syntax without prose annotations
//!
//! `strip_exp` converts prepared PL expressions and assignment patterns
//! into the shared evaluator's AST, preserving slots, types, and spans.
//! Recursive notation, path, and argument conversion leaves PL source intact.

use crate::lang::il::prepared as shared_ast;

use crate::lang::pl::prepared as ast;

// = Expressions

/// Removes prose annotations while retaining executable syntax and source data.
pub(super) fn strip_exp(exp: &ast::Exp) -> shared_ast::Exp {
    stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
        use ast::ExpKind as P;
        use shared_ast::ExpKind as E;
        let node = match &exp.node.node {
            P::Bool(value) => E::Bool(*value),
            P::Num(num) => E::Num(num.clone()),
            P::Text(text) => E::Text(text.clone()),
            P::Id(id) => E::Id(id.clone()),
            P::Un(op, typ, exp) => E::Un(*op, *typ, Box::new(strip_exp(exp))),
            P::Bin(op, typ, exp_l, exp_r) => {
                E::Bin(*op, *typ, Box::new(strip_exp(exp_l)), Box::new(strip_exp(exp_r)))
            }
            P::Cmp(op, typ, exp_l, exp_r) => {
                E::Cmp(*op, *typ, Box::new(strip_exp(exp_l)), Box::new(strip_exp(exp_r)))
            }
            P::UpCast(typ, exp) => E::UpCast(Box::new(typ.clone()), Box::new(strip_exp(exp))),
            P::DownCast(typ, exp) => E::DownCast(Box::new(typ.clone()), Box::new(strip_exp(exp))),
            P::Sub(exp, typ, check) => {
                E::Sub(Box::new(strip_exp(exp)), Box::new(typ.clone()), check.clone())
            }
            P::Match(exp, pattern) => E::Match(Box::new(strip_exp(exp)), pattern.clone()),
            P::Tuple(exps) => E::Tuple(exps.iter().map(strip_exp).collect()),
            P::Case(not_exp) => E::Case(Box::new(strip_not_exp(not_exp))),
            P::Str(fields) => E::Str(
                fields
                    .iter()
                    .map(|(atom, exp)| shared_ast::ExpField {
                        atom: atom.clone(),
                        exp: strip_exp(exp),
                    })
                    .collect(),
            ),
            P::Opt(exp) => E::Opt(exp.as_ref().map(|exp| Box::new(strip_exp(exp)))),
            P::List(exps) => E::List(exps.iter().map(strip_exp).collect()),
            P::Cons(exp_head, exp_tail) => {
                E::Cons(Box::new(strip_exp(exp_head)), Box::new(strip_exp(exp_tail)))
            }
            P::Cat(exp_l, exp_r) => E::Cat(Box::new(strip_exp(exp_l)), Box::new(strip_exp(exp_r))),
            P::Mem(exp_elem, exp_list) => {
                E::Mem(Box::new(strip_exp(exp_elem)), Box::new(strip_exp(exp_list)))
            }
            P::Len(exp) => E::Len(Box::new(strip_exp(exp))),
            P::Dot(exp, atom) => E::Dot(Box::new(strip_exp(exp)), atom.clone()),
            P::Idx(exp_base, exp_idx) => {
                E::Idx(Box::new(strip_exp(exp_base)), Box::new(strip_exp(exp_idx)))
            }
            P::Slice(exp_base, exp_idx, exp_len) => E::Slice(
                Box::new(strip_exp(exp_base)),
                Box::new(strip_exp(exp_idx)),
                Box::new(strip_exp(exp_len)),
            ),
            P::Upd(exp_base, path, exp_new) => E::Upd(
                Box::new(strip_exp(exp_base)),
                Box::new(strip_path(path)),
                Box::new(strip_exp(exp_new)),
            ),
            P::Call(id, targs, args) => {
                E::Call(id.clone(), targs.clone(), args.iter().map(strip_arg).collect())
            }
            P::Iter(exp, iter) => E::Iter(Box::new(strip_exp(exp)), iter.clone()),
        };
        crate::note_phrase! {
            node: node,
            note: exp.node.note.clone(),
            span: exp.node.span.clone(),
        }
    })
}

// = Notation

fn strip_not_exp(not_exp: &ast::NotExp) -> shared_ast::NotExp {
    not_exp.map(strip_exp)
}

// = Paths

fn strip_path(path: &ast::Path) -> shared_ast::Path {
    let node = match &path.node {
        ast::PathKind::Root => shared_ast::PathKind::Root,
        ast::PathKind::Idx(path, exp) => {
            shared_ast::PathKind::Idx(Box::new(strip_path(path)), Box::new(strip_exp(exp)))
        }
        ast::PathKind::Slice(path, exp_idx, exp_len) => shared_ast::PathKind::Slice(
            Box::new(strip_path(path)),
            Box::new(strip_exp(exp_idx)),
            Box::new(strip_exp(exp_len)),
        ),
        ast::PathKind::Dot(path, atom) => {
            shared_ast::PathKind::Dot(Box::new(strip_path(path)), atom.clone())
        }
    };
    crate::note_phrase!(node: node, note: path.note.clone(), span: path.span.clone())
}

// = Arguments

fn strip_arg(arg: &ast::Arg) -> shared_ast::Arg {
    let node = match &arg.node {
        ast::ArgKind::Exp(exp) => shared_ast::ArgKind::Exp(Box::new(strip_exp(exp))),
        ast::ArgKind::Def(id) => shared_ast::ArgKind::Def(id.clone()),
    };
    crate::phrase!(node: node, span: arg.span.clone())
}
