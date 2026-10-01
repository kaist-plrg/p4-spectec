//! Minimal-dimension inference for expression variables
//!
//! This is the binding pass's reduced form of elaboration dimension inference:
//! it records free variables
//! and retains the minimal dimension when one occurs more than once.

use crate::lang::common::ds::map::IdMap;

use crate::lang::il::ast;

use crate::runtime::{dim::Dim, envs::algo::VEnv};

// == Variable inference

/// Records `id` at its dimension, keeping the smaller one when repeated.
fn infer_id_exp(venv: &mut VEnv, exp: &ast::Exp, id: &ast::Id, iters: &[ast::Iter]) {
    let typ = crate::phrase!(node: exp.note.as_ref().clone(), span: exp.span.clone());
    let dim = Dim::new(typ, iters.to_vec());
    // A smaller dimension replaces the recorded one
    if venv
        .get(id)
        .is_none_or(|dim_previous| dim.sub(dim_previous))
    {
        venv.insert(id.clone(), dim);
    }
}

// == Expression inference

/// Infers the dimension of every variable occurring in an expression.
pub fn infer_exp(exp: &ast::Exp) -> VEnv {
    let mut venv = IdMap::new();
    infer_exp_inner(&mut venv, exp, &[]);
    venv
}

fn infer_exp_inner(venv: &mut VEnv, exp: &ast::Exp, iters: &[ast::Iter]) {
    match &exp.node {
        // Literals mention no identifiers; identifiers record their dimension
        ast::ExpKind::Bool(_) | ast::ExpKind::Num(_) | ast::ExpKind::Text(_) => {}
        ast::ExpKind::Id(id) => infer_id_exp(venv, exp, id, iters),
        // Unary forms collect from the operand
        ast::ExpKind::Un(_, _, exp)
        | ast::ExpKind::UpCast(_, exp)
        | ast::ExpKind::DownCast(_, exp)
        | ast::ExpKind::Sub(exp, _, _)
        | ast::ExpKind::Match(exp, _)
        | ast::ExpKind::Len(exp)
        | ast::ExpKind::Dot(exp, _) => infer_exp_inner(venv, exp, iters),
        // Binary forms collect from both operands
        ast::ExpKind::Bin(_, _, exp_l, exp_r)
        | ast::ExpKind::Cmp(_, _, exp_l, exp_r)
        | ast::ExpKind::Cons(exp_l, exp_r)
        | ast::ExpKind::Cat(exp_l, exp_r)
        | ast::ExpKind::Mem(exp_l, exp_r)
        | ast::ExpKind::Idx(exp_l, exp_r) => {
            infer_exp_inner(venv, exp_l, iters);
            infer_exp_inner(venv, exp_r, iters);
        }
        // Tuple or list: every element
        ast::ExpKind::Tuple(exps) | ast::ExpKind::List(exps) => {
            infer_exps_inner(venv, exps, iters);
        }
        // Case: the arguments
        ast::ExpKind::Case(not_exp) => {
            for exp in not_exp.args() {
                infer_exp_inner(venv, exp, iters);
            }
        }
        // Struct: the fields
        ast::ExpKind::Str(fields) => {
            for ast::ExpField { exp, .. } in fields {
                infer_exp_inner(venv, exp, iters);
            }
        }
        // Option: the payload
        ast::ExpKind::Opt(Some(exp)) => infer_exp_inner(venv, exp, iters),
        // Empty option mentions nothing
        ast::ExpKind::Opt(None) => {}
        // Slice: base, index, and length
        ast::ExpKind::Slice(exp_base, exp_idx, exp_len) => {
            infer_exp_inner(venv, exp_base, iters);
            infer_exp_inner(venv, exp_idx, iters);
            infer_exp_inner(venv, exp_len, iters);
        }
        // Update: base, path, and field
        ast::ExpKind::Upd(exp_base, path, exp_field) => {
            infer_exp_inner(venv, exp_base, iters);
            infer_path_inner(venv, path, iters);
            infer_exp_inner(venv, exp_field, iters);
        }
        // Call: the arguments
        ast::ExpKind::Call(_, _, args) => infer_args_inner(venv, args, iters),
        // The innermost iteration comes first
        ast::ExpKind::Iter(exp, ast::ExpIter { iter, .. }) => {
            let mut iters_inner = Vec::with_capacity(iters.len() + 1);
            iters_inner.push(*iter);
            iters_inner.extend_from_slice(iters);
            infer_exp_inner(venv, exp, &iters_inner);
        }
    }
}

/// Infers the dimension of every variable occurring in a list of expressions.
pub fn infer_exps(exps: &[ast::Exp]) -> VEnv {
    let mut venv = IdMap::new();
    infer_exps_inner(&mut venv, exps, &[]);
    venv
}

fn infer_exps_inner(venv: &mut VEnv, exps: &[ast::Exp], iters: &[ast::Iter]) {
    for exp in exps {
        infer_exp_inner(venv, exp, iters);
    }
}

// == Path inference

fn infer_path_inner(venv: &mut VEnv, path: &ast::Path, iters: &[ast::Iter]) {
    match &path.node {
        ast::PathKind::Root => {}
        ast::PathKind::Idx(path, exp) => {
            infer_path_inner(venv, path, iters);
            infer_exp_inner(venv, exp, iters);
        }
        ast::PathKind::Slice(path, exp_idx, exp_len) => {
            infer_path_inner(venv, path, iters);
            infer_exp_inner(venv, exp_idx, iters);
            infer_exp_inner(venv, exp_len, iters);
        }
        ast::PathKind::Dot(path, _) => infer_path_inner(venv, path, iters),
    }
}

// == Argument inference

fn infer_args_inner(venv: &mut VEnv, args: &[ast::Arg], iters: &[ast::Iter]) {
    for arg in args {
        if let ast::ArgKind::Exp(exp) = &arg.node {
            infer_exp_inner(venv, exp, iters);
        }
    }
}
