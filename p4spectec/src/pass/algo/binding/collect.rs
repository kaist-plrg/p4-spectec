//! Binding collection through invertible expression positions
//!
//! Free identifiers become binders only while traversing invertible constructs.
//! A binder found below any non-invertible operation
//! is rejected at that operation's source span.
//! In `let (x, y + 1) = e`, `x` is collected and `y` is rejected at `y + 1`.

use crate::lang::common::source::Span;

use crate::lang::il::ast;

use super::super::{AlgoError, error};

use super::{bind::BEnv, context::Context};

// == Helpers

// - Errors

/// Rejects binders found under a non-invertible construct.
fn reject_noninvertible(
    span: Span,
    construct: &'static str,
    benv: BEnv,
) -> Result<BEnv, AlgoError> {
    if benv.is_empty() {
        Ok(benv)
    } else {
        let error = error::binding::binding_non_invertible(&span, construct, &benv);
        Err(error)
    }
}

// == Binding collection

// - Expressions

/// Collects the binders of an expression.
pub fn collect_exp(ctx: &Context, exp: &ast::Exp) -> Result<BEnv, AlgoError> {
    match &exp.node {
        // Literals bind nothing
        ast::ExpKind::Bool(_) | ast::ExpKind::Num(_) | ast::ExpKind::Text(_) => {
            let benv = BEnv::new();
            Ok(benv)
        }
        // An identifier not bound yet is a binder
        ast::ExpKind::Id(id) => {
            if ctx.venv.contains_key(id) {
                let benv = BEnv::new();
                Ok(benv)
            } else {
                let typ = crate::phrase!(node: exp.note.as_ref().clone(), span: exp.span.clone());
                let benv = BEnv::singleton(id.clone(), typ);
                Ok(benv)
            }
        }
        // Unary operator: not invertible, so a binder below is an error
        ast::ExpKind::Un(_, _, exp_inner) => {
            let benv = collect_exp(ctx, exp_inner)?;
            reject_noninvertible(exp_inner.span.clone(), "unary operator", benv)
        }
        // Binary operator: not invertible
        ast::ExpKind::Bin(_, _, exp_l, exp_r) => {
            let benv_l = collect_exp(ctx, exp_l)?;
            let benv_r = collect_exp(ctx, exp_r)?;
            let benv = benv_l.union(benv_r)?;
            reject_noninvertible(exp.span.clone(), "binary operator", benv)
        }
        // Comparison: not invertible
        ast::ExpKind::Cmp(_, _, exp_l, exp_r) => {
            let benv_l = collect_exp(ctx, exp_l)?;
            let benv_r = collect_exp(ctx, exp_r)?;
            let benv = benv_l.union(benv_r)?;
            reject_noninvertible(exp.span.clone(), "comparison operator", benv)
        }
        // Upcast: invertible, binders pass through
        ast::ExpKind::UpCast(_, exp_inner) => collect_exp(ctx, exp_inner),
        // Downcast: not invertible
        ast::ExpKind::DownCast(_, exp_inner) => {
            let benv = collect_exp(ctx, exp_inner)?;
            reject_noninvertible(exp_inner.span.clone(), "downcast operator", benv)
        }
        // Subtype test: not invertible
        ast::ExpKind::Sub(exp_inner, _, _) => {
            let benv = collect_exp(ctx, exp_inner)?;
            reject_noninvertible(exp_inner.span.clone(), "subtype check operator", benv)
        }
        // Pattern test: not invertible
        ast::ExpKind::Match(exp_inner, _) => {
            let benv = collect_exp(ctx, exp_inner)?;
            reject_noninvertible(exp_inner.span.clone(), "match check operator", benv)
        }
        // Tuple or list: binders in every component
        ast::ExpKind::Tuple(exps) | ast::ExpKind::List(exps) => collect_exps(ctx, exps),
        // Case: binders in the arguments
        ast::ExpKind::Case(not_exp) => collect_exps(ctx, not_exp.args()),
        // Struct: binders in the fields
        ast::ExpKind::Str(fields) => {
            collect_exps(ctx, fields.iter().map(|ast::ExpField { exp, .. }| exp))
        }
        // Option: binders in the payload
        ast::ExpKind::Opt(Some(exp_inner)) => collect_exp(ctx, exp_inner),
        // Empty option binds nothing
        ast::ExpKind::Opt(None) => {
            let benv = BEnv::new();
            Ok(benv)
        }
        // Cons: binders in head and tail
        ast::ExpKind::Cons(exp_l, exp_r) => {
            let benv_l = collect_exp(ctx, exp_l)?;
            let benv_r = collect_exp(ctx, exp_r)?;
            benv_l.union(benv_r)
        }
        // Concatenation: not invertible
        ast::ExpKind::Cat(exp_l, exp_r) => {
            let benv_l = collect_exp(ctx, exp_l)?;
            let benv_r = collect_exp(ctx, exp_r)?;
            let benv = benv_l.union(benv_r)?;
            reject_noninvertible(exp.span.clone(), "concatenation operator", benv)
        }
        // Membership: not invertible
        ast::ExpKind::Mem(exp_l, exp_r) => {
            let benv_l = collect_exp(ctx, exp_l)?;
            let benv_r = collect_exp(ctx, exp_r)?;
            let benv = benv_l.union(benv_r)?;
            reject_noninvertible(exp.span.clone(), "set membership operator", benv)
        }
        // Length: not invertible
        ast::ExpKind::Len(exp_inner) => {
            let benv = collect_exp(ctx, exp_inner)?;
            reject_noninvertible(exp_inner.span.clone(), "length operator", benv)
        }
        // Field access: not invertible
        ast::ExpKind::Dot(exp_inner, _) => {
            let benv = collect_exp(ctx, exp_inner)?;
            reject_noninvertible(exp_inner.span.clone(), "dot operator", benv)
        }
        // Indexing: not invertible
        ast::ExpKind::Idx(exp_base, exp_idx) => {
            let benv_base = collect_exp(ctx, exp_base)?;
            let benv_idx = collect_exp(ctx, exp_idx)?;
            let benv = benv_base.union(benv_idx)?;
            reject_noninvertible(exp.span.clone(), "indexing operator", benv)
        }
        // Slicing: not invertible
        ast::ExpKind::Slice(exp_base, exp_idx, exp_len) => {
            let benv_base = collect_exp(ctx, exp_base)?;
            let benv_idx = collect_exp(ctx, exp_idx)?;
            let benv_len = collect_exp(ctx, exp_len)?;
            let benv = benv_base.union(benv_idx)?;
            let benv = benv.union(benv_len)?;
            reject_noninvertible(exp.span.clone(), "slicing operator", benv)
        }
        // Update: not invertible
        ast::ExpKind::Upd(exp_base, path, exp_field) => {
            let benv_base = collect_exp(ctx, exp_base)?;
            let benv_path = collect_path(ctx, path)?;
            let benv_field = collect_exp(ctx, exp_field)?;
            let benv = benv_base.union(benv_field)?;
            let benv = benv.union(benv_path)?;
            reject_noninvertible(exp.span.clone(), "update operator", benv)
        }
        // Call: not invertible
        ast::ExpKind::Call(_, _, args) => {
            let benv = collect_args(ctx, args)?;
            reject_noninvertible(exp.span.clone(), "call operator", benv)
        }
        // Binders under an iteration gain its dimension
        ast::ExpKind::Iter(exp_inner, ast::ExpIter { iter, .. }) => {
            let benv = collect_exp(ctx, exp_inner)?;
            let benv = benv.add_iter(*iter);
            Ok(benv)
        }
    }
}

/// Collects binders across expressions, combining parallel occurrences.
pub fn collect_exps<'a>(
    ctx: &Context,
    exps: impl IntoIterator<Item = &'a ast::Exp>,
) -> Result<BEnv, AlgoError> {
    let mut benvs = Vec::new();
    for exp in exps {
        benvs.push(collect_exp(ctx, exp)?);
    }
    let mut benv = BEnv::new();
    for benv_head in benvs.into_iter().rev() {
        benv = benv_head.union(benv)?;
    }
    Ok(benv)
}

// - Paths

/// Collects binders in the index and slice expressions along a path.
pub fn collect_path(ctx: &Context, path: &ast::Path) -> Result<BEnv, AlgoError> {
    match &path.node {
        ast::PathKind::Root => {
            let benv = BEnv::new();
            Ok(benv)
        }
        ast::PathKind::Idx(path, exp) => {
            let benv_path = collect_path(ctx, path)?;
            let benv_exp = collect_exp(ctx, exp)?;
            benv_path.union(benv_exp)
        }
        ast::PathKind::Slice(path, exp_idx, exp_len) => {
            let benv_path = collect_path(ctx, path)?;
            let benv_idx = collect_exp(ctx, exp_idx)?;
            let benv_len = collect_exp(ctx, exp_len)?;
            let benv = benv_path.union(benv_idx)?;
            benv.union(benv_len)
        }
        ast::PathKind::Dot(path, _) => collect_path(ctx, path),
    }
}

// - Arguments

/// Collects binders of an expression argument; function arguments have none.
pub fn collect_arg(ctx: &Context, arg: &ast::Arg) -> Result<BEnv, AlgoError> {
    match &arg.node {
        ast::ArgKind::Exp(exp) => collect_exp(ctx, exp),
        ast::ArgKind::Def(_) => {
            let benv = BEnv::new();
            Ok(benv)
        }
    }
}

/// Collects binders across arguments, combining parallel occurrences.
pub fn collect_args(ctx: &Context, args: &[ast::Arg]) -> Result<BEnv, AlgoError> {
    let mut benvs = Vec::with_capacity(args.len());
    for arg in args {
        benvs.push(collect_arg(ctx, arg)?);
    }
    let mut benv = BEnv::new();
    for benv_head in benvs.into_iter().rev() {
        benv = benv_head.union(benv)?;
    }
    Ok(benv)
}
