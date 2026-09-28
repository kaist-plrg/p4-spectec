//! Shared expression evaluation
//!
//! `eval_exp` evaluates prepared syntax to arena values,
//! nesting every failure under an evaluation trace for the expression.
//! Operators delegate to `ops`, calls to the stage's `Invoker`,
//! iterations to `iter::map`.

use super::super::context::ReadContext;
use super::Invoker;
use crate::interp::shared::prepare::ast;
use crate::lang::data::var::IdSlot;
use crate::lang::traits::at::At;

use std::{borrow::Borrow, rc::Rc};

use crate::interp::shared::error::ExprErrorKind;

use crate::{
    lang::{
        common::source::Span,
        data::value::{Value, ValueKind, get, make},
        traits::print::Print,
    },
    runner::{Extern, Interface, RunnerContext},
    runtime::{
        ops::typ::{TypeError, subst_typ},
        typdef::TypeDef,
    },
};

use super::{arg::eval_args, iter, ops, path::eval_update_path};
use crate::interp::shared::{
    backtrack::{Backtrack, err, ok, unmatch, unwrap, unwrap_from_result},
    error::{EntityKind, Error, ErrorKind},
    util::{find_slot_of_exp, find_var_of_exp},
};

// = Expression evaluation

/// Evaluates an expression, nesting failures under an evaluation trace.
pub(crate) fn eval_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    exp: &ast::Exp,
) -> Backtrack<Value> {
    let span = &exp.span;
    let typ = &exp.note;
    let result = (|| match &exp.node {
        ast::ExpKind::Bool(value) => ok!(unwrap_from_result!(
            make::bool(runner_ctx.arena_mut(), *value, Span::default()),
            span
        )),
        ast::ExpKind::Num(value) => ok!(unwrap_from_result!(
            make::num(runner_ctx.arena_mut(), value.clone(), Span::default()),
            span
        )),
        ast::ExpKind::Text(value) => ok!(unwrap_from_result!(
            make::text(runner_ctx.arena_mut(), value.clone(), Span::default()),
            span
        )),
        ast::ExpKind::Id(id) => eval_id_exp(ctx, span, id),
        ast::ExpKind::Un(op, _, exp_inner) => eval_un_exp(runner_ctx, ctx, span, op, exp_inner),
        ast::ExpKind::Bin(op, _, exp_l, exp_r) => {
            eval_bin_exp(runner_ctx, ctx, span, op, exp_l, exp_r)
        }
        ast::ExpKind::Cmp(op, _, exp_l, exp_r) => {
            eval_cmp_exp(runner_ctx, ctx, span, op, exp_l, exp_r)
        }
        ast::ExpKind::UpCast(typ, exp_inner) => eval_upcast_exp(runner_ctx, ctx, typ, exp_inner),
        ast::ExpKind::DownCast(typ, exp_inner) => {
            eval_downcast_exp(runner_ctx, ctx, typ, exp_inner)
        }
        ast::ExpKind::Sub(exp_inner, _, subcheck) => {
            eval_sub_exp(runner_ctx, ctx, span, exp_inner, subcheck)
        }
        ast::ExpKind::Match(exp_inner, pattern) => {
            eval_match_exp(runner_ctx, ctx, exp_inner, pattern)
        }
        ast::ExpKind::Tuple(exps) => eval_tuple_exp(runner_ctx, ctx, span, typ, exps),
        ast::ExpKind::Case(not_exp) => eval_case_exp(runner_ctx, ctx, span, typ, not_exp),
        ast::ExpKind::Str(exp_fields) => eval_str_exp(runner_ctx, ctx, span, typ, exp_fields),
        ast::ExpKind::Opt(exp) => eval_opt_exp(runner_ctx, ctx, span, typ, exp),
        ast::ExpKind::List(exps) => eval_list_exp(runner_ctx, ctx, span, typ, exps),
        ast::ExpKind::Cons(exp_head, exp_tail) => {
            eval_cons_exp(runner_ctx, ctx, span, typ, exp_head, exp_tail)
        }
        ast::ExpKind::Cat(exp_l, exp_r) => eval_cat_exp(runner_ctx, ctx, span, typ, exp_l, exp_r),
        ast::ExpKind::Mem(exp_elem, exp_list) => {
            eval_mem_exp(runner_ctx, ctx, span, exp_elem, exp_list)
        }
        ast::ExpKind::Len(exp_inner) => eval_len_exp(runner_ctx, ctx, exp_inner),
        ast::ExpKind::Dot(exp_base, atom) => eval_dot_exp(runner_ctx, ctx, span, exp_base, atom),
        ast::ExpKind::Idx(exp_base, exp_idx) => eval_idx_exp(runner_ctx, ctx, exp_base, exp_idx),
        ast::ExpKind::Slice(exp_base, exp_idx, exp_len) => {
            eval_slice_exp(runner_ctx, ctx, span, typ, exp_base, exp_idx, exp_len)
        }
        ast::ExpKind::Upd(exp_base, path, exp_upd) => {
            eval_upd_exp(runner_ctx, ctx, exp_base, path, exp_upd)
        }
        ast::ExpKind::Call(id, targs, args) => eval_call_exp(runner_ctx, ctx, id, targs, args),
        ast::ExpKind::Iter(exp_inner, exp_iter) => {
            eval_iter_exp(runner_ctx, ctx, exp, exp_inner, exp_iter)
        }
    })();
    result.nest(exp.span.clone(), || {
        ErrorKind::Trace(crate::interp::shared::error::TraceErrorKind::Evaluation {
            text: Print::to_string(exp),
        })
    })
}

pub(crate) fn eval_exps<
    'global,
    T: Borrow<ast::Exp>,
    Interp: Invoker<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    exps: &[T],
) -> Backtrack<Vec<Value>> {
    let mut values = Vec::with_capacity(exps.len());
    for exp in exps {
        values.push(unwrap!(eval_exp(runner_ctx, ctx, exp.borrow())));
    }
    ok!(values)
}

// - Identifier expression

/// Reads the value bound to the variable's slot.
fn eval_id_exp(ctx: &impl ReadContext, span: &Span, id: &IdSlot) -> Backtrack<Value> {
    let value = *unwrap_from_result!(
        ctx.find_value_at_slot(id.slot).ok_or_else(|| {
            Error::undefined(EntityKind::Value, id.id.node.clone(), id.id.span.clone())
        }),
        span
    );
    ok!(value)
}

// - Unary expression

fn eval_un_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    op: &ast::UnOp,
    exp_inner: &ast::Exp,
) -> Backtrack<Value> {
    let value = unwrap!(eval_exp(runner_ctx, ctx, exp_inner));
    ops::unop(runner_ctx.arena_mut(), span, op, value)
}

// - Binary expression

fn eval_bin_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    op: &ast::BinOp,
    exp_l: &ast::Exp,
    exp_r: &ast::Exp,
) -> Backtrack<Value> {
    let value_l = unwrap!(eval_exp(runner_ctx, ctx, exp_l));
    let value_r = unwrap!(eval_exp(runner_ctx, ctx, exp_r));
    ops::binop(runner_ctx.arena_mut(), span, op, value_l, value_r)
}

// - Comparison expression

fn eval_cmp_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    op: &ast::CmpOp,
    exp_l: &ast::Exp,
    exp_r: &ast::Exp,
) -> Backtrack<Value> {
    let value_l = unwrap!(eval_exp(runner_ctx, ctx, exp_l));
    let value_r = unwrap!(eval_exp(runner_ctx, ctx, exp_r));
    let result = unwrap!(ops::cmpop(runner_ctx.arena(), span, op, value_l, value_r));
    let value =
        unwrap_from_result!(make::bool(runner_ctx.arena_mut(), result, Span::default()), span);
    ok!(value)
}

// - Upcast expression

fn eval_upcast_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    typ: &ast::Typ,
    exp_inner: &ast::Exp,
) -> Backtrack<Value> {
    let value = unwrap!(eval_exp(runner_ctx, ctx, exp_inner));
    ops::cast_up(runner_ctx.arena_mut(), ctx, typ, value)
}

// - Downcast expression

fn eval_downcast_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    typ: &ast::Typ,
    exp_inner: &ast::Exp,
) -> Backtrack<Value> {
    let value = unwrap!(eval_exp(runner_ctx, ctx, exp_inner));
    ops::cast_down(runner_ctx.arena_mut(), ctx, typ, value)
}

// - Subtype check expression

fn eval_sub_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    exp_inner: &ast::Exp,
    subcheck: &ast::Subcheck,
) -> Backtrack<Value> {
    let value = unwrap!(eval_exp(runner_ctx, ctx, exp_inner));
    let matches = unwrap!(ops::sub(runner_ctx.arena(), ctx, span, subcheck, value));
    let value =
        unwrap_from_result!(make::bool(runner_ctx.arena_mut(), matches, Span::default()), span);
    ok!(value)
}

// - Match expression

fn eval_match_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    exp_inner: &ast::Exp,
    pattern: &ast::Pattern,
) -> Backtrack<Value> {
    let value = unwrap!(eval_exp(runner_ctx, ctx, exp_inner));
    let matches = ops::r#match(runner_ctx.arena(), pattern, value);
    let value = unwrap_from_result!(
        make::bool(runner_ctx.arena_mut(), matches, Span::default()),
        &Span::default()
    );
    ok!(value)
}

// - Tuple expression

fn eval_tuple_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    typ: &Rc<ast::TypKind>,
    exps: &[ast::Exp],
) -> Backtrack<Value> {
    let values = unwrap!(eval_exps(runner_ctx, ctx, exps));
    let value = unwrap_from_result!(
        make::tuple(runner_ctx.arena_mut(), typ.clone(), values, Span::default()),
        span
    );
    ok!(value)
}

// - Case expression

/// Evaluates the arguments in notation order and rebuilds the case value.
fn eval_case_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    typ: &Rc<ast::TypKind>,
    not_exp: &ast::NotExp,
) -> Backtrack<Value> {
    // Evaluate and rebuild in one traversal, preserving early failure and order
    let case = match not_exp.try_map(|exp| match eval_exp(runner_ctx, ctx, exp) {
        ok!(value) => Ok(value),
        err!(errors) => Err(err!(errors)),
        unmatch!(errors) => Err(unmatch!(errors)),
    }) {
        Ok(case) => case,
        Err(result) => return result,
    };
    let value = unwrap_from_result!(
        make::case(runner_ctx.arena_mut(), typ.clone(), case, Span::default()),
        span
    );
    ok!(value)
}

// - Struct expression

fn eval_str_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    typ: &Rc<ast::TypKind>,
    exp_fields: &[ast::ExpField],
) -> Backtrack<Value> {
    let mut value_fields = Vec::with_capacity(exp_fields.len());
    for ast::ExpField { atom, exp } in exp_fields {
        value_fields.push((atom.clone(), unwrap!(eval_exp(runner_ctx, ctx, exp))));
    }
    let value = unwrap_from_result!(
        make::structure(runner_ctx.arena_mut(), typ.clone(), value_fields, Span::default()),
        span
    );
    ok!(value)
}

// - Optional expression

fn eval_opt_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    typ: &Rc<ast::TypKind>,
    exp: &Option<Box<ast::Exp>>,
) -> Backtrack<Value> {
    let value = match exp {
        Some(exp) => Some(unwrap!(eval_exp(runner_ctx, ctx, exp))),
        None => None,
    };
    let value = unwrap_from_result!(
        make::opt(runner_ctx.arena_mut(), typ.clone(), value, Span::default()),
        span
    );
    ok!(value)
}

// - List expression

fn eval_list_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    typ: &Rc<ast::TypKind>,
    exps: &[ast::Exp],
) -> Backtrack<Value> {
    let values = unwrap!(eval_exps(runner_ctx, ctx, exps));
    let value = unwrap_from_result!(
        make::list(runner_ctx.arena_mut(), typ.clone(), values, Span::default()),
        span
    );
    ok!(value)
}

// - Cons expression

fn eval_cons_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    typ: &Rc<ast::TypKind>,
    exp_head: &ast::Exp,
    exp_tail: &ast::Exp,
) -> Backtrack<Value> {
    let value_head = unwrap!(eval_exp(runner_ctx, ctx, exp_head));
    let value_tail = unwrap!(eval_exp(runner_ctx, ctx, exp_tail));
    // Prepend the head to the evaluated tail
    let values_tail = unwrap_from_result!(get::list(runner_ctx.arena(), &value_tail), span);
    let mut values = Vec::with_capacity(values_tail.len() + 1);
    values.push(value_head);
    values.extend_from_slice(values_tail);
    let value = unwrap_from_result!(
        make::list(runner_ctx.arena_mut(), typ.clone(), values, Span::default()),
        span
    );
    ok!(value)
}

// - Concatenation expression

/// Concatenates two texts or two lists.
fn eval_cat_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    typ: &Rc<ast::TypKind>,
    exp_l: &ast::Exp,
    exp_r: &ast::Exp,
) -> Backtrack<Value> {
    let value_l = unwrap!(eval_exp(runner_ctx, ctx, exp_l));
    let value_r = unwrap!(eval_exp(runner_ctx, ctx, exp_r));
    let value = match (runner_ctx.arena().kind(&value_l), runner_ctx.arena().kind(&value_r)) {
        // Texts concatenate
        (ValueKind::Text(text_l), ValueKind::Text(text_r)) => {
            let text = format!("{text_l}{text_r}");
            unwrap_from_result!(make::text(runner_ctx.arena_mut(), text, Span::default()), span)
        }
        // Lists concatenate
        (ValueKind::List(values_l), ValueKind::List(values_r)) => {
            let mut values = values_l.clone();
            values.extend_from_slice(values_r);
            unwrap_from_result!(
                make::list(runner_ctx.arena_mut(), typ.clone(), values, Span::default()),
                span
            )
        }
        // Mixed operands are an error
        _ => {
            return err!(
                [&exp_l, &exp_r].at(),
                ErrorKind::Expr(ExprErrorKind::ConcatenationOperandMismatch),
            );
        }
    };
    ok!(value)
}

// - Membership expression

fn eval_mem_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    exp_elem: &ast::Exp,
    exp_list: &ast::Exp,
) -> Backtrack<Value> {
    let value_elem = unwrap!(eval_exp(runner_ctx, ctx, exp_elem));
    let value_list = unwrap!(eval_exp(runner_ctx, ctx, exp_list));
    let contains = unwrap!(ops::mem(runner_ctx.arena(), span, value_elem, value_list));
    let value =
        unwrap_from_result!(make::bool(runner_ctx.arena_mut(), contains, Span::default()), span);
    ok!(value)
}

// - Length expression

/// Length of a text in bytes or of a list in elements.
fn eval_len_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    exp_inner: &ast::Exp,
) -> Backtrack<Value> {
    let value = unwrap!(eval_exp(runner_ctx, ctx, exp_inner));
    let len = match runner_ctx.arena().kind(&value) {
        // Texts count bytes, lists count elements; anything else is an error
        ValueKind::Text(text) => text.len(),
        ValueKind::List(values) => values.len(),
        _ => {
            return err!(
                exp_inner.span.clone(),
                ErrorKind::Expr(ExprErrorKind::LengthOperandMismatch),
            );
        }
    };
    let value = unwrap_from_result!(
        make::nat(runner_ctx.arena_mut(), (len as u64).into(), Span::default()),
        &Span::default()
    );
    ok!(value)
}

// - Field access expression

fn eval_dot_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    exp_base: &ast::Exp,
    atom: &ast::Atom,
) -> Backtrack<Value> {
    let value = unwrap!(eval_exp(runner_ctx, ctx, exp_base));
    ops::access_dot(runner_ctx.arena(), &value, atom, span)
}

// - Index expression

fn eval_idx_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    exp_base: &ast::Exp,
    exp_idx: &ast::Exp,
) -> Backtrack<Value> {
    let value = unwrap!(eval_exp(runner_ctx, ctx, exp_base));
    let value_idx = unwrap!(eval_exp(runner_ctx, ctx, exp_idx));
    ops::access_index(runner_ctx.arena_mut(), &value, &value_idx, &exp_base.span, &exp_idx.span)
}

// - Slice expression

fn eval_slice_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    span: &Span,
    typ: &Rc<ast::TypKind>,
    exp_base: &ast::Exp,
    exp_idx: &ast::Exp,
    exp_len: &ast::Exp,
) -> Backtrack<Value> {
    let value = unwrap!(eval_exp(runner_ctx, ctx, exp_base));
    let value_idx = unwrap!(eval_exp(runner_ctx, ctx, exp_idx));
    let value_len = unwrap!(eval_exp(runner_ctx, ctx, exp_len));
    // Text slices report bounds errors at the index, lists at the length
    let span_bounds = if matches!(runner_ctx.arena().kind(&value), ValueKind::Text(_)) {
        &exp_idx.span
    } else {
        &exp_len.span
    };
    ops::access_slice(
        runner_ctx.arena_mut(),
        &value,
        &value_idx,
        &value_len,
        typ,
        span,
        &exp_base.span,
        &exp_idx.span,
        &exp_len.span,
        span_bounds,
    )
}

// - Update expression

fn eval_upd_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    exp_base: &ast::Exp,
    path: &ast::Path,
    exp_upd: &ast::Exp,
) -> Backtrack<Value> {
    let value_base = unwrap!(eval_exp(runner_ctx, ctx, exp_base));
    let value_upd = unwrap!(eval_exp(runner_ctx, ctx, exp_upd));
    eval_update_path(runner_ctx, ctx, &value_base, path, value_upd)
}

// - Call expression

/// Substitutes local plain type aliases into type arguments before a call.
pub(crate) fn resolve_targs(
    ctx: &impl ReadContext,
    targs: &[ast::Typ],
) -> Result<Vec<ast::Typ>, TypeError> {
    // Only unparameterized plain aliases in local scope are substituted
    let find_subst = |id: &ast::Id| match ctx.find_typdef_local_opt(id)? {
        TypeDef::Defined(tparams, def_typ) if tparams.is_empty() => match &def_typ.node {
            ast::DefTypKind::Plain(typ) => Some(typ),
            _ => None,
        },
        _ => None,
    };
    targs
        .iter()
        .map(|targ| subst_typ(&find_subst, targ))
        .collect()
}

/// Resolves type arguments, evaluates arguments, and invokes the function.
fn eval_call_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    id: &ast::Id,
    targs: &[ast::Typ],
    args: &[ast::Arg],
) -> Backtrack<Value> {
    let targs_subst = unwrap_from_result!(resolve_targs(ctx, targs), &id.span);
    let values = unwrap!(eval_args(runner_ctx, ctx, args));
    Interp::invoke_func(runner_ctx, ctx, id, &targs_subst, &values)
}

// - Iteration expression

/// Evaluates an iterated expression; a bare iterated variable reads its slot.
fn eval_iter_exp<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    exp: &ast::Exp,
    exp_inner: &ast::Exp,
    exp_iter: &ast::ExpIter,
) -> Backtrack<Value> {
    let span = &exp.span;
    let typ = &exp.note;
    // `x*` as an expression is just the bound value
    if let Some(slot) = find_slot_of_exp(ctx, exp) {
        return ok!(*unwrap_from_result!(
            ctx.find_value_at_slot(slot).ok_or_else(|| {
                let var = find_var_of_exp(ctx, exp).expect("identity iteration has a variable");
                Error::undefined(
                    EntityKind::Value,
                    Print::to_string(&var.var),
                    var.var.id.span.clone(),
                )
            }),
            span
        ));
    }
    // Otherwise map the body over the iterated variables
    iter::map(runner_ctx, ctx, span, typ, exp_iter, |runner_ctx, ctx_sub| {
        eval_exp(runner_ctx, ctx_sub, exp_inner)
    })
}
