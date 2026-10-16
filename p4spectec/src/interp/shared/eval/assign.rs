//! Destructuring assignments preserve iteration paths and isolate list rows
//!
//! `assign_exp` matches a value against a binder pattern
//! and binds its variables:
//! `(x, y) <- (1, 2)` binds `x` and `y`;
//! `x* <- [1, 2]` binds `x*` as a whole,
//! while `(x, y)* <- [...]` assigns each row in a fresh sub-context
//! and gathers the rows into `x*` and `y*`.

use std::{borrow::Borrow, rc::Rc};

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value, ValueKind},
    },
    traits::at::At,
};

use crate::lang::il::prepared as ast;

use crate::interp::shared::{
    backtrack::{Backtrack, ok, unwrap, unwrap_from_result},
    util::{find_slot_of_exp, iterate_vars},
};

use super::super::context::WriteContext;

// = Type parameter assignment

/// Binds type arguments in a local scope, requiring equal counts.
#[inline(always)]
pub fn assign_tparams<Ctx: WriteContext>(
    mut ctx: Ctx,
    tparams: &[ast::TParam],
    targs: &[ast::Typ],
    span: &Span,
) -> Backtrack<Ctx> {
    // Check arity before binding any type parameter
    assert_eq!(tparams.len(), targs.len(), "type argument arity mismatch at {span}");
    // Reuse immutable scopes when the context has no local type bindings
    unwrap!(ctx.bind_tparams(tparams, targs));
    ok!(ctx)
}

// = Expression assignment

/// Matches `value` against the pattern `exp`, binding its variables.
#[inline]
pub fn assign_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    mut ctx: Ctx,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<Ctx> {
    unwrap!(assign_in_place(arena, &mut ctx, exp, value));
    ok!(ctx)
}

/// Destructuring updates one owned frame without moving the context per child.
#[inline(always)]
pub(crate) fn assign_in_place<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<()> {
    if let Some(slot) = find_slot_of_exp(ctx, exp) {
        ctx.add_value_at_slot(slot, value);
        return ok!(());
    }
    assign_composite(arena, ctx, exp, value)
}

/// Destructures a composite pattern, borrowing one child handle at a time.
fn assign_composite<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<()> {
    match (&exp.node, arena.value.kind(value.node)) {
        (ast::ExpKind::Tuple(exps), ValueKind::Tuple(_))
        | (ast::ExpKind::List(exps), ValueKind::List(_)) => {
            assign_children(arena, ctx, exps.iter(), value)
        }
        (ast::ExpKind::Case(not_exp), ValueKind::Case(_)) => {
            assign_children(arena, ctx, not_exp.args().iter(), value)
        }
        (ast::ExpKind::Str(exp_fields), ValueKind::Struct(_)) => {
            assign_children(arena, ctx, exp_fields.iter().map(|field| &field.exp), value)
        }
        (ast::ExpKind::Opt(exp_opt), ValueKind::Opt(value_opt)) => {
            let value_opt = *value_opt;
            assign_opt_exp(arena, ctx, exp_opt, &value_opt)
        }
        (ast::ExpKind::Cons(exp_head, exp_tail), ValueKind::List(_)) => {
            assign_cons_exp(arena, ctx, exp_head, exp_tail, value)
        }
        (ast::ExpKind::Iter(exp_inner, exp_iter), _) => {
            assign_iter_exp(arena, ctx, exp, exp_inner, exp_iter, value)
        }
        // Lowering checks the pattern before destructuring it
        _ => unreachable!("assignment pattern must match the value"),
    }
}

/// Assigns fixed children without allocating a temporary copy of their handles.
fn assign_children<'exp, Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exps: impl ExactSizeIterator<Item = &'exp ast::Exp>,
    value: Value,
) -> Backtrack<()> {
    let len = match arena.value.kind(value.node) {
        ValueKind::Tuple(values) | ValueKind::List(values) => values.len(),
        ValueKind::Case(value_case) => value_case.args().len(),
        ValueKind::Struct(value_fields) => value_fields.len(),
        _ => unreachable!("fixed assignment children"),
    };
    assert_eq!(exps.len(), len, "assignment arity mismatch");
    for (idx, exp) in exps.enumerate() {
        // Recursive assignment may grow the arena, so reacquire each child
        let value_inner = match arena.value.kind(value.node) {
            ValueKind::Tuple(values) | ValueKind::List(values) => values[idx],
            ValueKind::Case(value_case) => value_case.args()[idx],
            ValueKind::Struct(value_fields) => value_fields[idx].value,
            _ => unreachable!("fixed assignment children"),
        };
        unwrap!(assign_in_place(arena, ctx, exp, value_inner));
    }
    ok!(())
}

/// Assigns values to patterns pairwise, requiring equal counts.
pub fn assign_exps<Ctx: WriteContext, T: Borrow<ast::Exp> + At>(
    arena: &mut Arena,
    mut ctx: Ctx,
    exps: &[T],
    values: &[Value],
) -> Backtrack<Ctx> {
    // Counts must match
    assert_eq!(exps.len(), values.len(), "assignment arity mismatch");
    for (exp, value) in exps.iter().zip(values) {
        unwrap!(assign_in_place(arena, &mut ctx, exp.borrow(), *value));
    }
    ok!(ctx)
}

// - Optional expression

/// Assigns an option: a payload to a payload, absence to absence.
fn assign_opt_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp_opt: &Option<Box<ast::Exp>>,
    value_opt: &Option<Value>,
) -> Backtrack<()> {
    match (exp_opt, value_opt) {
        // Both present: assign the payload
        (Some(exp), Some(value)) => assign_in_place(arena, ctx, exp, *value),
        // Both absent: nothing to bind
        (None, None) => ok!(()),
        // Lowering checks optionality before destructuring
        _ => unreachable!("option pattern must match the value"),
    }
}

// - Cons expression

/// Splits a non-empty list into head and tail and assigns each.
fn assign_cons_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp_head: &ast::Exp,
    exp_tail: &ast::Exp,
    value: Value,
) -> Backtrack<()> {
    let value_head = *value::get::list(arena, &value)
        .expect("list tail")
        .first()
        .expect("cons pattern must match a non-empty list");
    let value_tail =
        unwrap_from_result!(arena.value.list_tail(&arena.mixop, value), &Span::default());
    unwrap!(assign_in_place(arena, ctx, exp_head, value_head));
    assign_in_place(arena, ctx, exp_tail, value_tail)
}

// - Iteration expression

/// Assigns an iterated pattern, binding its variables one iteration outward.
fn assign_iter_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp: &ast::Exp,
    exp_inner: &ast::Exp,
    exp_iter: &ast::ExpIter,
    value: Value,
) -> Backtrack<()> {
    // A bare iterated variable binds as a whole
    if let Some(slot) = find_slot_of_exp(ctx, exp) {
        ctx.add_value_at_slot(slot, value);
        return ok!(());
    }
    // Otherwise assign each element in a sub-context and gather per variable
    let span = &exp.span;
    let vars_outer = iterate_vars(ctx, &exp_iter.vars, exp_iter.iter);
    match exp_iter.iter {
        // Option: assign the payload once, or bind every variable to none
        ast::Iter::Opt => {
            let value_opt = value::get::opt(arena, &value)
                .expect("iteration assignment value must be an option");
            let ctx_sub = match value_opt {
                Some(value) => Some(unwrap!(assign_exp(arena, ctx.clone(), exp_inner, value))),
                None => None,
            };
            for (var, var_outer) in exp_iter.vars.iter().zip(&vars_outer) {
                let typ = Rc::clone(&var_outer.typ);
                let value_opt = ctx_sub.as_ref().map(|ctx_sub| {
                    *ctx_sub
                        .find_value_at_slot(var.slot)
                        .expect("value must be bound")
                });
                let value = unwrap_from_result!(
                    value::make::opt(arena, typ, value_opt, Span::default()),
                    span
                );
                ctx.add_value_at_slot(var_outer.slot, value);
            }
            ok!(())
        }
        // List: one fresh sub-context per element
        ast::Iter::List => {
            if let Some(values) =
                super::rows::collect(arena, ctx, exp_inner, &exp_iter.vars, &vars_outer, value)
            {
                let values = unwrap_from_result!(values, span);
                for (var, value) in vars_outer.iter().zip(values) {
                    ctx.add_value_at_slot(var.slot, value);
                }
                return ok!(());
            }
            // Nested iterations and constructed tails keep isolated row scopes
            let values = value::get::list(arena, &value)
                .expect("iteration assignment value must be a list")
                .to_vec();
            let mut ctx_sub = ctx.clone();
            let mut values_by_var: Vec<_> = (0..exp_iter.vars.len())
                .map(|_| Vec::with_capacity(values.len()))
                .collect();
            for value in values {
                ctx_sub.clear_value_bindings();
                ctx_sub = unwrap!(assign_exp(arena, ctx_sub, exp_inner, value));
                for (var, values) in exp_iter.vars.iter().zip(&mut values_by_var) {
                    values.push(
                        *ctx_sub
                            .find_value_at_slot(var.slot)
                            .expect("value must be bound"),
                    );
                }
            }
            // Each variable collects its per-row values into a list
            for (var_outer, values) in vars_outer.iter().zip(values_by_var) {
                let typ = Rc::clone(&var_outer.typ);
                let value_sub = unwrap_from_result!(
                    value::make::list(arena, typ, values, Span::default()),
                    span
                );
                ctx.add_value_at_slot(var_outer.slot, value_sub);
            }
            ok!(())
        }
    }
}

// = Argument assignment

/// Assigns an argument value: to a pattern, or to a function parameter's slot.
pub fn assign_arg<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: Ctx,
    arg: &ast::Arg,
    value: Value,
) -> Backtrack<Ctx> {
    match &arg.node {
        ast::ArgKind::Exp(exp) => assign_exp_arg(arena, ctx, exp, value),
        ast::ArgKind::Def(id) => assign_def(ctx, id, value),
    }
}

/// Assigns values to arguments pairwise, requiring equal counts.
pub fn assign_args<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: Ctx,
    args: &[ast::Arg],
    values: &[Value],
) -> Backtrack<Ctx> {
    // Counts must match
    assert_eq!(args.len(), values.len(), "validated argument assignment arity");
    let mut ctx = ctx;
    for (arg, value) in args.iter().zip(values.iter()) {
        ctx = unwrap!(assign_arg(arena, ctx, arg, *value));
    }
    ok!(ctx)
}

// - Expression argument

fn assign_exp_arg<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: Ctx,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<Ctx> {
    assign_exp(arena, ctx, exp, value)
}

// - Function argument

/// Binds a function argument, a value naming a global definition, to its parameter's slot.
pub fn assign_def<Ctx: WriteContext>(mut ctx: Ctx, id: &ast::Id, value: Value) -> Backtrack<Ctx> {
    assign_def_in_place(&mut ctx, id, value);
    ok!(ctx)
}

/// Binds a function argument without moving the callee's frame.
pub(crate) fn assign_def_in_place<Ctx: WriteContext>(ctx: &mut Ctx, id: &ast::Id, value: Value) {
    let slot = ctx
        .find_func_slot(id)
        .expect("function parameter must have a prepared slot");
    ctx.add_value_at_slot(slot, value);
}
