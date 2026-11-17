//! Destructuring assignments preserve iteration paths and isolate list rows
//!
//! `assign_exp` matches a value against a binder pattern
//! and binds its variables:
//! `(x, y) <- (1, 2)` binds `x` and `y`;
//! `x* <- [1, 2]` binds `x*` as a whole,
//! while `(x, y)* <- [...]` assigns each row in a fresh sub-context
//! and gathers the rows into `x*` and `y*`.

use std::{borrow::Borrow, rc::Rc};

use smallvec::SmallVec;

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value, ValueKind},
        var::IdSlot,
    },
    traits::at::At,
};

use crate::lang::il::prepared as ast;

use crate::runtime::envs::interp::shared::TypeDef;

use crate::interp::shared::{
    backtrack::{Backtrack, ok, unwrap, unwrap_from_result},
    util::{find_slot_of_exp, iterate_vars},
};

use crate::phrase;

use super::super::context::WriteContext;

/// Child handles copied out of the arena, which recursive assignment may grow.
type Values = SmallVec<[Value; 8]>;

// = Type parameter assignment

/// Binds type arguments in a local scope, requiring equal counts.
pub fn assign_tparams<Ctx: WriteContext>(
    mut ctx: Ctx,
    tparams: &[ast::TParam],
    targs: &[ast::Typ],
    span: &Span,
) -> Backtrack<Ctx> {
    // Check arity before binding any type parameter
    assert_eq!(tparams.len(), targs.len(), "type argument arity mismatch at {span}");
    // Type arguments shadow global definitions in the callee scope
    for (tparam, targ) in tparams.iter().zip(targs) {
        let def_typ = phrase!(node: ast::DefTypKind::Plain(targ.clone()), span: targ.span);
        unwrap_from_result!(
            ctx.add_typdef_local(tparam.clone(), TypeDef::Defined(vec![], Box::new(def_typ))),
            &tparam.span
        );
    }
    ok!(ctx)
}

// = Expression assignment

/// Matches `value` against the pattern `exp`, binding its variables.
pub fn assign_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<()> {
    match (&exp.node, arena.value.kind(value.node)) {
        // A variable binds directly
        (ast::ExpKind::Id(id), _) => assign_id_exp(arena, ctx, id, value),
        // Tuple: componentwise
        (ast::ExpKind::Tuple(exps), ValueKind::Tuple(values)) => {
            let values = Values::from_slice(values);
            assign_tuple_exp(arena, ctx, exps, &values)
        }
        // Case: the arguments
        (ast::ExpKind::Case(not_exp), ValueKind::Case(value_case)) => {
            let values = Values::from_slice(value_case.args());
            assign_case_exp(arena, ctx, not_exp, &values)
        }
        // Struct: the fields in order
        (ast::ExpKind::Str(exp_fields), ValueKind::Struct(value_fields)) => {
            let values = value_fields
                .iter()
                .map(|value_field| value_field.value)
                .collect::<Values>();
            assign_str_exp(arena, ctx, exp_fields, &values)
        }
        // Option: both present or both absent
        (ast::ExpKind::Opt(exp_opt), ValueKind::Opt(value_opt)) => {
            let value_opt = *value_opt;
            assign_opt_exp(arena, ctx, exp_opt, &value_opt)
        }
        // List literal: elementwise
        (ast::ExpKind::List(exps), ValueKind::List(values)) => {
            let values = Values::from_slice(values);
            assign_list_exp(arena, ctx, exps, &values)
        }
        // Cons: the first element, then the rest
        (ast::ExpKind::Cons(exp_head, exp_tail), ValueKind::List(_)) => {
            assign_cons_exp(arena, ctx, exp_head, exp_tail, value)
        }
        // Iteration: as a whole or row by row
        (ast::ExpKind::Iter(exp_inner, exp_iter), _) => {
            assign_iter_exp(arena, ctx, exp, exp_inner, exp_iter, value)
        }
        // Lowering checks the pattern before destructuring it
        _ => unreachable!("assignment pattern must match the value"),
    }
}

/// Assigns values to patterns pairwise, requiring equal counts.
pub fn assign_exps<Ctx: WriteContext, T: Borrow<ast::Exp> + At>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exps: &[T],
    values: &[Value],
) -> Backtrack<()> {
    // Counts must match
    assert_eq!(exps.len(), values.len(), "assignment arity mismatch");
    for (exp, value) in exps.iter().zip(values) {
        unwrap!(assign_exp(arena, ctx, exp.borrow(), *value));
    }
    ok!(())
}

// - Identifier expression

fn assign_id_exp<Ctx: WriteContext>(
    _arena: &mut Arena,
    ctx: &mut Ctx,
    id: &IdSlot,
    value: Value,
) -> Backtrack<()> {
    ctx.add_value_at_slot(id.slot, value);
    ok!(())
}

// - Tuple expression

fn assign_tuple_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exps: &[ast::Exp],
    values: &[Value],
) -> Backtrack<()> {
    assign_exps(arena, ctx, exps, values)
}

// - Case expression

fn assign_case_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    not_exp: &ast::NotExp,
    values: &[Value],
) -> Backtrack<()> {
    assign_exps(arena, ctx, not_exp.args(), values)
}

// - Struct expression

/// Assigns the field values to the field patterns in order.
fn assign_str_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp_fields: &[ast::ExpField],
    values: &[Value],
) -> Backtrack<()> {
    // Counts must match
    assert_eq!(exp_fields.len(), values.len(), "assignment arity mismatch");
    for (exp_field, value) in exp_fields.iter().zip(values) {
        unwrap!(assign_exp(arena, ctx, &exp_field.exp, *value));
    }
    ok!(())
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
        (Some(exp), Some(value)) => assign_exp(arena, ctx, exp, *value),
        // Both absent: nothing to bind
        (None, None) => ok!(()),
        // Lowering checks optionality before destructuring
        _ => unreachable!("option pattern must match the value"),
    }
}

// - List expression

fn assign_list_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exps: &[ast::Exp],
    values: &[Value],
) -> Backtrack<()> {
    assign_exps(arena, ctx, exps, values)
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
        .expect("cons pattern must match a list")
        .first()
        .expect("cons pattern must match a non-empty list");
    // Reuse the tail body the arena derived from this list
    let value_tail =
        unwrap_from_result!(arena.value.list_tail(&arena.mixop, value), &Span::default());
    unwrap!(assign_exp(arena, ctx, exp_head, value_head));
    assign_exp(arena, ctx, exp_tail, value_tail)
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
    if let Some(slot) = find_slot_of_exp(exp) {
        ctx.add_value_at_slot(slot, value);
        return ok!(());
    }
    // Otherwise assign each element and gather per variable
    match exp_iter.iter {
        // Option: the payload once, or none for every variable
        ast::Iter::Opt => assign_iter_opt_exp(arena, ctx, exp, exp_inner, exp_iter, value),
        // List: one row per element
        ast::Iter::List => assign_iter_list_exp(arena, ctx, exp, exp_inner, exp_iter, value),
    }
}

/// Assigns an optional payload in a sub-context, or binds every variable to none.
fn assign_iter_opt_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp: &ast::Exp,
    exp_inner: &ast::Exp,
    exp_iter: &ast::ExpIter,
    value: Value,
) -> Backtrack<()> {
    let span = &exp.span;
    let vars_outer = iterate_vars(&exp_iter.vars);
    // Assign the payload, if any, in a copy of the context
    let value_opt =
        value::get::opt(arena, &value).expect("iteration assignment value must be an option");
    let ctx_sub = match value_opt {
        Some(value) => {
            let mut ctx_sub = ctx.clone();
            unwrap!(assign_exp(arena, &mut ctx_sub, exp_inner, value));
            Some(ctx_sub)
        }
        None => None,
    };
    // Each variable becomes an option of its payload binding
    for (var, var_outer) in exp_iter.vars.iter().zip(&vars_outer) {
        let typ = Rc::clone(&var_outer.typ);
        let value_opt = ctx_sub.as_ref().map(|ctx_sub| {
            *ctx_sub
                .find_value_at_slot(var.slot)
                .expect("value must be bound")
        });
        let value =
            unwrap_from_result!(value::make::opt(arena, typ, value_opt, Span::default()), span);
        ctx.add_value_at_slot(var_outer.slot, value);
    }
    ok!(())
}

/// Assigns each list element as one row and gathers the rows per variable.
fn assign_iter_list_exp<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp: &ast::Exp,
    exp_inner: &ast::Exp,
    exp_iter: &ast::ExpIter,
    value: Value,
) -> Backtrack<()> {
    let span = &exp.span;
    let vars_outer = iterate_vars(&exp_iter.vars);
    // Fixed patterns project the rows into columns directly
    if let Some(values) =
        super::rows::collect(arena, ctx, exp_inner, &exp_iter.vars, &vars_outer, value)
    {
        let values = unwrap_from_result!(values, span);
        for (var_outer, value) in vars_outer.iter().zip(values) {
            ctx.add_value_at_slot(var_outer.slot, value);
        }
        return ok!(());
    }
    // Nested iterations and constructed tails keep isolated row scopes
    let values_by_var = unwrap!(assign_rows(arena, ctx, exp_inner, &exp_iter.vars, value));
    // Each variable collects its per-row values into a list
    for (var_outer, values) in vars_outer.iter().zip(values_by_var) {
        let typ = Rc::clone(&var_outer.typ);
        let value_sub =
            unwrap_from_result!(value::make::list(arena, typ, values, Span::default()), span);
        ctx.add_value_at_slot(var_outer.slot, value_sub);
    }
    ok!(())
}

/// Assigns each list element in a cleared copy of the context,
/// returning every variable's values in row order.
fn assign_rows<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &Ctx,
    exp_inner: &ast::Exp,
    vars: &[ast::Var],
    value: Value,
) -> Backtrack<Vec<Vec<Value>>> {
    let values = value::get::list(arena, &value)
        .expect("iteration assignment value must be a list")
        .to_vec();
    let mut ctx_sub = ctx.clone();
    let mut values_by_var: Vec<_> = (0..vars.len())
        .map(|_| Vec::with_capacity(values.len()))
        .collect();
    for value in values {
        // Each row starts without the previous row's bindings
        ctx_sub.clear_value_bindings();
        unwrap!(assign_exp(arena, &mut ctx_sub, exp_inner, value));
        for (var, values) in vars.iter().zip(&mut values_by_var) {
            values.push(
                *ctx_sub
                    .find_value_at_slot(var.slot)
                    .expect("value must be bound"),
            );
        }
    }
    ok!(values_by_var)
}

// = Argument assignment

/// Assigns an argument value: to a pattern, or to a function parameter's slot.
pub fn assign_arg<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    arg: &ast::Arg,
    value: Value,
) -> Backtrack<()> {
    match &arg.node {
        ast::ArgKind::Exp(exp) => assign_exp_arg(arena, ctx, exp, value),
        ast::ArgKind::Def(id) => assign_def_arg(ctx, id, value),
    }
}

/// Assigns values to arguments pairwise, requiring equal counts.
pub fn assign_args<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    args: &[ast::Arg],
    values: &[Value],
) -> Backtrack<()> {
    // Counts must match
    assert_eq!(args.len(), values.len(), "validated argument assignment arity");
    for (arg, value) in args.iter().zip(values) {
        unwrap!(assign_arg(arena, ctx, arg, *value));
    }
    ok!(())
}

// - Expression argument

fn assign_exp_arg<Ctx: WriteContext>(
    arena: &mut Arena,
    ctx: &mut Ctx,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<()> {
    assign_exp(arena, ctx, exp, value)
}

// - Function argument

/// Binds a function argument, a value naming a global definition, to its parameter.
pub fn assign_def_arg<Ctx: WriteContext>(
    ctx: &mut Ctx,
    id: &ast::Id,
    value: Value,
) -> Backtrack<()> {
    ctx.add_func_param(id, value);
    ok!(())
}
