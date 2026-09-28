//! Destructuring assignments preserve iteration paths and isolate list rows
//!
//! `assign_exp` matches a value against a binder pattern
//! and binds its variables:
//! `(x, y) <- (1, 2)` binds `x` and `y`;
//! `x* <- [1, 2]` binds `x*` as a whole,
//! while `(x, y)* <- [...]` assigns each row in a fresh sub-context
//! and gathers the rows into `x*` and `y*`.

use super::super::context::{ReadContext, WriteContext};
use crate::interp::shared::prepare::ast;
use crate::interp::shared::util::iterate_vars;
use crate::lang::data::var::IdSlot;
use crate::lang::traits::at::At;
use crate::runtime::typdef::TypeDef;

use std::{borrow::Borrow, rc::Rc};

use crate::interp::shared::error::AssignErrorKind;

use crate::{
    lang::{
        common::source::Span,
        data::{
            typ,
            value::{Value, ValueArena, ValueKind, get, make},
        },
        traits::print::Print,
    },
    phrase,
};

use crate::interp::shared::{
    backtrack::{Backtrack, err, ok, unwrap, unwrap_from_result},
    error::{CallErrorKind, EntityKind, Error, ErrorKind},
    util::find_slot_of_exp,
};

// = Type parameter assignment

/// Binds type arguments in a local scope, requiring equal counts.
pub fn assign_tparams<Ctx: WriteContext>(
    mut ctx: Ctx,
    tparams: &[ast::TParam],
    targs: &[ast::Typ],
    span: &Span,
) -> Backtrack<Ctx> {
    // Check arity before binding any type parameter
    unwrap!(Backtrack::check(
        tparams.len() == targs.len(),
        span.clone(),
        ErrorKind::Call(CallErrorKind::TypeArgumentArityMismatch {
            expected: tparams.len(),
            actual: targs.len(),
        })
    ));
    // Type arguments shadow global definitions in the callee scope
    for (tparam, targ) in tparams.iter().zip(targs) {
        let def_typ = phrase!(node: ast::DefTypKind::Plain(targ.clone()), span: targ.span.clone());
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
    arena: &mut ValueArena,
    ctx: Ctx,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<Ctx> {
    match (&exp.node, arena.kind(&value)) {
        // A variable binds directly
        (ast::ExpKind::Id(id), _) => assign_id_exp(arena, ctx, id, value),
        // Tuple: componentwise
        (ast::ExpKind::Tuple(exps), ValueKind::Tuple(values)) => {
            let values = values.to_vec();
            assign_tuple_exp(arena, ctx, exps, &values)
        }
        // Case: the arguments
        (ast::ExpKind::Case(not_exp), ValueKind::Case(value_case)) => {
            let mut values = Vec::new();
            value_case.iter(|value| values.push(*value));
            assign_case_exp(arena, ctx, not_exp, &values)
        }
        // Struct: the fields in order
        (ast::ExpKind::Str(exp_fields), ValueKind::Struct(value_fields)) => {
            let values = value_fields
                .iter()
                .map(|(_, value)| *value)
                .collect::<Vec<_>>();
            assign_str_exp(arena, ctx, exp_fields, &values)
        }
        // Option: both present or both absent
        (ast::ExpKind::Opt(exp_opt), ValueKind::Opt(value_opt)) => {
            let value_opt = *value_opt;
            assign_opt_exp(arena, ctx, exp, exp_opt, &value, &value_opt)
        }
        // List literal: elementwise
        (ast::ExpKind::List(exps), ValueKind::List(values)) => {
            let values = values.to_vec();
            assign_list_exp(arena, ctx, exps, &values)
        }
        // Cons: the first element, then the rest
        (ast::ExpKind::Cons(exp_head, exp_tail), ValueKind::List(values)) => {
            let values = values.to_vec();
            assign_cons_exp(arena, ctx, exp, exp_head, exp_tail, &value, &values)
        }
        // Iteration: as a whole or row by row
        (ast::ExpKind::Iter(exp_inner, exp_iter), _) => {
            assign_iter_exp(arena, ctx, exp, exp_inner, exp_iter, value)
        }
        // Pattern and value shapes disagree
        _ => err!(
            exp.span.clone(),
            ErrorKind::Assign(AssignErrorKind::Mismatch {
                exp: Print::to_string(exp),
                value: arena.to_string(&value),
            }),
        ),
    }
}

/// Assigns values to patterns pairwise, requiring equal counts.
pub fn assign_exps<Ctx: WriteContext, T: Borrow<ast::Exp> + At>(
    arena: &mut ValueArena,
    mut ctx: Ctx,
    exps: &[T],
    values: &[Value],
) -> Backtrack<Ctx> {
    // Counts must match
    if exps.len() != values.len() {
        return err!(
            exps.at(),
            ErrorKind::Assign(AssignErrorKind::ExpressionArityMismatch {
                expected: exps.len(),
                actual: values.len(),
            }),
        );
    }
    for (exp, value) in exps.iter().zip(values) {
        ctx = unwrap!(assign_exp(arena, ctx, exp.borrow(), *value));
    }
    ok!(ctx)
}

// - Identifier expression

fn assign_id_exp<Ctx: WriteContext>(
    _arena: &mut ValueArena,
    mut ctx: Ctx,
    id: &IdSlot,
    value: Value,
) -> Backtrack<Ctx> {
    ctx.add_value_at_slot(id.slot, value);
    ok!(ctx)
}

// - Tuple expression

fn assign_tuple_exp<Ctx: WriteContext>(
    arena: &mut ValueArena,
    ctx: Ctx,
    exps: &[ast::Exp],
    values: &[Value],
) -> Backtrack<Ctx> {
    assign_exps(arena, ctx, exps, values)
}

// - Case expression

fn assign_case_exp<Ctx: WriteContext>(
    arena: &mut ValueArena,
    ctx: Ctx,
    not_exp: &ast::NotExp,
    values: &[Value],
) -> Backtrack<Ctx> {
    let exps = not_exp.args();
    assign_exps(arena, ctx, &exps, values)
}

// - Struct expression

fn assign_str_exp<Ctx: WriteContext>(
    arena: &mut ValueArena,
    ctx: Ctx,
    exp_fields: &[ast::ExpField],
    values: &[Value],
) -> Backtrack<Ctx> {
    let exps = exp_fields
        .iter()
        .map(|ast::ExpField { exp, .. }| exp)
        .collect::<Vec<_>>();
    assign_exps(arena, ctx, &exps, values)
}

// - Optional expression

/// Assigns an option: a payload to a payload, absence to absence.
fn assign_opt_exp<Ctx: WriteContext>(
    arena: &mut ValueArena,
    ctx: Ctx,
    exp: &ast::Exp,
    exp_opt: &Option<Box<ast::Exp>>,
    value: &Value,
    value_opt: &Option<Value>,
) -> Backtrack<Ctx> {
    match (exp_opt, value_opt) {
        // Both present: assign the payload
        (Some(exp), Some(value)) => assign_exp(arena, ctx, exp, *value),
        // Both absent: nothing to bind
        (None, None) => ok!(ctx),
        // One side present: mismatch
        _ => err!(
            exp.span.clone(),
            ErrorKind::Assign(AssignErrorKind::Mismatch {
                exp: Print::to_string(exp),
                value: arena.to_string(value),
            }),
        ),
    }
}

// - List expression

fn assign_list_exp<Ctx: WriteContext>(
    arena: &mut ValueArena,
    ctx: Ctx,
    exps: &[ast::Exp],
    values: &[Value],
) -> Backtrack<Ctx> {
    assign_exps(arena, ctx, exps, values)
}

// - Cons expression

/// Splits a non-empty list into head and tail and assigns each.
fn assign_cons_exp<Ctx: WriteContext>(
    arena: &mut ValueArena,
    ctx: Ctx,
    exp: &ast::Exp,
    exp_head: &ast::Exp,
    exp_tail: &ast::Exp,
    value: &Value,
    values: &[Value],
) -> Backtrack<Ctx> {
    let Some((value_head, values_tail)) = values.split_first() else {
        return err!(exp.span.clone(), ErrorKind::Assign(AssignErrorKind::EmptyCons));
    };
    // Rebuild the tail as a list value of the same type
    let typ = phrase!(node: arena.typ(value).clone(), span: exp.span.clone());
    let value_tail = unwrap_from_result!(
        make::list(arena, typ.node.clone(), values_tail.to_vec(), Span::default()),
        &Span::default()
    );
    let ctx = unwrap!(assign_exp(arena, ctx, exp_head, *value_head));
    assign_exp(arena, ctx, exp_tail, value_tail)
}

// - Iteration expression

/// Assigns an iterated pattern, binding its variables one iteration outward.
fn assign_iter_exp<Ctx: WriteContext>(
    arena: &mut ValueArena,
    mut ctx: Ctx,
    exp: &ast::Exp,
    exp_inner: &ast::Exp,
    exp_iter: &ast::ExpIter,
    value: Value,
) -> Backtrack<Ctx> {
    // A bare iterated variable binds as a whole
    if let Some(slot) = find_slot_of_exp(&ctx, exp) {
        ctx.add_value_at_slot(slot, value);
        return ok!(ctx);
    }
    // Otherwise assign each element in a sub-context and gather per variable
    let span = &exp.span;
    let vars_outer = iterate_vars(&ctx, &exp_iter.vars, exp_iter.iter);
    match exp_iter.iter {
        // Option: assign the payload once, or bind every variable to none
        ast::Iter::Opt => {
            let value_opt = unwrap_from_result!(get::opt(arena, &value), span);
            let ctx_sub = match value_opt {
                Some(value) => Some(unwrap!(assign_exp(arena, ctx.clone(), exp_inner, value))),
                None => None,
            };
            for (var, var_outer) in exp_iter.vars.iter().zip(&vars_outer) {
                let typ = typ::make::iterate(var_outer.var.typ.clone(), &var_outer.var.iters);
                let value_opt = match &ctx_sub {
                    Some(ctx_sub) => Some(*unwrap_from_result!(
                        ctx_sub.find_value_at_slot(var.slot).ok_or_else(|| {
                            Error::undefined(
                                EntityKind::Value,
                                Print::to_string(&var.var),
                                var.var.id.span.clone(),
                            )
                        }),
                        &var.var.id.span
                    )),
                    None => None,
                };
                let value = unwrap_from_result!(
                    make::opt(arena, typ.node.into(), value_opt, Span::default()),
                    span
                );
                ctx.add_value_at_slot(var_outer.slot, value);
            }
            ok!(ctx)
        }
        // List: one fresh sub-context per element
        ast::Iter::List => {
            let values = unwrap_from_result!(get::list(arena, &value), span).to_vec();
            let mut ctx_sub = ctx.clone();
            ctx_sub.clear_value_bindings();
            let mut ctxs = Vec::with_capacity(values.len());
            for value in values {
                ctxs.push(unwrap!(assign_exp(arena, ctx_sub.clone(), exp_inner, value)));
            }
            // Each variable collects its per-row values into a list
            for (var, var_outer) in exp_iter.vars.iter().zip(&vars_outer) {
                let typ = typ::make::iterate(var_outer.var.typ.clone(), &var_outer.var.iters);
                let mut values = Vec::with_capacity(ctxs.len());
                for ctx_sub in &ctxs {
                    let value = unwrap_from_result!(
                        ctx_sub.find_value_at_slot(var.slot).ok_or_else(|| {
                            Error::undefined(
                                EntityKind::Value,
                                Print::to_string(&var.var),
                                var.var.id.span.clone(),
                            )
                        }),
                        &var.var.id.span
                    );
                    values.push(*value);
                }
                let value_sub = unwrap_from_result!(
                    make::list(arena, typ.node.into(), values, Span::default()),
                    span
                );
                ctx.add_value_at_slot(var_outer.slot, value_sub);
            }
            ok!(ctx)
        }
    }
}

// = Argument assignment

/// Assigns an argument value: to a pattern, or as a function definition.
pub fn assign_arg<Ctx: WriteContext>(
    arena: &mut ValueArena,
    ctx_caller: &impl ReadContext<Func = Ctx::Func>,
    ctx_callee: Ctx,
    arg: &ast::Arg,
    value: Value,
) -> Backtrack<Ctx> {
    match &arg.node {
        ast::ArgKind::Exp(exp) => assign_exp_arg(arena, ctx_callee, exp, value),
        ast::ArgKind::Def(id) => assign_def(arena, ctx_caller, ctx_callee, id, value),
    }
}

/// Assigns values to arguments pairwise, requiring equal counts.
pub fn assign_args<Ctx: WriteContext>(
    arena: &mut ValueArena,
    ctx_caller: &impl ReadContext<Func = Ctx::Func>,
    ctx_callee: Ctx,
    args: &[ast::Arg],
    values: &[Value],
) -> Backtrack<Ctx> {
    // Counts must match
    if args.len() != values.len() {
        return err!(
            args.at(),
            ErrorKind::Assign(AssignErrorKind::ArgumentArityMismatch {
                expected: args.len(),
                actual: values.len(),
            }),
        );
    }
    let mut ctx = ctx_callee;
    for (arg, value) in args.iter().zip(values.iter()) {
        ctx = unwrap!(assign_arg(arena, ctx_caller, ctx, arg, *value));
    }
    ok!(ctx)
}

// - Expression argument

fn assign_exp_arg<Ctx: WriteContext>(
    arena: &mut ValueArena,
    ctx: Ctx,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<Ctx> {
    assign_exp(arena, ctx, exp, value)
}

// - Function argument

/// Binds a function argument in the callee from its definition in the caller.
pub fn assign_def<Ctx: WriteContext>(
    arena: &ValueArena,
    ctx_caller: &impl ReadContext<Func = Ctx::Func>,
    mut ctx_callee: Ctx,
    id: &ast::Id,
    value: Value,
) -> Backtrack<Ctx> {
    // The value must be a function reference
    let ValueKind::Func(id_func) = arena.kind(&value) else {
        return err!(
            id.span.clone(),
            ErrorKind::Assign(AssignErrorKind::DefinitionMismatch {
                value: arena.to_string(&value),
                def: id.node.clone(),
            }),
        );
    };
    // Look the definition up in the caller, bind it in the callee
    let func = unwrap_from_result!(ctx_caller.find_func(id_func), &id_func.span);
    unwrap_from_result!(ctx_callee.add_func(id.clone(), Rc::clone(func)), &id.span);
    ok!(ctx_callee)
}
