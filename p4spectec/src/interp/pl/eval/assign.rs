//! Expression and parameter assignment
//!
//! Expression patterns retain PL syntax until `assign_exp` removes hints.
//! Shared assignment then binds their prepared slots.
//! `assign_params` binds function arguments to their parameters' slots.

use std::borrow::Borrow;

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::pl::prepared as ast;

use crate::interp::shared::{
    backtrack::{Backtrack, ok, unwrap},
    eval::assign as shared,
};

use crate::interp::pl::context::Context;

use super::strip::strip_exp;

use shared::assign_def_arg;

// = Expression assignment

/// Assigns a value to a PL pattern after removing its prose hints.
pub(super) fn assign_exp(
    arena: &mut Arena,
    ctx: &mut Context<'_>,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<()> {
    let exp_shared = strip_exp(exp);
    shared::assign_exp(arena, ctx, &exp_shared, value)
}

/// Assigns values pairwise to PL patterns through shared assignment.
pub(super) fn assign_exps<T: Borrow<ast::Exp>>(
    arena: &mut Arena,
    ctx: &mut Context<'_>,
    exps: &[T],
    values: &[Value],
) -> Backtrack<()> {
    let exps_shared = exps
        .iter()
        .map(|exp| strip_exp(exp.borrow()))
        .collect::<Vec<_>>();
    shared::assign_exps(arena, ctx, &exps_shared, values)
}

// = Parameter assignment

/// Binds prepared parameter patterns and function arguments.
pub(super) fn assign_params(
    arena: &mut Arena,
    ctx: &mut Context<'_>,
    params: &[ast::Param],
    values: &[Value],
) -> Backtrack<()> {
    // Argument count must match the parameters
    assert_eq!(params.len(), values.len(), "validated parameter argument arity");
    // Bind pairwise, threading the callee context
    for (param, value) in params.iter().zip(values) {
        unwrap!(match &param.node {
            ast::ParamKind::Exp(_, exp) => assign_exp(arena, ctx, exp, *value),
            ast::ParamKind::Def(id, ..) => assign_def_arg(ctx, id, *value),
        });
    }
    ok!(())
}
