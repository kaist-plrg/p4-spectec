//! Expression and parameter assignment
//!
//! Expression patterns retain PL syntax until `assign_exp` removes hints.
//! Shared assignment then binds their prepared slots.
//! `assign_params` resolves function arguments in the caller's context.

use std::borrow::Borrow;

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::pl::prepared as ast;

use crate::interp::shared::{
    backtrack::{Backtrack, ok, unwrap},
    eval::assign as shared,
};

use crate::interp::pl::context::Context;

use super::strip::strip_exp;

use shared::assign_def;

// = Expression assignment

/// Assigns a value to a PL pattern after removing its prose hints.
pub(super) fn assign_exp<'g>(
    arena: &mut Arena,
    ctx: Context<'g>,
    exp: &ast::Exp,
    value: Value,
) -> Backtrack<Context<'g>> {
    let exp_shared = strip_exp(exp);
    shared::assign_exp(arena, ctx, &exp_shared, value)
}

/// Assigns values pairwise to PL patterns through shared assignment.
pub(super) fn assign_exps<'g, T: Borrow<ast::Exp>>(
    arena: &mut Arena,
    ctx: Context<'g>,
    exps: &[T],
    values: &[Value],
) -> Backtrack<Context<'g>> {
    let exps_shared = exps
        .iter()
        .map(|exp| strip_exp(exp.borrow()))
        .collect::<Vec<_>>();
    shared::assign_exps(arena, ctx, &exps_shared, values)
}

// = Parameter assignment

/// Binds prepared parameter patterns and resolves caller function aliases.
pub(super) fn assign_params<'g>(
    arena: &mut Arena,
    ctx_caller: &Context<'_>,
    mut ctx: Context<'g>,
    params: &[ast::Param],
    values: &[Value],
) -> Backtrack<Context<'g>> {
    // Argument count must match the parameters
    assert_eq!(params.len(), values.len(), "validated parameter argument arity");
    // Bind pairwise, threading the callee context
    for (param, value) in params.iter().zip(values) {
        let result = match &param.node {
            ast::ParamKind::Exp(_, exp) => assign_exp(arena, ctx, exp, *value),
            ast::ParamKind::Def(id, ..) => assign_def(arena, ctx_caller, ctx, id, *value),
        };
        ctx = unwrap!(result);
    }
    ok!(ctx)
}
