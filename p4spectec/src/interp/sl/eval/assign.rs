//! Expression and parameter assignment
//!
//! Re-exports the shared assignment and adds parameters,
//! whose patterns live in the parameter, not in a separate argument list.

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::sl::prepared as ast;

use crate::interp::shared::backtrack::{Backtrack, ok, unwrap};

pub use crate::interp::shared::eval::assign::*;

use super::super::context::Context;

// = Parameter assignment

/// Assigns a value to a parameter: to its pattern, or as a function definition.
fn assign_param<'global>(
    arena: &mut Arena,
    ctx_caller: &Context<'_>,
    ctx: Context<'global>,
    param: &ast::Param,
    value: Value,
) -> Backtrack<Context<'global>> {
    match &param.node {
        ast::ParamKind::Exp(_, exp) => assign_exp(arena, ctx, exp, value),
        ast::ParamKind::Def(id, ..) => assign_def(arena, ctx_caller, ctx, id, value),
    }
}

/// Assigns values to parameters pairwise, requiring equal counts.
pub(in crate::interp::sl) fn assign_params<'global>(
    arena: &mut Arena,
    ctx_caller: &Context<'_>,
    mut ctx: Context<'global>,
    params: &[ast::Param],
    values: &[Value],
) -> Backtrack<Context<'global>> {
    // Argument count must match the parameters
    assert_eq!(params.len(), values.len(), "validated parameter argument arity");
    // Bind pairwise, threading the context
    for (param, value) in params.iter().zip(values) {
        ctx = unwrap!(assign_param(arena, ctx_caller, ctx, param, *value));
    }
    ok!(ctx)
}
