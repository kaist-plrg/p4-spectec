//! Expression and parameter assignment
//!
//! Re-exports the shared assignment and adds parameters,
//! whose patterns live in the parameter, not in a separate argument list.

use super::super::context::Context;
use crate::interp::shared::error;
use crate::runtime::envs::interp::sl::ast_prepared as ast;
use crate::{
    interp::shared::backtrack::{Backtrack, ok, unwrap},
    lang::data::value::{Value, ValueArena},
};

pub use crate::interp::shared::eval::assign::*;

// = Parameter assignment

/// Assigns a value to a parameter: to its pattern, or as a function definition.
fn assign_param<'global>(
    arena: &mut ValueArena,
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
    arena: &mut ValueArena,
    ctx_caller: &Context<'_>,
    mut ctx: Context<'global>,
    params: &[ast::Param],
    values: &[Value],
) -> Backtrack<Context<'global>> {
    // Argument count must match the parameters
    unwrap!(crate::interp::shared::backtrack::check(
        params.len() == values.len(),
        crate::lang::common::source::Span::default(),
        || error::assign::assignment_argument_arity_mismatch(params.len(), values.len())
    ));
    // Bind pairwise, threading the context
    for (param, value) in params.iter().zip(values) {
        ctx = unwrap!(assign_param(arena, ctx_caller, ctx, param, *value));
    }
    ok!(ctx)
}
