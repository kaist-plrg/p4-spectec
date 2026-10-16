//! Expression and parameter assignment
//!
//! Re-exports the shared assignment and adds parameters,
//! whose patterns live in the parameter, not in a separate argument list.

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::sl::prepared as ast;

use crate::interp::shared::backtrack::{Backtrack, ok};

pub use crate::interp::shared::eval::assign::*;

use super::super::context::Context;

// = Parameter assignment

/// Assigns values to parameters pairwise, requiring equal counts.
pub(in crate::interp::sl) fn assign_params(
    arena: &mut Arena,
    ctx: &mut Context<'_>,
    params: &[ast::Param],
    values: &[Value],
) -> Backtrack<()> {
    // Argument count must match the parameters
    assert_eq!(params.len(), values.len(), "validated parameter argument arity");
    // Bind pairwise, threading the context
    for (param, value) in params.iter().zip(values) {
        match &param.node {
            ast::ParamKind::Exp(_, exp) => assign_in_place(arena, ctx, exp, *value)?,
            ast::ParamKind::Def(id, ..) => assign_def_in_place(ctx, id, *value),
        }
    }
    ok!(())
}
