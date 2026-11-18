//! Expression and parameter assignment
//!
//! Re-exports the shared assignment and adds parameters,
//! whose patterns live in the parameter, not in a separate argument list.

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::pl::prepared as ast;

use crate::interp::shared::backtrack::{Backtrack, ok};

pub(super) use crate::interp::shared::eval::assign::*;

use crate::interp::pl::context::Context;

// = Parameter assignment

/// Assigns values to parameters pairwise, requiring equal counts.
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
        match &param.node {
            ast::ParamKind::Exp(_, exp) => assign_exp(arena, ctx, exp, *value)?,
            ast::ParamKind::Def(id, ..) => assign_def_arg(ctx, id, *value)?,
        }
    }
    ok!(())
}
