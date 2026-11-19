//! Dimension checks for reads of bound variables
//!
//! A variable bound inside an iterated premise holds one value per iteration.
//! After `-- (if y = x + 1)*` binds `y` at `nat*`,
//! a read of `y` must sit under the same iterations or iterate `y` itself.
//! `check_read` is shared by direct reads in `transform`
//! and by the conditions `partial` generates for bound values in patterns.

use crate::lang::il::ast;

use crate::runtime::dim::Dim;

use super::super::{AlgoError, error};

use super::iteration::ICtx;

/// Requires a read to cover the dimension its variable is bound at.
///
/// A variable bound at `nat` may still be read inside an iteration.
pub fn check_read(iter_ctx: &ICtx, var: &ast::Var, dim_bound: &Dim) -> Result<(), AlgoError> {
    // Lift through enclosing iterations, innermost first, as bindings are
    let mut dim_read = Dim::new(var.typ.clone(), var.iters.clone());
    for iter in iter_ctx.iters() {
        dim_read = dim_read.add_iter(iter);
    }
    if dim_bound.sub(&dim_read) {
        Ok(())
    } else {
        Err(error::binding::bound_dimension_mismatch(&var.id, dim_bound, &dim_read))
    }
}
