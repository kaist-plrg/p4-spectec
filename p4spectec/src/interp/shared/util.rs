//! Iterated variables through their prepared outer variables
//!
//! `iterate_vars` returns the variables one iteration outward.

use crate::lang::il::prepared as ast;

/// The variables one iteration outward, prepared with their iteration.
pub fn iterate_vars(vars: &[ast::Var]) -> Vec<ast::Var> {
    vars.iter().map(|var| var.outer().clone()).collect()
}
