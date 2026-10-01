//! Algorithmic-language variable expressions
//!
//! Variables are IL variables, so the conversion is IL's.

use super::ast::*;

// == Conversion to expressions

/// Converts a variable to an expression.
pub fn as_exp(is_dim: bool, var: &Var) -> Exp {
    crate::lang::il::var::as_exp(is_dim, var)
}
