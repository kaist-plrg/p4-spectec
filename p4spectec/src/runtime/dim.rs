//! Iteration dimensions used by static language passes
//!
//! A `Dim` is a base type under a stack of iterations,
//! the shape a binding analysis assigns to a variable:
//! `nat*?` is `nat` under `*` and then `?`.

use std::fmt;

use crate::lang::traits::{
    eq::SyntaxEq,
    print::{Print, Printer},
};

use crate::lang::il::ast::{self, Iter};

/// A base type paired with the iteration dimensions around it.
#[derive(Clone, Debug, PartialEq)]
pub struct Dim {
    /// Base type below the iteration dimensions.
    pub(crate) typ: ast::Typ,
    /// Iteration dimensions from innermost to outermost.
    pub(crate) iters: Vec<Iter>,
}

impl Dim {
    pub fn new(typ: ast::Typ, iters: Vec<Iter>) -> Self {
        Self { typ, iters }
    }

    /// Tests whether this value's dimensions are a prefix of `other`.
    pub fn sub(&self, other: &Self) -> bool {
        self.typ.syntax_eq(&other.typ)
            && self.iters.len() <= other.iters.len()
            && self.iters == other.iters[..self.iters.len()]
    }

    /// Appends one outer iteration dimension.
    pub fn add_iter(mut self, iter: Iter) -> Self {
        self.iters.push(iter);
        self
    }
}

impl Print for Dim {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.typ.print(printer)?;
        for iter in &self.iters {
            iter.print(printer)?;
        }
        Ok(())
    }
}
