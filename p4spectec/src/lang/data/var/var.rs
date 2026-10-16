//! Variables as an identifier under an iteration path, with its type
//!
//! `x*?` is the variable `x` under iterations `[*, ?]`;
//! its type is the element type, not the iterated one.
//! Variable lists compare as sets, since binders are unordered.

use std::fmt;

use crate::lang::{
    common::{Id, Iter, ds::set::IdSet},
    traits::{
        eq::SyntaxEq,
        free::FreeIds,
        print::{Print, Printer},
    },
};

use super::super::typ::Typ;

/// A variable reference.
#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    /// The bound name.
    pub id: Id,
    /// The element type, below the iterations.
    pub typ: Typ,
    /// Iterations from innermost to outermost.
    pub iters: Vec<Iter>,
}

impl Print for Var {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.id.print(printer)?;
        for iter in &self.iters {
            iter.print(printer)?;
        }
        Ok(())
    }
}

impl SyntaxEq for Var {
    fn syntax_eq(&self, other: &Self) -> bool {
        self.id.syntax_eq(&other.id) && self.iters == other.iters
    }

    fn slice_syntax_eq(vars_l: &[Self], vars_r: &[Self]) -> bool {
        // Sort both by name and iterations, then compare pairwise
        let mut vars_l = vars_l.iter().collect::<Vec<_>>();
        let mut vars_r = vars_r.iter().collect::<Vec<_>>();
        let cmp_var = |var_l: &&Self, var_r: &&Self| {
            var_l
                .id
                .node
                .cmp(&var_r.id.node)
                .then_with(|| var_l.iters.cmp(&var_r.iters))
        };
        vars_l.sort_by(cmp_var);
        vars_r.sort_by(cmp_var);
        vars_l.len() == vars_r.len()
            && vars_l
                .into_iter()
                .zip(vars_r)
                .all(|(var_l, var_r)| var_l.syntax_eq(var_r))
    }
}

impl FreeIds for Var {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}
