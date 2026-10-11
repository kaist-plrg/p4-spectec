//! Printing flat notation
//!
//! Flat traversal resolves handles and supplies atoms and argument positions;
//! shared helpers apply spacing and silent-atom rules.

use std::fmt;

use crate::lang::traits::print::Printer;

use super::super::print::print_item;
use super::{Mixfix, Mixop, MixopArena};

// = Notation forms

impl Mixop {
    /// Writes atoms and arguments, separating non-empty pieces with spaces.
    ///
    /// Empty keyword atoms print nothing, not even a space;
    /// `print_arg` writes the argument at a position.
    pub fn print_with(
        self,
        arena_mixop: &MixopArena,
        printer: &mut Printer<'_>,
        mut print_arg: impl FnMut(usize, &mut Printer<'_>) -> fmt::Result,
    ) -> fmt::Result {
        let mut is_first = true;
        let mut result = Ok(());
        self.visit(arena_mixop, |visit_item| {
            if result.is_ok() {
                result = print_item(visit_item, printer, &mut is_first, &mut print_arg);
            }
        });
        result
    }
}

// = Filled forms

impl<T> Mixfix<T> {
    /// Writes atoms and arguments as the expanded tree would print.
    pub fn print_with(
        &self,
        arena_mixop: &MixopArena,
        printer: &mut Printer<'_>,
        mut print_arg: impl FnMut(&T, &mut Printer<'_>) -> fmt::Result,
    ) -> fmt::Result {
        self.mixop
            .print_with(arena_mixop, printer, |pos, printer| print_arg(&self.args[pos], printer))
    }
}
