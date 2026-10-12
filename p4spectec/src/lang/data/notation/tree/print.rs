//! Printing tree notation
//!
//! Traversal supplies atoms and argument positions in reading order.
//! Each argument is viewed through `Print`; silent atoms add no separator.

use std::fmt;

use crate::lang::traits::print::{Print, Printer};

use super::super::items::Item;
use super::{Mixfix, Mixop};

// = Notation forms

impl Mixop {
    /// Writes atoms and viewed arguments, separating nonempty items with spaces.
    pub fn print_with<T: Print>(
        &self,
        printer: &mut Printer<'_>,
        mut view_arg: impl FnMut(usize) -> T,
    ) -> fmt::Result {
        let mut is_first = true;
        let mut result = Ok(());
        self.visit(|item| {
            // Ignore silent atoms and stop writing after an output error
            if result.is_err() || item.is_silent() {
                return;
            }
            // Separate only the items that contribute output
            result = if is_first { Ok(()) } else { printer.write(" ") };
            is_first = false;
            result = result.and_then(|()| {
                let item = match item {
                    Item::Atom(atom) => Item::Atom(atom),
                    Item::Arg(pos) => Item::Arg(view_arg(pos)),
                };
                item.print(printer)
            });
        });
        result
    }
}

impl Print for Mixop {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        self.print_with(printer, |_| "%")
    }
}

// = Filled forms

impl<T> Mixfix<T> {
    /// Writes atoms and viewed arguments in notation order.
    pub fn print_with<'a, U: Print>(
        &'a self,
        printer: &mut Printer<'_>,
        mut view_arg: impl FnMut(&'a T) -> U,
    ) -> fmt::Result {
        self.mixop
            .print_with(printer, |pos| view_arg(&self.args[pos]))
    }
}
