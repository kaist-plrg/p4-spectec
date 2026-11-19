//! Notation atoms and arguments in reading order
//!
//! Tree and flat traversal yield argument positions as `Item<usize>`.
//! Printing replaces each position with a printable argument;
//! `ArgRef` renders a missing argument as `%`.

use std::fmt;

use crate::lang::{
    common::notation::atom::Atom,
    traits::print::{Print, Printer},
};

use super::AtomPhrase;

// = Traversal items

/// An atom or argument encountered in notation order.
#[derive(Clone, Copy, Debug)]
pub(crate) enum Item<'a, T> {
    /// A literal atom.
    Atom(&'a AtomPhrase),
    /// An argument or its zero-based position.
    Arg(T),
}

impl<T> Item<'_, T> {
    /// Identifies empty keyword atoms, which add no output or separator.
    pub(super) fn is_silent(&self) -> bool {
        matches!(self, Self::Atom(atom) if matches!(&atom.node, Atom::Keyword(keyword) if keyword.is_empty()))
    }
}

impl<T: Print> Print for Item<'_, T> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self {
            Self::Atom(atom) => atom.print(printer),
            Self::Arg(arg) => arg.print(printer),
        }
    }
}

// = Optional arguments

/// Borrows a filled argument, or a position left open as `%`.
#[derive(Clone, Copy, Debug)]
pub(crate) struct ArgRef<'a, T> {
    pub(crate) arg: Option<&'a T>,
}

impl<T: Print> Print for ArgRef<'_, T> {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        match self.arg {
            Some(arg) => arg.print(printer),
            None => printer.write("%"),
        }
    }
}
