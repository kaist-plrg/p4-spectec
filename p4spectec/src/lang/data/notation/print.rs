//! Rendering notation atoms and arguments in reading order
//!
//! Tree and flat visits supply the items; empty keyword atoms are silent.
//! `print_item` separates the remaining items with spaces
//! and delegates argument rendering to the caller.

use std::fmt;

use crate::lang::{
    common::notation::atom::Atom,
    traits::print::{Print, Printer},
};

use super::visit::VisitItem;

// = Traversal items

/// Prints one traversal item with spacing shared by both representations.
pub(super) fn print_item(
    visit_item: VisitItem<'_>,
    printer: &mut Printer<'_>,
    is_first: &mut bool,
    print_arg: &mut impl FnMut(usize, &mut Printer<'_>) -> fmt::Result,
) -> fmt::Result {
    match visit_item {
        VisitItem::Atom(atom) if matches!(&atom.node, Atom::Keyword(keyword) if keyword.is_empty()) => {
            Ok(())
        }
        VisitItem::Atom(atom) => print_sep(printer, is_first).and_then(|()| atom.print(printer)),
        VisitItem::Arg(pos) => print_sep(printer, is_first).and_then(|()| print_arg(pos, printer)),
    }
}

/// Writes a space before every item but the first.
fn print_sep(printer: &mut Printer<'_>, is_first: &mut bool) -> fmt::Result {
    if *is_first {
        *is_first = false;
        Ok(())
    } else {
        printer.write(" ")
    }
}
