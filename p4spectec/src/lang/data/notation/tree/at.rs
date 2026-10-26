//! Source locations of tree notation
//!
//! Covers the atoms and arguments of a form in notation order.

use crate::lang::{common::source::Span, traits::at::At};

use super::super::items::Item;
use super::{Mixfix, Mixop};

impl At for Mixop {
    fn at(&self) -> Span {
        // Cover atoms only; a bare mixop has no arguments to locate
        let mut spans = Vec::new();
        self.visit(|visit_item| {
            if let Item::Atom(atom) = visit_item {
                spans.push(atom.at());
            }
        });
        spans.at()
    }
}

impl<T: At> At for Mixfix<T> {
    fn at(&self) -> Span {
        // Cover atoms and arguments, so empty sequences add no default span
        let mut spans = Vec::new();
        self.mixop.visit(|visit_item| match visit_item {
            Item::Atom(atom) => spans.push(atom.at()),
            Item::Arg(pos) => spans.push(self.args[pos].at()),
        });
        spans.at()
    }
}
