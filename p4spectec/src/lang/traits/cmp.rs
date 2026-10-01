//! Syntax comparison shared across language stages
//!
//! `SyntaxCmp` refines `SyntaxEq` to a total order,
//! so syntax can key ordered maps and sets.
//! Blanket impls cover slices, strings, spanned nodes, and `Rc`.

use std::{cmp::Ordering, rc::Rc};

use crate::lang::common::source::NotePhrase;

use super::eq::SyntaxEq;

/// Compares syntax lexicographically, ignoring source and analysis metadata.
pub trait SyntaxCmp<Rhs: ?Sized = Self>: SyntaxEq<Rhs> {
    /// Compares two nodes by their syntax.
    fn syntax_cmp(&self, other: &Rhs) -> Ordering;

    /// Compares slices of nodes lexicographically.
    fn slice_syntax_cmp(items_l: &[Self], items_r: &[Rhs]) -> Ordering
    where
        Self: Sized,
        Rhs: Sized,
    {
        for (item_l, item_r) in items_l.iter().zip(items_r) {
            let ordering = item_l.syntax_cmp(item_r);
            if ordering != Ordering::Equal {
                return ordering;
            }
        }
        items_l.len().cmp(&items_r.len())
    }
}

impl<T, Rhs> SyntaxCmp<[Rhs]> for [T]
where
    T: SyntaxCmp<Rhs>,
{
    fn syntax_cmp(&self, other: &[Rhs]) -> Ordering {
        T::slice_syntax_cmp(self, other)
    }
}

impl SyntaxCmp for String {
    fn syntax_cmp(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }
}

impl<T: SyntaxCmp, N, S> SyntaxCmp for NotePhrase<T, N, S> {
    fn syntax_cmp(&self, other: &Self) -> Ordering {
        self.node.syntax_cmp(&other.node)
    }
}

impl<T: SyntaxCmp + ?Sized> SyntaxCmp for Rc<T> {
    fn syntax_cmp(&self, other: &Self) -> Ordering {
        self.as_ref().syntax_cmp(other.as_ref())
    }
}
