//! Iteration operators
//!
//! `?` marks an optional element, `*` a list;
//! iterations stack outward, so `x*?` is an optional list.

use std::{cmp::Ordering, fmt};

use crate::lang::{
    common::ds::set::IdSet,
    traits::{
        cmp::SyntaxCmp,
        eq::SyntaxEq,
        free::FreeIds,
        print::{Print, Printer},
    },
};

/// An iteration marker.
#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub enum Iter {
    /// `?`
    Opt,
    /// `*`
    List,
}

impl Print for Iter {
    fn print(&self, printer: &mut Printer<'_>) -> fmt::Result {
        printer.write(match self {
            Self::Opt => "?",
            Self::List => "*",
        })
    }
}

impl SyntaxEq for Iter {
    fn syntax_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SyntaxCmp for Iter {
    fn syntax_cmp(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }
}

impl FreeIds for Iter {
    fn free_ids(&self) -> IdSet {
        IdSet::new()
    }
}
