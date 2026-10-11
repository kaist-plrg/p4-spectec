//! Borrowed views of flat values
//!
//! `ValueRef` pairs a value with the arena used to compare and print its contents.

use crate::lang::data::arena::Arena;

use super::Value;

// = Borrowed views

/// A value together with its arena, for comparison and printing.
///
/// Comparisons require both views to refer to the same arena.
#[derive(Clone, Copy, Debug)]
pub struct ValueRef<'a> {
    pub(super) arena: &'a Arena,
    pub(super) value: Value,
}

impl Value {
    /// Pairs this value with its arena for comparison and printing.
    pub fn view(self, arena: &Arena) -> ValueRef<'_> {
        ValueRef { arena, value: self }
    }
}
