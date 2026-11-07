//! Source and prepared stages of SL syntax
//!
//! Expressions use IL's identifier, variable, and mixop representations.
//! SL additionally selects the rule-call payload:
//! source calls retain their continuation,
//! while prepared calls distinguish ordinary calls from tail calls.
//! Prepared instructions carry an `InstrId` numbered in pre-order.

use std::fmt;

use crate::lang::il;

pub use il::stage::{Prepared, Source};

/// The expression representation and rule-call form used by SL syntax.
pub trait Stage: il::stage::Stage {
    /// A source rule call or a prepared ordinary/tail call.
    type RuleInstr: Clone + fmt::Debug + PartialEq;
    /// The note attached to each instruction.
    type InstrNote: Clone + fmt::Debug + PartialEq;
}

impl Stage for Source {
    type RuleInstr = super::ast::RuleInstr<Self>;
    type InstrNote = ();
}

impl Stage for Prepared {
    type RuleInstr = super::prepared::RuleInstr;
    type InstrNote = InstrId;
}

/// A prepared instruction's index in the specification-wide pre-order.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct InstrId(pub u32);

impl InstrId {
    /// The index as a `usize`.
    pub fn index(self) -> usize {
        self.0 as usize
    }
}
