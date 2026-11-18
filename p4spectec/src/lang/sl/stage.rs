//! Source and prepared stages of SL syntax
//!
//! Expressions use IL's identifier, variable, and mixop representations.
//! SL additionally selects the rule-call, result, and return payloads:
//! a source call retains its notation and continuation,
//! while a prepared one holds its inputs and outputs apart;
//! a source result or return holds its expressions,
//! while a prepared one concludes with their values or with a tail call.
//! Prepared instructions carry an `InstrId` numbered in pre-order.

use std::fmt;

use crate::lang::il;

pub use il::stage::{Prepared, Source};

/// The expression representation, rule-call, result, and return forms used by SL syntax.
pub trait Stage: il::stage::Stage {
    /// A source rule call or a prepared one split by its input hint.
    type RuleInstr: Clone + fmt::Debug + PartialEq;
    /// A source result or a prepared values/tail call.
    type ResultInstr: Clone + fmt::Debug + PartialEq;
    /// A source return or a prepared value/tail call.
    type ReturnInstr: Clone + fmt::Debug + PartialEq;
    /// The note attached to each instruction.
    type InstrNote: Clone + fmt::Debug + PartialEq;
}

impl Stage for Source {
    type RuleInstr = super::ast::RuleInstr<Self>;
    type ResultInstr = super::ast::ResultInstr<Self>;
    type ReturnInstr = super::ast::ReturnInstr<Self>;
    type InstrNote = ();
}

impl Stage for Prepared {
    type RuleInstr = super::prepared::RuleInstr;
    type ResultInstr = super::prepared::ResultInstr;
    type ReturnInstr = super::prepared::ReturnInstr;
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
