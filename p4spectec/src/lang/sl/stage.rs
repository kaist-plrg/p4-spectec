//! Source and prepared stages of SL syntax
//!
//! Expressions use IL's identifier, variable, and mixop representations.
//! SL additionally selects the rule-call payload:
//! source calls retain their continuation,
//! while prepared calls distinguish ordinary calls from tail calls.

use std::fmt;

use crate::lang::il;

pub use il::stage::{Prepared, Source};

/// The expression representation and rule-call form used by SL syntax.
pub trait Stage: il::stage::Stage {
    /// A source rule call or a prepared ordinary/tail call.
    type RuleInstr: Clone + fmt::Debug + PartialEq;
}

impl Stage for Source {
    type RuleInstr = super::ast::RuleInstr<Self>;
}

impl Stage for Prepared {
    type RuleInstr = super::prepared::RuleInstr;
}
