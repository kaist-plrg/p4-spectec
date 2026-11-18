//! Source and prepared stages of AL syntax
//!
//! Expressions use IL's identifiers, variables, and mixop representations.
//! AL additionally selects the rule-premise payload:
//! a source premise keeps the relation's notation and input hint,
//! while a prepared premise holds its input and output expressions apart.

use std::fmt;

use crate::lang::il;

pub use il::stage::{Prepared, Source};

/// The expression representation and rule-premise form used by AL syntax.
pub trait Stage: il::stage::Stage {
    /// A source rule premise or a prepared one split by its input hint.
    type RulePrem: Clone + fmt::Debug + PartialEq;
}

impl Stage for Source {
    type RulePrem = super::ast::RulePrem<Self>;
}

impl Stage for Prepared {
    type RulePrem = super::prepared::RulePrem;
}
