//! Source and prepared stages of AL syntax
//!
//! Expressions use IL's identifiers, variables, and mixop representations.
//! AL additionally selects the rule-premise payload and what concludes a callable:
//! a source premise keeps the relation's notation and input hint,
//! while a prepared premise holds its input and output expressions apart;
//! a source rule path or clause concludes with expressions,
//! while a prepared one concludes with their values or with a tail call.

use std::fmt;

use crate::lang::il;

pub use il::stage::{Prepared, Source};

/// The expression representation, rule-premise, and conclusion forms used by AL syntax.
pub trait Stage: il::stage::Stage {
    /// A source rule premise or a prepared one split by its input hint.
    type RulePrem: Clone + fmt::Debug + PartialEq;
    /// A source rule path's outputs, or a prepared values/tail call.
    type RelResult: Clone + fmt::Debug + PartialEq;
    /// A source clause or row body, or a prepared value/tail call.
    type FuncReturn: Clone + fmt::Debug + PartialEq;
}

impl Stage for Source {
    type RulePrem = super::ast::RulePrem<Self>;
    type RelResult = Vec<super::ast::Exp<Self>>;
    type FuncReturn = super::ast::Exp<Self>;
}

impl Stage for Prepared {
    type RulePrem = super::prepared::RulePrem;
    type RelResult = il::prepared::RelResult;
    type FuncReturn = il::prepared::FuncReturn;
}
