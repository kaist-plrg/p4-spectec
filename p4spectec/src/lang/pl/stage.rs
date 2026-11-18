//! Source and prepared stages of PL syntax
//!
//! Identifiers, variables, and mixops use IL's representations.
//! PL additionally selects the expression and the relation call:
//! a source expression keeps PL syntax and its prose hints,
//! while a prepared expression is IL's, as AL and SL execute it;
//! a source relation call keeps the notation and input hint,
//! while a prepared one holds its input and output expressions apart.

use std::fmt;

use crate::lang::il;

pub use il::stage::{Prepared, Source};

/// The expression representation and relation-call form used by PL syntax.
pub trait Stage: il::stage::Stage {
    /// A PL expression with prose hints, or a prepared IL expression.
    type Exp: Clone + fmt::Debug + PartialEq;
    /// A source relation call or a prepared one split by its input hint.
    type RuleInstr: Clone + fmt::Debug + PartialEq;
}

impl Stage for Source {
    type Exp = super::ast::Exp;
    type RuleInstr = super::ast::RuleInstr<Self>;
}

impl Stage for Prepared {
    type Exp = il::prepared::Exp;
    type RuleInstr = super::prepared::RuleInstr;
}
