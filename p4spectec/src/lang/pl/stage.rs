//! Source and prepared stages of PL syntax
//!
//! Identifiers, variables, and mixops use IL's representations.
//! PL additionally selects the expression:
//! a source expression keeps PL syntax and its prose hints,
//! while a prepared expression is IL's, as AL and SL execute it.

use std::fmt;

use crate::lang::il;

pub use il::stage::{Prepared, Source};

/// The expression representation used by PL syntax.
pub trait Stage: il::stage::Stage {
    /// A PL expression with prose hints, or a prepared IL expression.
    type Exp: Clone + fmt::Debug + PartialEq;
}

impl Stage for Source {
    type Exp = super::ast::Exp;
}

impl Stage for Prepared {
    type Exp = il::prepared::Exp;
}
