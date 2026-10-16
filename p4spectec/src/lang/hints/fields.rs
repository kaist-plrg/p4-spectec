//! Field hints
//!
//! `hint(prose_fields "a" "b")` names the fields a destructuring step binds.

use thiserror::Error;

use crate::lang::common::source::Phrase;

use crate::lang::el::ast::{Exp, ExpKind, Text};

// == Field hints

/// Field labels with the expression and individual name locations.
pub type FieldHint = Phrase<Vec<Phrase<Text>>>;

#[derive(Clone, Debug, Error, PartialEq, Eq)]
/// A failure validating a field hint.
pub enum FieldError {
    /// The hint names a different number of fields than the notation has.
    #[error("field hint expects {expected} strings, but got {actual}")]
    FieldCountMismatch { expected: usize, actual: usize },
}

// == Initialization

/// Initializes located field names, returning the first non-text expression.
pub fn init(exp: &Exp) -> Result<FieldHint, &Exp> {
    // Preserve the element that violates the text-only contract
    let field = |exp: &Exp| match &exp.node {
        ExpKind::Text(text) => Some(crate::phrase! {
            node: text.clone(), span: exp.span,
        }),
        _ => None,
    };
    let fields = match &exp.node {
        ExpKind::Seq(exps) => exps
            .iter()
            .map(|exp| field(exp).ok_or(exp))
            .collect::<Result<_, _>>()?,
        _ => vec![field(exp).ok_or(exp)?],
    };
    Ok(crate::phrase! { node: fields, span: exp.span })
}

// == Validation

/// Validates that the field count matches `arity`.
pub fn validate(hint: &FieldHint, arity: usize) -> Result<(), FieldError> {
    if hint.node.len() == arity {
        Ok(())
    } else {
        Err(FieldError::FieldCountMismatch { expected: arity, actual: hint.node.len() })
    }
}
