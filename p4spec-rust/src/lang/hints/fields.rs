//! Field hints
//!
//! `hint(prose_fields "a" "b")` names the fields a destructuring step binds.

use crate::lang::{
    common::source::Phrase,
    el::ast::{Exp, ExpKind, Text},
};
use thiserror::Error;

// == Field hints

/// Field labels for prose rendering.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldHint {
    /// Field labels and their source locations in order.
    fields: Vec<Phrase<Text>>,
}

#[derive(Clone, Debug, Error, PartialEq, Eq)]
/// A failure validating a field hint.
pub enum FieldError {
    /// The hint names a different number of fields than the notation has.
    #[error("field hint expects {expected} strings, but got {actual}")]
    ArityMismatch { expected: usize, actual: usize },
}

impl FieldHint {
    /// Preserves fields without validation.
    pub fn new(fields: Vec<Phrase<Text>>) -> Self {
        Self { fields }
    }

    /// Borrows the labels.
    pub fn fields(&self) -> &[Phrase<Text>] {
        &self.fields
    }

    /// Consumes the value into fields.
    pub fn into_fields(self) -> Vec<Phrase<Text>> {
        self.fields
    }
}

// == Initialization

/// Initializes located field names, returning the first non-text expression.
pub fn init(exp: &Exp) -> Result<FieldHint, &Exp> {
    // Preserve the element that violates the text-only contract
    let field = |exp: &Exp| match &exp.node {
        ExpKind::Text(text) => Some(crate::phrase! {
            node: text.clone(), span: exp.span.clone(),
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
    Ok(FieldHint::new(fields))
}

// == Validation

/// Validates that the field count matches `arity`.
pub fn validate(hint: &FieldHint, arity: usize) -> Result<(), FieldError> {
    if hint.fields.len() == arity {
        Ok(())
    } else {
        Err(FieldError::ArityMismatch { expected: arity, actual: hint.fields.len() })
    }
}
