//! Errors produced by runtime type operations

use std::fmt;

use thiserror::Error;

use crate::lang::common::{ds::map::ArityMismatch, source::Span};

/// A failure in a runtime type operation.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
#[error("{kind} at {span}")]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

/// Category of a runtime type failure.
#[derive(Clone, Debug, Error, PartialEq, Eq)]
pub enum TypeErrorKind {
    /// A type variable with arguments was substituted.
    #[error("higher-order substitution is disallowed")]
    HigherOrderSubstitutionUnsupported,

    /// Two type-level lists differ in length.
    #[error("{0}")]
    ArityMismatch(TypeArityMismatch),

    /// A type name has no definition.
    #[error("type variable {0} is not defined")]
    TypeUndefined(String),
}

/// Context of an arity mismatch in a runtime type operation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeArityMismatch {
    /// Type arguments against type parameters.
    TypeArgument(ArityMismatch),
    /// Type parameters of two function types.
    TypeParameter(ArityMismatch),
    /// Parameters of two function types.
    Parameter(ArityMismatch),
}

impl fmt::Display for TypeArityMismatch {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (description, mismatch) = match self {
            Self::TypeArgument(mismatch) => ("type argument count differs", mismatch),
            Self::TypeParameter(mismatch) => ("type parameter counts differ", mismatch),
            Self::Parameter(mismatch) => ("parameter counts differ", mismatch),
        };
        write!(formatter, "{description}: expected {}, got {}", mismatch.expected, mismatch.actual)
    }
}

impl std::error::Error for TypeArityMismatch {}

impl TypeError {
    pub(crate) fn new(kind: TypeErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}
