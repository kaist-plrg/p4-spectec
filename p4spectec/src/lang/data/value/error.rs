//! Errors of building and projecting values
//!
//! Kind and count mismatches identify invalid projections.
//! Interning failures distinguish value handles from case mixop handles.
//! Extern type definition mismatches name both definitions.

use std::num::TryFromIntError;

use thiserror::Error;

use crate::lang::data::notation::MixopError;

use super::ValueTag;

/// A failure building or projecting a value.
#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum ValueError {
    /// The arena ran out of 32-bit handles.
    #[error("value arena index overflow")]
    IndexOverflow,
    /// Interning the case notation failed.
    #[error(transparent)]
    Mixop(#[from] MixopError),
    /// A projection met a value of another kind.
    #[error("expected {expected:?} value, got {actual:?}")]
    KindMismatch { expected: ValueTag, actual: ValueTag },
    /// An element index past the end.
    #[error("value index {index} is out of bounds for length {len}")]
    IndexOutOfBounds { index: usize, len: usize },
    /// A fixed-arity projection met another count.
    #[error("expected exactly {expected} values, got {actual}")]
    CountMismatch { expected: usize, actual: usize },
    /// An extern value read through a definition that did not make it.
    #[error("expected an extern value of {expected}, got one of {actual}")]
    TypDefMismatch { expected: String, actual: String },
    /// Two definitions registered under one extern type name.
    #[error("extern type {name} has more than one definition")]
    TypDefRepeated { name: String },
    /// An extern type definition used with an arena that did not issue it.
    #[error("extern type definition belongs to another arena")]
    TypDefForeign,
    /// An extern type name the receiving runner has no definition for.
    #[error("extern type {name} has no registered definition")]
    TypDefUnregistered { name: String },
    /// An extern payload copied out of its arena without a codec.
    #[error("extern type {name} has no codec")]
    CodecMissing { name: String },
    /// An extern payload codec failed.
    #[error("extern type {name} payload: {message}")]
    CodecFailed { name: String, message: String },
}

// - Index overflow

impl From<TryFromIntError> for ValueError {
    fn from(_: TryFromIntError) -> Self {
        Self::IndexOverflow
    }
}
