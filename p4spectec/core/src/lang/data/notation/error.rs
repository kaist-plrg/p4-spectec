//! Errors of notation operations
//!
//! Arity mismatches identify invalid mixfix construction.
//! Interning reports when a mixop handle no longer fits the arena index.

use std::num::TryFromIntError;

use thiserror::Error;

/// A failure interning a mixop.
#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum MixopError {
    /// The arena ran out of 32-bit handles.
    #[error("mixop arena index overflow")]
    IndexOverflow,
}

/// An error caused by a mismatch between mixop arity and supplied arguments.
#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum ArityMismatch {
    /// Fewer arguments were supplied than the mixop requires.
    #[error("too few arguments for mixop")]
    ArgumentCountTooFew,
    /// More arguments were supplied than the mixop requires.
    #[error("too many arguments for mixop")]
    ArgumentCountTooMany,
}

// - Index overflow

impl From<TryFromIntError> for MixopError {
    fn from(_: TryFromIntError) -> Self {
        Self::IndexOverflow
    }
}
