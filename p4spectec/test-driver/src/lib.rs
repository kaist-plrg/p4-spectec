//! Shared acceptance utilities and suite implementations
//!
//! The main driver selects registered compiler, execution, and simulation suites
//! The metacirc binary runs the SL execution corpus through an AL tower
//! Snapshot normalization is shared by both drivers

use clap::Args;

mod corpus;
mod output;
pub mod snapshot;
pub mod suite;

pub mod elab;

pub mod algo;

pub mod structure;

pub mod prose;

pub mod adoc;

pub mod micro;
pub mod p4parse;
pub mod run;
pub mod sim;

pub mod diagnostic;

pub mod metacirc;

/// An acceptance driver failure.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// A filesystem failure.
    #[error(transparent)]
    Io(#[from] std::io::Error),
    /// An invalid input or mismatched expectation.
    #[error("{0}")]
    Invalid(String),
}
/// The result of an acceptance check.
pub type Result<T> = std::result::Result<T, Error>;

/// Configures interpreter execution consistently for run, sim, and metacirc.
#[derive(Clone, Copy, Args)]
pub struct ExecutionOptions {
    /// Disable deterministic execution checks
    #[arg(long, global = true)]
    no_det: bool,
    /// Enable or disable interpreter caching
    #[arg(long, global = true, default_value_t = true, action = clap::ArgAction::Set)]
    cache_on: bool,
}
