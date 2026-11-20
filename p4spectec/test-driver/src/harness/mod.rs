//! Test infrastructure shared by every suite
//!
//! `corpus` discovers and runs corpora,
//! `interpreter` builds runners,
//! `output` silences interpreter stdout,
//! and `worker` runs tests in a process killed after a timeout.
//! Nothing here depends on a particular suite.

pub mod corpus;
pub mod interpreter;
pub mod output;
pub mod worker;

pub type Error = Box<dyn std::error::Error>;
pub type Result<T> = std::result::Result<T, Error>;
