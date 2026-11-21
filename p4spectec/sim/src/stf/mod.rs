//! STF packet-test language frontend and execution
//!
//! `syntax` turns STF source into statements,
//! `transform` renames what they refer to for the architecture under test,
//! and `run` executes them against one runner,
//! applying control-plane statements through `p4::RuntimeInterface`
//! and matching outputs against `expect` lines.

pub(crate) mod run;
pub(crate) mod syntax;
pub(crate) mod transform;
