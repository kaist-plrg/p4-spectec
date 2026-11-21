//! STF packet-test language frontend and execution
//!
//! `syntax` turns STF source into statements,
//! and `run` executes them against one runner,
//! adding table entries through `table`
//! and matching outputs against `expect` lines.

pub(crate) mod run;
pub(crate) mod syntax;
pub(crate) mod table;
pub(crate) mod transform;
