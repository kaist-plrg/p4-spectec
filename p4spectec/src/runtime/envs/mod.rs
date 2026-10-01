//! Environments grouped by their runtime consumer
//!
//! `elab` and `algo` map names to static shapes;
//! `interp` maps them to prepared callables and frames;
//! `prosify` maps definitions to their prose hints.

pub mod algo;
pub mod elab;
pub mod interp;
pub mod prosify;
