//! Calls to the specification's relations and functions by name
//!
//! `rel` and `func` call specification definitions by name with arena values
//! and unpack their outputs;
//! `args` pairs extern arguments with their names.

pub(crate) mod args;
pub(crate) mod func;
pub(crate) mod rel;
