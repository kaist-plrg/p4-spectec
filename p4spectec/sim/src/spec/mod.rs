//! Typed wrappers around the specification's relations and functions
//!
//! `rel` and `func` call specification definitions by name with arena values;
//! `pgm` runs a program's initialization relation;
//! `args` pairs extern arguments with their names;
//! `pack` and `unpack` convert P4 values
//! between Rust and the specification's cases.
//! Call wrappers preserve interpreter failures for the caller to handle.

// == Calls

pub(crate) mod args;
pub(crate) mod func;
pub(crate) mod pgm;
pub(crate) mod rel;

// == Values

pub(crate) mod pack;
pub(crate) mod unpack;
