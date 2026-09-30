//! Typed wrappers around the specification's relations and functions
//!
//! `rel` and `func` call specification definitions by name with arena values;
//! `pgm` runs a program's initialization relation;
//! `args` pairs extern arguments with their names;
//! `pack` and `unpack` convert P4 values
//! between Rust and the specification's cases.
//! Calls finalize exhausted interpreter searches as fatal extern diagnostics.

// == Calls

pub mod args;
pub mod func;
pub mod pgm;
pub mod rel;

// == Values

pub mod pack;
pub mod unpack;
