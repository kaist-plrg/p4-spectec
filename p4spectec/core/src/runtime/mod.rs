//! Environments and operations shared by language passes and interpreters
//!
//! `dim` and `typdef` are the static shapes passes track;
//! `envs` groups environments by consumer;
//! `ops` implements type and value operations over them.

pub mod dim;
pub mod envs;
pub mod ops;
pub mod typdef;
