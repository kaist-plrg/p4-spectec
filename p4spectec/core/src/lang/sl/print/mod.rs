//! Text rendering for SL syntax
//!
//! `source` implements source rendering.
//! Prepared expressions reuse IL rendering.

mod source;

pub(crate) use source::{write_instr_heading, write_relinput};
