//! Language hints and their utilities
//!
//! A `hint(name exp)` on a definition is read by the pass that owns the name:
//! `input` marks relation inputs, `alter` and `fields` shape prose,
//! `flag` is a bare switch, `hint` the raw expression.

pub mod alter;
pub mod fields;
pub mod flag;
pub mod hint;
pub mod input;
