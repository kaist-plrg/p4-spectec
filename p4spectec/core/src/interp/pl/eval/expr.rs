//! Shared expr operations
//!
//! Re-exports the shared expression evaluation; PL adds nothing,
//! since preparation already removed the prose hints.

pub(super) use crate::interp::shared::eval::expr::{eval_exp, eval_exps};
