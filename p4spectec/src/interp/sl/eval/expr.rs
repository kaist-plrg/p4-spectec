//! Shared expr operations
//!
//! Re-exports the shared expression and argument evaluation; SL adds nothing.

pub(crate) use crate::interp::shared::eval::{arg::eval_args, expr::*};
