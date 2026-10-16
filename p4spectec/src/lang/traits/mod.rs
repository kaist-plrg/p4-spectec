//! Traits shared across language representations
//!
//! `SyntaxEq` and `SyntaxCmp` compare nodes ignoring spans and notes,
//! `FreeIds` and `FreeVars` collect free names, `HasCall` finds calls,
//! [`at::At`] retrieves source spans, while `Print` renders text;
//! each stage implements them for its model.

pub mod at;
pub mod cmp;
pub mod eq;
pub mod free;
pub mod has_call;
pub mod print;
