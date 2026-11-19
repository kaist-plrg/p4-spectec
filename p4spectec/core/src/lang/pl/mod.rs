//! Prose language
//!
//! PL is SL prepared for prose rendering:
//! instructions are split into a dispatch tier (which rule group applies)
//! and a group-body tier (what the group does),
//! shorthand instructions fold common patterns into one prose step,
//! and nodes carry prose hints and fall-through markers.
//! `pass::prosify` produces PL from SL.

pub mod annot;
pub mod ast;
pub mod eq;
pub mod free;
pub mod has_call;
pub mod prepared;
pub mod print;
pub mod rule_group;
pub mod stage;
