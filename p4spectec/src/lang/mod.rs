//! Language representations and shared utilities
//!
//! One module per stage (`el`, `il`, `al`, `sl`, `pl`),
//! each with its model and the `eq`, `free`, and `print` traversals;
//! `common`, `data`, `hints`, and `traits` hold what the stages share.

pub mod al;
pub mod common;
pub mod data;
pub mod el;
pub mod hints;
pub mod il;
pub mod pl;
pub mod sl;
pub mod traits;
