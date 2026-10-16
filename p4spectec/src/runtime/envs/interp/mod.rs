//! Environments used by interpreter execution
//!
//! `shared` holds frames, callables, and caches;
//! `al` and `sl` hold each language's prepared definitions.

pub mod al;
pub mod pl;
pub mod shared;
pub mod sl;
