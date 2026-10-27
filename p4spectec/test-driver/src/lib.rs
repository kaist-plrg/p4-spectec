//! Test harness and suites shared by the acceptance drivers
//!
//! The main driver, `bin/main.rs`, runs the suites under `suite`;
//! the metacirc driver, `bin/metacirc.rs`, runs those under `metacirc`
//! through towers, reusing the harness, snapshots, and simulation suites.

pub mod harness;
pub mod metacirc;
pub mod suite;
