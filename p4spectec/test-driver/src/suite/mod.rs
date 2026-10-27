//! Concrete test suites, one per registration in `suites/`
//!
//! Each module reads its `suites/<name>.toml` through `harness::load`
//! and checks its sources with the shared harness.

pub mod diagnostic;
pub mod micro;
pub mod p4parse;
pub mod run;
pub mod sim;
pub mod snapshot;
