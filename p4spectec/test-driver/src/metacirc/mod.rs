//! Suites of the metacircular driver, one per registration in `suites/metacirc/`
//!
//! `run` and `sim` execute corpus and simulation suites through towers
//! under a `config::Configuration`,
//! comparing with the main driver's expectations;
//! `metaparse` checks that the meta codecs round-trip.

pub mod config;
pub mod metaparse;
pub mod run;
pub mod sim;
