//! Suites of the metacircular driver, one per registration in `suites/metacirc/`
//!
//! `run` and `sim` execute corpus and simulation suites through towers
//! under a `config::Configuration`,
//! comparing with the main driver's expectations;
//! `k_in_st` runs K definitions through spec-k, comparing with `krun`;
//! `metaparse` checks that the meta codecs round-trip.

pub mod config;
pub mod k_in_st;
pub mod metaparse;
pub mod run;
pub mod sim;
