//! The metacircular driver's registrations and runners
//!
//! `suite` loads the registrations of `suites/metacirc/`;
//! `run` and `sim` execute corpus and simulation suites through towers,
//! comparing with the main driver's expectations;
//! `metaparse` checks that the meta codecs round-trip.

pub mod metaparse;
pub mod run;
pub mod sim;
pub mod suite;
