//! Stateful PSA extern objects, from counters to checksums
//!
//! Each object is decoded from the specification's object state,
//! updated by its method, and encoded back by the pipeline.
//! Doc comments summarize the object's description in `psa.p4`.

pub mod counter;
pub mod hash;
pub mod internet_checksum;
pub mod meter;
pub mod register;

pub use counter::Counter;
pub use hash::HashExtern;
pub use internet_checksum::InternetChecksum;
pub use meter::{Color, Meter};
pub use register::Register;
