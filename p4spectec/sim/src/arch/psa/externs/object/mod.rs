//! Stateful PSA extern objects, from counters to checksums
//!
//! Each object is read from the specification's object state,
//! updated by its method, and stored back by the dispatcher.
//! Doc comments summarize the object's description in `psa.p4`.

pub(crate) mod counter;
pub(crate) mod hash;
pub(crate) mod internet_checksum;
pub(crate) mod meter;
pub(crate) mod register;

pub use counter::Counter;
pub use hash::HashExtern;
pub use internet_checksum::InternetChecksum;
pub use meter::Meter;
pub use register::Register;
