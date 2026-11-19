//! Stateful v1model extern objects
//!
//! Each object is read from the specification's object state,
//! updated by its method, and stored back by the pipeline.
//! Doc comments summarize the object's description in `v1model.p4`.

pub mod counter;
pub mod direct_counter;
pub mod direct_meter;
pub mod register;

pub use counter::Counter;
pub use direct_counter::DirectCounter;
pub use direct_meter::DirectMeter;
pub use register::Register;
