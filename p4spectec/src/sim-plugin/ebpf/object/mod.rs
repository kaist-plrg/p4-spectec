//! Stateful eBPF extern objects
//!
//! The eBPF model has one: a counter array.

pub mod counter_array;

pub use counter_array::CounterArray;
