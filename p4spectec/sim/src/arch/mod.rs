//! The architectures the simulator runs, and what they share
//!
//! Each of `ebpf`, `psa`, and `v1model` implements `p4::Architecture`
//! and `p4::RuntimeInterface`;
//! its `target::pipe` scheduler drives one packet over a `p4::Sim`.
//! `dummy` is the extern for running a program without an architecture.
//! `core` holds the core.p4 externs every architecture shares,
//! and `state`, `io`, and `hash` the pipeline state, packets, and hash algorithms.

pub(crate) mod core;
pub(crate) mod dummy;
pub(crate) mod ebpf;
pub(crate) mod hash;
pub(crate) mod io;
pub(crate) mod psa;
pub(crate) mod state;
pub(crate) mod v1model;
