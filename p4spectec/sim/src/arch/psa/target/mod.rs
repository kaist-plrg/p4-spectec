//! The PSA switch behind the externs
//!
//! `state` is the architecture state the specification carries:
//! the packet queue (`packet`), the clone sessions (`mirror`),
//! and the multicast groups (`multicast`).
//! `runtime` is the control plane that configures them,
//! and `pipe` the scheduler that drives one received packet
//! through both pipelines and the PRE and BQE.

mod mirror;
mod multicast;
mod packet;
pub(in crate::arch) mod pipe;
mod runtime;
pub(super) mod state;
