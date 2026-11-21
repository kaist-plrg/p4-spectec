//! The v1model switch behind the externs
//!
//! `state` is the architecture state the specification carries:
//! the packet queue (`packet`), the mirror sessions (`mirror`),
//! the multicast groups (`multicast`), and the current packet's requests.
//! `runtime` is the control plane that configures them,
//! and `pipe` the scheduler that drives one received packet
//! through the blocks and acts on the requests.

mod mirror;
mod multicast;
pub(super) mod packet;
pub(in crate::arch) mod pipe;
mod runtime;
pub(super) mod state;
