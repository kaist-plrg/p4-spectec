//! The eBPF switch behind the externs
//!
//! `runtime` is the control plane, which has nothing beyond tables,
//! and `pipe` the pipeline that parses, filters, and forwards one received packet.
//! eBPF keeps no architecture state of its own.

pub(in crate::arch) mod pipe;
mod runtime;
