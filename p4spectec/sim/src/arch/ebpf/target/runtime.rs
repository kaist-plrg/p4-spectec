//! The eBPF control plane
//!
//! eBPF has no control plane beyond tables,
//! so every other operation fails with "not implemented".

use crate::p4::RuntimeInterface;

use super::super::Ebpf;

// == Control plane

// eBPF has no control plane beyond tables
impl RuntimeInterface for Ebpf {}
