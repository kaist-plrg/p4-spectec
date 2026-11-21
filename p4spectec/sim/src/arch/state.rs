//! Mutable pipeline state held outside the runner
//!
//! The context and architecture values are threaded
//! through every specification call
//! and written back here; `txs` collects the packets one input produced.

use crate::p4::SpecValue;

use super::io::Tx;

/// Mutable pipeline state, held outside the runner.
///
/// Values belong to the shared execution arena.
/// `value_arch` includes architecture work queues;
/// `txs` holds one input packet's outputs in order.
pub struct SimState {
    /// The specification's evaluation context.
    pub value_ctx: SpecValue,
    /// The architecture state, including object states and queues.
    pub value_arch: SpecValue,
    /// Packets transmitted while driving the current input.
    pub txs: Vec<Tx>,
}

impl SimState {
    /// The state of a packet in the context `value_ctx` and the architecture state `value_arch`,
    /// before it transmits anything.
    pub fn new(value_ctx: SpecValue, value_arch: SpecValue) -> Self {
        Self { value_ctx, value_arch, txs: Vec::new() }
    }
}
