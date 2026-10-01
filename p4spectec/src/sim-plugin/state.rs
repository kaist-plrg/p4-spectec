//! Mutable pipeline state held outside the runner
//!
//! The context and architecture values are threaded
//! through every specification call
//! and written back here; `txs` collects the packets one input produced.

use crate::lang::data::value::Value;

use super::io::Tx;

/// Mutable pipeline state, held outside the runner
///
/// Values belong to the accompanying runner's arena.
/// `value_arch` includes architecture work queues;
/// `txs` holds one input packet's outputs in order.
pub struct SimState {
    /// The specification's evaluation context.
    pub value_ctx: Value,
    /// The architecture state, including object states and queues.
    pub value_arch: Value,
    /// Packets transmitted while driving the current input.
    pub txs: Vec<Tx>,
}
