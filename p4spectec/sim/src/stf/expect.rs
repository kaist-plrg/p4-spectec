//! STF `expect` lines
//!
//! `matches` compares an output against an `expect` line,
//! where `*` is a wildcard nibble.

use crate::arch::io::Tx;

/// An STF `expect` line.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expectation {
    /// The expected packet; an empty payload expects any packet on the port.
    pub tx: Tx,
    /// Whether the lengths must match too, not just the given prefix.
    pub exact: bool,
}

/// Compares ASCII packets case-sensitively, with `*` matching one nibble.
pub fn matches(tx: &Tx, expect: &Expectation) -> bool {
    // Port must match; exact expectations also fix the length
    if tx.port != expect.tx.port || (expect.exact && tx.packet.len() != expect.tx.packet.len()) {
        return false;
    }
    // Otherwise the expectation is a prefix pattern
    tx.packet
        .get(..expect.tx.packet.len())
        .is_some_and(|packet| super::r#match::matches(packet, &expect.tx.packet))
}
