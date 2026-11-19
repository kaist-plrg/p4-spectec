//! Packet transmissions and STF expectations
//!
//! A packet is a port and its hex payload;
//! `matches` compares an output against an `expect` line,
//! where `*` is a wildcard nibble.

/// A packet on a port.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Transmission {
    /// Port number.
    pub port: usize,
    /// Payload as uppercase hex digits.
    pub packet: String,
}

/// A received (input) packet.
pub type Rx = Transmission;
/// A transmitted (output) packet.
pub type Tx = Transmission;

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
        .is_some_and(|packet| crate::stf::r#match::matches(packet, &expect.tx.packet))
}

impl std::fmt::Display for Transmission {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "({})", self.port)?;
        if !self.packet.is_empty() {
            write!(formatter, " {}", self.packet)?;
        }
        Ok(())
    }
}
