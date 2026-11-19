//! Packet transmissions
//!
//! A packet is a port and its hex payload;
//! `Rx` enters a pipeline and `Tx` leaves it.

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

impl std::fmt::Display for Transmission {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "({})", self.port)?;
        if !self.packet.is_empty() {
            write!(formatter, " {}", self.packet)?;
        }
        Ok(())
    }
}
