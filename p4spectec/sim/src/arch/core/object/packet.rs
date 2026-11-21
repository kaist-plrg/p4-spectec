//! Reassembling an output packet
//!
//! The emitted headers come first, then whatever of the input was not parsed.

use p4spectec::runner::ExternError;

use super::{PacketIn, PacketOut, bits::bits_to_string};

/// The output packet as hex: emitted bits followed by the unparsed payload.
pub fn to_string(packet_in: &PacketIn, packet_out: &PacketOut) -> Result<String, ExternError> {
    let bits: Vec<_> = packet_out
        .bits
        .iter()
        .copied()
        .chain(packet_in.payload()?.iter().copied())
        .collect();
    Ok(bits_to_string(&bits))
}
