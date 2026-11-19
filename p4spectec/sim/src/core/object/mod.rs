//! Core extern objects, packets and their bit helpers
//!
//! `PacketIn` is the parser's input cursor,
//! `PacketOut` the deparser's output buffer;
//! `bits` converts between hex text, bit vectors, and integers.

pub mod bits;
pub mod packet;
pub mod packet_in;
pub mod packet_out;

pub use bits::{bits_to_int_unsigned, bits_to_string, string_to_bits};
pub use packet_in::PacketIn;
pub use packet_out::PacketOut;
