//! Core extern objects, packets and their bit helpers
//!
//! `PacketIn` is the parser's input cursor,
//! `PacketOut` the deparser's output buffer;
//! `bits` converts between hex text, bit vectors, and integers.

pub(crate) mod bits;
pub(crate) mod packet;
pub(crate) mod packet_in;
pub(crate) mod packet_out;

pub use packet_in::PacketIn;
pub use packet_out::PacketOut;
