//! Conversions between source AL and the flat AL meta specification
//!
//! Decoded syntax supports editing and re-encoding. Type notes, general hints,
//! and printing-only signatures are reconstructed canonically; decoding does
//! not produce a replacement for a compiler-checked native program.
//! Every runtime value belongs to the arena passed by the caller.

pub mod decode;
pub mod encode;
mod mixops;
mod typs;
