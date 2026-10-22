//! Elaborated IL at the meta-language boundary
//!
//! `encode` turns an `il::Spec` into a meta `specIL` value
//! for the IL-to-AL specification, and `decode` reads it back.
//! Shared forms reuse the AL codec; relation arguments stay in notation order
//! next to their input positions.

pub mod decode;
pub mod encode;
mod mixops;
mod stub;

pub use super::al::{Cache, Error, ErrorKind};
