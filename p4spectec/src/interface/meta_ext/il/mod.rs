//! Elaborated IL at the meta-language boundary
//!
//! `encode` turns an `il::Spec` into a meta `specIL` value
//! for the IL-to-AL specification, and `decode` reads it back.
//! Shared forms use `meta::shared`; relation arguments stay in notation order
//! next to their input positions.

pub mod decode;
pub mod encode;
mod mixops;
mod stub;
