//! Parsed EL at the meta-language boundary
//!
//! `encode` turns an `el::Spec` into a meta `specEL` value
//! for the elaboration specification; EL is never decoded.

pub mod encode;
mod mixops;
