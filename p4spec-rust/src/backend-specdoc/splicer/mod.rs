//! Skeleton marker replacement for specification documents
//!
//! Marker parsers consume identifiers from a byte-positioned source.
//! Rendering preserves the surrounding document text.

pub mod error;
pub mod parser;
pub mod source;

pub use error::Error;
