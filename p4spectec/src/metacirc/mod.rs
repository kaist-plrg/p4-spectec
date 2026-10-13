//! Meta-circular representations of source syntax and runtime values
//!
//! AL conversions borrow the caller's arena and preserve the information
//! represented by `spec-meta`. Runner construction and host dispatch live
//! outside this conversion boundary.

pub mod al;
mod error;

pub use error::{Error, ErrorKind};
