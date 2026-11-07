//! Common syntax of spec-meta at the meta-language boundary
//!
//! Types, expressions, paths, arguments, and parameters,
//! which `al` builds AL syntax from.

mod cache;
pub mod decode;
pub mod encode;
mod error;
pub(in crate::interface::meta) mod mixops;
pub(in crate::interface::meta) mod stub;

pub use cache::Cache;
pub use error::{Error, ErrorKind};
