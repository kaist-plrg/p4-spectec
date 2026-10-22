//! Syntax shared by IL and AL at the meta-language boundary
//!
//! Types, expressions, paths, arguments, parameters, and iteration premises
//! have one meta form for both stages.

mod cache;
pub mod decode;
pub mod encode;
mod error;
pub(in crate::interface::meta) mod mixops;
pub(in crate::interface::meta) mod stub;

pub use cache::Cache;
pub use error::{Error, ErrorKind};
