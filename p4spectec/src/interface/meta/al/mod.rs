//! AL syntax and runtime values at the meta-language boundary
//!
//! `encode` and `decode` convert AL specifications and runtime values;
//! shared syntax comes from `meta::shared`.

pub mod decode;
pub mod encode;
mod mixops;
mod stub;
