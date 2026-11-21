//! P4 types and values as the simulator sees them
//!
//! `value` has `P4Value`, a P4 runtime value as a plain Rust enum,
//! with the `P4Fields` of a struct or header;
//! `typ` has `P4Type`, the specification's type value held opaque.
//! `SpecValue`, from `spec`, is any other specification value the simulator holds opaque.

mod typ;
mod value;

pub(crate) use typ::P4Type;
pub(crate) use value::{P4Fields, P4Value};

pub(crate) use crate::spec::SpecValue;
