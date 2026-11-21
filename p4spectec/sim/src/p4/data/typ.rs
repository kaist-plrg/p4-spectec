//! P4 types as the simulator sees them
//!
//! The simulator never takes a type apart:
//! it asks the specification for a type's default value, its size,
//! or a cast to it, through `P4Ops`.
//! So `P4Type` only holds the specification's type value.

use crate::spec::SpecValue;

/// A P4 type, opaque to the simulator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct P4Type(pub(crate) SpecValue);
