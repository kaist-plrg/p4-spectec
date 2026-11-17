//! Shared execution environments
//!
//! `caches` memoize calls.

pub mod caches;

use crate::lang::common::ds::map::{FrozenIdMap, MicroIdMap};

use crate::lang::il::stage::Prepared;

use crate::runtime::typdef;

/// A type definition, prepared by default for execution.
pub type TypeDef<P = Prepared> = typdef::TypeDef<P>;

/// Type names to their definitions, fixed once loading ends.
pub type FrozenTDEnv = FrozenIdMap<TypeDef>;

/// Type names to their definitions, for one or two bindings shared when a context is cloned.
pub type MicroTDEnv = MicroIdMap<TypeDef>;
