//! Shared execution environments
//!
//! `caches` memoize calls.

pub mod caches;

use crate::lang::common::ds::map::{FrozenIdMap, IdMap};

use crate::lang::il::stage::Prepared;

use crate::runtime::typdef;

/// A type definition, prepared by default for execution.
pub type TypeDef<P = Prepared> = typdef::TypeDef<P>;

/// Type names to their definitions.
pub type TDEnv<P = Prepared> = IdMap<TypeDef<P>>;

/// Type names to their definitions, fixed once loading ends.
pub type FrozenTDEnv = FrozenIdMap<TypeDef>;
