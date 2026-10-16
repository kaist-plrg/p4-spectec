//! Shared execution environments and callable frames
//!
//! `frame` maps names to slots,
//! `callable` pairs prepared syntax with its layout,
//! `caches` memoize calls.

pub mod caches;
pub mod callable;
pub mod frame;

use crate::lang::common::ds::map::IdMap;

use crate::lang::il::stage::Prepared;

use crate::runtime::typdef;

/// A type definition, prepared by default for execution.
pub type TypeDef<P = Prepared> = typdef::TypeDef<P>;

/// Type names to their definitions.
pub type TDEnv<P = Prepared> = IdMap<TypeDef<P>>;
