//! Shared execution environments and callable frames
//!
//! `frame` maps names to slots,
//! `callable` pairs prepared syntax with its layout,
//! `caches` memoize calls.

pub mod caches;
pub mod callable;
pub mod frame;

use crate::lang::common::ds::map::IdMap;

use crate::runtime::typdef::TypeDef;

/// Type names to their definitions.
pub type TDEnv = IdMap<TypeDef>;
