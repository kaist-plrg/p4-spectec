//! Environments used by algorithmic conversion
//!
//! Binding analysis tracks types, variable dimensions, and meta-variable types.

use crate::lang::common::ds::map::IdMap;

use crate::lang::il::ast;

use crate::runtime::{dim::Dim, typdef::TypeDef};

/// Type names to their definitions.
pub type TDEnv = IdMap<TypeDef>;
/// Variables to their bound dimension.
pub type VEnv = IdMap<Dim>;
/// Meta-variables to their types.
pub type MEnv = IdMap<ast::Typ>;
