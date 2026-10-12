//! Environments used by elaboration
//!
//! Elaboration tracks types, variable dimensions, and meta-variable types,
//! plus relations, their input hints, and functions.

use crate::lang::{common::ds::map::IdMap, hints::input::InputHint};

use crate::lang::il::{ast, stage::Source};

use crate::runtime::{dim::Dim, typdef::TypeDef};

/// Type names to their definitions.
pub type TDEnv<P = Source> = IdMap<TypeDef<P>>;
/// Variables to their bound dimension.
pub type VEnv = IdMap<Dim>;
/// Meta-variables to their types.
pub type MEnv = IdMap<ast::Typ>;
/// Relations to their definitions.
pub type REnv = IdMap<ast::RelDef>;
/// Relations to their input hints.
pub type IHEnv = IdMap<InputHint>;
/// Functions to their definitions.
pub type FEnv = IdMap<ast::MetaFuncDef>;
