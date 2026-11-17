//! Definition environments used by AL execution
//!
//! Relations and functions are stored prepared; each rule group, table row, and clause
//! carries its frame layout.

use std::rc::Rc;

use crate::lang::common::ds::map::IdMap;

use crate::lang::al::prepared as ast;

pub use super::shared::TDEnv;

/// Relations to their prepared definitions.
pub type REnv = IdMap<ast::RelDef>;
/// Functions to their prepared definitions, shared through `Rc`.
pub type FEnv = IdMap<Rc<ast::MetaFuncDef>>;
