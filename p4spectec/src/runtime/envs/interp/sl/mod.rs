//! Definition environments used by SL execution
//!
//! Relations and functions are stored prepared; those with a body carry their frame layouts.

use std::rc::Rc;

use crate::lang::common::ds::map::IdMap;

use crate::lang::sl::prepared as ast;

pub use super::shared::TDEnv;

/// Relations to their prepared definitions.
pub type REnv = IdMap<ast::RelDef>;
/// Functions to their prepared definitions, shared through `Rc`.
pub type FEnv = IdMap<Rc<ast::MetaFuncDef>>;
