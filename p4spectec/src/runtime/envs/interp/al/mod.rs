//! Definition environments used by AL execution
//!
//! Relations and functions are stored prepared, with their frame layouts.

use std::rc::Rc;

use crate::lang::common::ds::map::IdMap;

use crate::lang::al::prepared as ast;

use super::shared::callable::Callable;

pub use super::shared::TDEnv;

/// Relations to their prepared callables.
pub type REnv = IdMap<Callable<ast::RelDef>>;
/// Functions to their prepared callables, shared through `Rc`.
pub type FEnv = IdMap<Rc<Callable<ast::MetaFuncDef>>>;
