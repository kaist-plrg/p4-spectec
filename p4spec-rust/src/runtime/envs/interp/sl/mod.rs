//! Definition environments used by SL execution
//!
//! Relations and functions are stored prepared, with their frame layouts.

pub mod ast_prepared;

use std::rc::Rc;

use crate::lang::common::ds::map::IdMap;

use super::shared::callable::Callable;

pub use super::shared::TDEnv;

use self::ast_prepared as ast;
/// Relations to their prepared callables.
pub type REnv = IdMap<Callable<ast::RelDef>>;
/// Functions to their prepared callables, shared through `Rc`.
pub type FEnv = IdMap<Rc<Callable<ast::MetaFuncDef>>>;
