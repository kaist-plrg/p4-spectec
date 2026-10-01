//! Definition environments used by PL execution
//!
//! Callables contain prepared PL control flow and a frame layout.
//! Prepared expressions retain their prose annotations until evaluation.

pub mod ast_prepared;

use std::rc::Rc;

use crate::lang::common::ds::map::IdMap;

use crate::runtime::envs::interp::shared::callable::Callable;

pub use super::shared::TDEnv;

use ast_prepared as ast;

/// Relations to their prepared callables.
pub type REnv = IdMap<Callable<ast::RelDef>>;
/// Functions to their prepared callables, shared through `Rc`.
pub type FEnv = IdMap<Rc<Callable<ast::MetaFuncDef>>>;
