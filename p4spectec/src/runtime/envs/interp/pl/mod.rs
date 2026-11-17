//! Definition environments used by PL execution
//!
//! Defined relations and functions carry prepared PL control flow and a frame layout.
//! Prepared expressions retain their prose annotations until evaluation.

use std::rc::Rc;

use crate::lang::common::ds::map::IdMap;

use crate::lang::pl::prepared as ast;

pub use super::shared::TDEnv;

/// Relations to their prepared definitions.
pub type REnv = IdMap<ast::RelDef>;
/// Functions to their prepared definitions, shared through `Rc`.
pub type FEnv = IdMap<Rc<ast::MetaFuncDef>>;
