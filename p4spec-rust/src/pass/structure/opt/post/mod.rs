//! Remove dead bindings and singleton-variant matches after loop rewrites
//!
//! `let y = 1 { return x }` becomes `return x` when `y` is unused.
//! These rewrites run once after the loop reaches a fixed point.

pub(crate) mod remove_let_dead;
pub(crate) mod remove_match_singleton;

use crate::{
    pass::structure::{StructureError, ol::ast::Block},
    runtime::envs::algo::TDEnv,
};

// == Optimization

/// Removes dead lets, then singleton matches.
pub(super) fn optimize(tdenv: &TDEnv, block: Block) -> Result<Block, StructureError> {
    let block = remove_let_dead::apply(block);
    let block = remove_match_singleton::apply(tdenv, block)?;
    Ok(block)
}
