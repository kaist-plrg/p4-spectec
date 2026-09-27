//! Optimize OL blocks with pre-rewrites, loop rewrites, then post-rewrites
//!
//! `let y = x { return y }` becomes `return x` during pre-rewrites.
//! Loop rewrites repeat until no merge succeeds; post-rewrites then run once.

pub(super) mod r#loop;
pub(super) mod merge;
pub(super) mod overlap;
pub(super) mod post;
pub(super) mod pre;

use super::{StructureError, ol::ast::Block};
use crate::runtime::envs::algo::TDEnv;

// == Optimization

/// Runs the pre, loop, and post rewrites in order.
pub(super) fn optimize(
    tdenv: &TDEnv,
    block: Block,
    without_rule_groups: bool,
) -> Result<Block, StructureError> {
    let block = pre::optimize(block, without_rule_groups);
    let block = r#loop::optimize(tdenv, block)?;
    post::optimize(tdenv, block)
}
