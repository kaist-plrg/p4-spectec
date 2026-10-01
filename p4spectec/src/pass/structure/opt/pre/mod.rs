//! Remove groups on request, substitute aliases, then turn tests into matches
//!
//! `let y = x { return y }` becomes `return x` before loop rewrites.

pub(crate) mod matchify_if_eq_terminal;
pub(crate) mod remove_group;
pub(crate) mod remove_let_alias;

use crate::pass::structure::ol::ast::Block;

// == Optimization

/// Removes groups when requested, then aliases, then rewrites terminal tests.
pub(super) fn optimize(block: Block, without_rule_groups: bool) -> Block {
    let block = if without_rule_groups { remove_group::apply(block) } else { block };
    let block = remove_let_alias::apply(block);
    matchify_if_eq_terminal::apply(block)
}
