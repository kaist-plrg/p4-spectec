//! Revive used underscore names and remove ticks until names stop changing
//!
//! An input `_x''` used by `Return(_x'')` becomes `x` and `Return(x)`.
//!
//! Revival removes leading underscores; tick cleanup shortens suffixes.
//! Actual name changes cannot cancel within a round, including iterator names.

pub(super) mod rename_tick;
pub(super) mod revive_underscore;

use crate::lang::il::ast::{Arg, Exp};

use super::ol::ast::Block;

// == Relations

/// Prettifies a relation's inputs and blocks until a round changes nothing.
pub(crate) fn pretty_rel(
    exps_match: Vec<Exp>,
    block: Block,
    block_else: Option<Block>,
) -> (Vec<Exp>, Block, Option<Block>) {
    let mut body = (exps_match, block, block_else);
    loop {
        let mut changed = false;
        body = revive_underscore::apply_rel(&mut changed, body);
        body = rename_tick::apply_rel(&mut changed, body);
        if !changed {
            return body;
        }
    }
}

// == Functions

/// Prettifies a function's arguments and blocks until a round changes nothing.
pub(crate) fn pretty_func(
    args_input: Vec<Arg>,
    block: Block,
    block_else: Option<Block>,
) -> (Vec<Arg>, Block, Option<Block>) {
    let mut body = (args_input, block, block_else);
    loop {
        let mut changed = false;
        body = revive_underscore::apply_func(&mut changed, body);
        body = rename_tick::apply_func(&mut changed, body);
        if !changed {
            return body;
        }
    }
}
