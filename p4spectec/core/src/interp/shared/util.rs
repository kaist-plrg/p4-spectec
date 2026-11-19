//! Resolve simple iterated-variable expressions through their prepared outer variables
//!
//! `find_slot_of_exp` maps `x`, or `x*` read whole, to its slot;
//! `iterate_vars` returns the variables one iteration outward.

use crate::lang::data::var::SlotIdx;

use crate::lang::il::prepared as ast;

/// The variables one iteration outward, prepared with their iteration.
pub fn iterate_vars(vars: &[ast::Var]) -> Vec<ast::Var> {
    vars.iter().map(|var| var.outer().clone()).collect()
}

/// The slot of a variable, or of an iterated variable read whole.
///
/// ```text
/// x      x's slot
/// x*     x*'s slot, when chosen whole
/// x**    x**'s slot, when chosen whole
/// x + 1  None
/// ```
pub fn find_slot_of_exp(exp: &ast::Exp) -> Option<SlotIdx> {
    match &exp.node {
        ast::ExpKind::Id(id) => Some(id.slot),
        ast::ExpKind::Iter(_, ast::ExpIter::Whole { var, .. }) => Some(var.outer().slot),
        _ => None,
    }
}
