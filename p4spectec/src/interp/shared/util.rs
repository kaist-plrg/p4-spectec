//! Resolve simple iterated-variable expressions through their prepared outer variables
//!
//! `find_slot_of_exp` maps `x*` used as an expression to its slot;
//! `iterate_vars` returns the variables one iteration outward.

use crate::lang::data::var::SlotIdx;

use crate::lang::il::prepared as ast;

/// The variables one iteration outward, prepared with their iteration.
pub fn iterate_vars(vars: &[ast::Var]) -> Vec<ast::Var> {
    vars.iter().map(|var| var.outer().clone()).collect()
}

/// Recognizes identity iterations using prepared slots without cloning syntax.
pub fn find_slot_of_exp(exp: &ast::Exp) -> Option<SlotIdx> {
    match &exp.node {
        // A plain variable already has its slot
        ast::ExpKind::Id(id) => Some(id.slot),
        // Matching slots imply the same name and the same iteration path
        ast::ExpKind::Iter(exp_inner, ast::ExpIter { vars, .. }) => {
            let [var] = vars.as_slice() else {
                return None;
            };
            let slot = find_slot_of_exp(exp_inner)?;
            (slot == var.slot).then(|| var.outer().slot)
        }
        // Computed expressions require normal iteration evaluation
        _ => None,
    }
}
