//! Resolve simple iterated-variable expressions through their callable layout
//!
//! `find_var_of_exp` maps `x*` used as an expression back to the slot of the iterated
//! variable; `iterate_vars` computes the slots one iteration outward.

use crate::lang::data::var::{SlotIdx, Var, VarSlot};

use crate::lang::il::prepared as ast;

use super::context::ReadContext;

/// Advances prepared variables through one iterator dimension.
pub fn iterate_vars(ctx: &impl ReadContext, vars: &[ast::Var], iter: ast::Iter) -> Vec<ast::Var> {
    vars.iter()
        .map(|var| ctx.find_var_iterated(var, iter))
        .collect()
}

/// Recognizes identity iterations using prepared slots without cloning syntax.
#[inline]
pub fn find_slot_of_exp(ctx: &impl ReadContext, exp: &ast::Exp) -> Option<SlotIdx> {
    match &exp.node {
        // A plain variable already has its slot
        ast::ExpKind::Id(id) => Some(id.slot),
        // Matching slots imply the same name and the same iteration path
        ast::ExpKind::Iter(exp_inner, ast::ExpIter { iter, vars }) => {
            let [var] = vars.as_slice() else {
                return None;
            };
            let slot = match &exp_inner.node {
                ast::ExpKind::Id(id) => id.slot,
                _ => find_nested_slot(ctx, exp_inner)?,
            };
            (slot == var.slot).then(|| ctx.find_slot_iterated(var, *iter))
        }
        // Computed expressions require normal iteration evaluation
        _ => None,
    }
}

/// Keeps recursive identity iterations out of the common inline slot lookup.
#[inline(never)]
fn find_nested_slot(ctx: &impl ReadContext, exp: &ast::Exp) -> Option<SlotIdx> {
    find_slot_of_exp(ctx, exp)
}

/// Finds the slot-backed variable represented by a simple iterated expression.
pub fn find_var_of_exp(ctx: &impl ReadContext, exp: &ast::Exp) -> Option<VarSlot> {
    match &exp.node {
        // A plain variable is its own slot
        ast::ExpKind::Id(id) => Some(VarSlot::new(
            id.slot,
            Var {
                id: id.id.clone(),
                typ: crate::phrase!(node: exp.note.as_ref().clone(), span: exp.span),
                iters: vec![],
            },
        )),
        // `x*` must be a single-variable iteration over `x` itself
        ast::ExpKind::Iter(exp_inner, ast::ExpIter { iter, vars }) => {
            let [var] = vars.as_slice() else {
                return None;
            };
            let var_inner = find_var_of_exp(ctx, exp_inner)?;
            if var_inner.var.id.node != var.var.id.node || var_inner.var.iters != var.var.iters {
                return None;
            }
            Some(ctx.find_var_iterated(&var_inner, *iter))
        }
        // Anything else is a computed expression
        _ => None,
    }
}
