//! Iterations over fixed positions, analyzed once during preparation
//!
//! ```text
//! (b, a)*   compose  Tuple with idxs [1, 0]
//!           zip      a* = [1, 3], b* = [2, 4]  into  [(2, 1), (4, 3)]
//! ```
//!
//! Preparation records the result in the iteration,
//! so evaluation dispatches on it without reading the body again.

use crate::interp::shared::util::find_slot_of_exp;
use crate::lang::data::value::flat::zipper::{Comp, Shape};
use crate::lang::il::prepared as ast;

/// Composes a list iteration whose body constructs from all its variables,
/// or returns `None` when the body computes anything else.
///
/// ```text
/// expression  (b, a)*   vars [a, b]
/// comp        Tuple with idxs [1, 0]: argument 0 is b, argument 1 is a
///
/// (a, b)?       None: not a list iteration
/// (a, b + 1)*   None: an argument computes
/// (a, a)*       None: b is no argument, so its length would go unchecked
/// ```
pub(crate) fn compose(exp_inner: &ast::Exp, iter: ast::Iter, vars: &[ast::Var]) -> Option<Comp> {
    // Only a list iteration zips its outer values
    if iter != ast::Iter::List {
        return None;
    }
    // The body must be a tuple, list, or case constructor
    let (shape, exps) = match &exp_inner.node {
        ast::ExpKind::Tuple(exps) => (Shape::Tuple, exps.as_slice()),
        ast::ExpKind::List(exps) => (Shape::List, exps.as_slice()),
        ast::ExpKind::Case(not_exp) => (Shape::Case(*not_exp.mixop()), not_exp.args()),
        _ => return None,
    };
    // Each argument must be one iterated variable
    let idxs: Vec<usize> = exps
        .iter()
        .map(|exp| {
            let slot = find_slot_of_exp(exp)?;
            vars.iter().position(|var| var.slot == slot)
        })
        .collect::<Option<_>>()?;
    // Every variable must be an argument, so that zipping checks every length
    if (0..vars.len()).any(|idx| !idxs.contains(&idx)) {
        return None;
    }
    Some(Comp { shape, idxs })
}
