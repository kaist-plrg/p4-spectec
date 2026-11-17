//! Fixed iteration patterns and constructors, analyzed to unzip and zip
//!
//! ```text
//! (a, b)* <- [(1, 2), (3, 4)]   decompose  a at [Tuple(0)], b at [Tuple(1)]
//!                               unzip      a* = [1, 3], b* = [2, 4]
//! (b, a)*                       compose    Tuple with idxs [1, 0]
//!                               zip        [(2, 1), (4, 3)]
//! ```
//!
//! Both read only syntax; `None` sends the caller
//! to the per-element path in `assign` and `iter`.

use crate::interp::shared::util::find_slot_of_exp;
use crate::lang::data::value::flat::zipper::{Comp, Decomp, Shape, Step};
use crate::lang::il::prepared as ast;

/// Decomposes a fixed pattern into steps to each variable, in `vars` order,
/// or returns `None` when any part of it computes a value.
///
/// ```text
/// pattern  (a, (b, c))   vars [a, b, c]
/// decomp   [[Tuple(0)], [Tuple(1), Tuple(0)], [Tuple(1), Tuple(1)]]
///
/// (a, y :: ys)   None: the cons builds a tail per element
/// (a, 1)         None: the constant is not a variable
/// a              None for vars [a, b]: b does not occur
/// ```
pub(super) fn decompose(exp: &ast::Exp, vars: &[ast::Var]) -> Option<Decomp> {
    let mut steps_by_var = vec![None; vars.len()];
    decompose_inner(exp, vars, &mut Vec::new(), &mut steps_by_var)?;
    // Every variable must occur in the pattern
    steps_by_var.into_iter().collect()
}

/// Records the steps to each variable, failing on a computed part.
///
/// ```text
/// visit (a, (b, c))   steps []
/// visit a             steps [Tuple(0)]             records a
/// visit (b, c)        steps [Tuple(1)]
/// visit b             steps [Tuple(1), Tuple(0)]   records b
/// visit c             steps [Tuple(1), Tuple(1)]   records c
/// ```
fn decompose_inner(
    exp: &ast::Exp,
    vars: &[ast::Var],
    steps: &mut Vec<Step>,
    steps_by_var: &mut [Option<Vec<Step>>],
) -> Option<()> {
    // A variable or an identity iteration ends its steps
    if let Some(slot) = find_slot_of_exp(exp) {
        if let Some(idx) = vars.iter().position(|var| var.slot == slot) {
            steps_by_var[idx] = Some(steps.clone());
        }
        return Some(());
    }
    // Every child extends the steps by its position
    let mut find_child = |step: Step, exp_child: &ast::Exp| {
        steps.push(step);
        let found = decompose_inner(exp_child, vars, steps, steps_by_var);
        steps.pop();
        found
    };
    match &exp.node {
        // Tuple: positional components
        ast::ExpKind::Tuple(exps) => exps
            .iter()
            .enumerate()
            .try_for_each(|(idx, exp_child)| find_child(Step::Tuple(idx), exp_child)),
        // List literal: positional elements
        ast::ExpKind::List(exps) => exps
            .iter()
            .enumerate()
            .try_for_each(|(idx, exp_child)| find_child(Step::List(idx), exp_child)),
        // Case: positional arguments
        ast::ExpKind::Case(not_exp) => not_exp
            .args()
            .iter()
            .enumerate()
            .try_for_each(|(idx, exp_child)| find_child(Step::Case(idx), exp_child)),
        // Struct: fields in order
        ast::ExpKind::Str(exp_fields) => exp_fields
            .iter()
            .enumerate()
            .try_for_each(|(idx, exp_field)| find_child(Step::Struct(idx), &exp_field.exp)),
        // Option: the payload, if present
        ast::ExpKind::Opt(Some(exp_child)) => find_child(Step::Opt, exp_child),
        // Absent option: nothing to bind
        ast::ExpKind::Opt(None) => Some(()),
        // Anything else computes a value
        _ => None,
    }
}

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
pub(super) fn compose(exp_inner: &ast::Exp, exp_iter: &ast::ExpIter) -> Option<Comp> {
    // Only a list iteration zips its outer values
    if exp_iter.iter != ast::Iter::List {
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
            exp_iter.vars.iter().position(|var| var.slot == slot)
        })
        .collect::<Option<_>>()?;
    // Every variable must be an argument, so that zipping checks every length
    if (0..exp_iter.vars.len()).any(|idx| !idxs.contains(&idx)) {
        return None;
    }
    Some(Comp { shape, idxs })
}
