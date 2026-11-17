//! Decomposition and composition of iterations over fixed positions
//!
//! `decompose` finds the steps from an element of `(a, b)*` to `a` and `b`,
//! so that `(a, b)* <- xs` can unzip `xs` into the columns `a*` and `b*`;
//! `compose` finds the constructor and argument variables of `(a, b)*`,
//! so that evaluating it can zip the columns `a*` and `b*` into elements.
//! Both apply only when every variable sits at a fixed position in the element,
//! so no context is built per element.
//! Patterns and expressions that compute values or nest iterations
//! take the general path in `assign` and `iter`.

use crate::interp::shared::util::find_slot_of_exp;
use crate::lang::data::value::flat::zipper::{Comp, Decomp, Shape, Step};
use crate::lang::il::prepared as ast;

/// Decomposes a fixed pattern into steps to each variable, in `vars` order,
/// or returns `None` when any part of it computes a value.
///
/// For `(a, (b, c))* <- xs` over `a`, `b`, and `c`, the pattern `(a, (b, c))`
/// decomposes into `[[Tuple(0)], [Tuple(1), Tuple(0)], [Tuple(1), Tuple(1)]]`;
/// for `(a, y :: ys)* <- xs`, the cons `y :: ys` computes a tail,
/// so there is no decomposition.
pub(super) fn decompose(exp: &ast::Exp, vars: &[ast::Var]) -> Option<Decomp> {
    let mut steps_by_var = vec![None; vars.len()];
    decompose_inner(exp, vars, &mut Vec::new(), &mut steps_by_var)?;
    // Every variable must occur in the pattern
    steps_by_var.into_iter().collect()
}

/// Records the steps to each variable, failing on a computed part.
///
/// Inside `(a, (b, c))`, reaching `b` with `steps` = `[Tuple(1)]`
/// records `[Tuple(1), Tuple(0)]` for `b`.
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
/// For `(b, a)*` over `a` and `b`,
/// the tuple composes with the variable indices `[1, 0]`;
/// `(a, b + 1)*` computes an argument and `(a, a)*` omits `b`,
/// so neither composes.
pub(super) fn compose(exp_inner: &ast::Exp, exp_iter: &ast::ExpIter) -> Option<Comp> {
    // Only a list iteration zips its columns
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
