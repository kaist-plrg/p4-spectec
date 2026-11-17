//! Fixed iteration patterns, analyzed to unzip
//!
//! ```text
//! (a, b)* <- [(1, 2), (3, 4)]   decompose  a at [Tuple(0)], b at [Tuple(1)]
//!                               unzip      a* = [1, 3], b* = [2, 4]
//! ```
//!
//! `decompose` reads only syntax; `None` sends the caller
//! to the per-element path in `assign`.

use crate::interp::shared::util::find_slot_of_exp;
use crate::lang::data::value::flat::zipper::{Decomp, Step};
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
