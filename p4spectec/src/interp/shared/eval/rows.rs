//! Projection of destructured list rows into binding columns
//!
//! Fixed tuple, case, struct, option, and list patterns only select children.
//! Resolve each output slot's final selection once, then collect without
//! constructing a context for every row. Patterns that construct tails or
//! nested iterations retain the general assignment path.

use crate::interp::shared::util::find_slot_of_exp;
use crate::lang::data::{
    arena::Arena,
    value::{
        ValueError,
        flat::{Value, projection::Step},
    },
};
use crate::lang::il::prepared as ast;

/// Records the position of each variable in a fixed pattern,
/// failing on any part that computes a value.
fn find_paths(
    exp: &ast::Exp,
    vars: &[ast::Var],
    path: &mut Vec<Step>,
    paths: &mut [Option<Vec<Step>>],
) -> Option<()> {
    // A variable or an identity iteration ends its path
    if let Some(slot) = find_slot_of_exp(exp) {
        if let Some(idx) = vars.iter().position(|var| var.slot == slot) {
            paths[idx] = Some(path.clone());
        }
        return Some(());
    }
    // Every child extends the path by its position
    let mut find_child = |step: Step, exp_child: &ast::Exp| {
        path.push(step);
        let found = find_paths(exp_child, vars, path, paths);
        path.pop();
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

/// Collects fixed-pattern bindings, or requests general row assignment.
pub(super) fn collect(
    arena: &mut Arena,
    exp: &ast::Exp,
    vars: &[ast::Var],
    vars_outer: &[ast::Var],
    value: Value,
) -> Option<Result<Vec<Value>, ValueError>> {
    // Short flat patterns need no owned path vectors on a cache hit.
    if vars.len() <= 4 {
        let flat = match &exp.node {
            ast::ExpKind::Tuple(exps) => Some((Step::Tuple as fn(usize) -> Step, exps.as_slice())),
            ast::ExpKind::List(exps) => Some((Step::List as fn(usize) -> Step, exps.as_slice())),
            ast::ExpKind::Case(not_exp) => Some((Step::Case as fn(usize) -> Step, not_exp.args())),
            _ => None,
        };
        if let Some((step, exps)) = flat {
            let mut steps = [Step::Opt; 4];
            let mut assigned = [false; 4];
            let simple = exps.iter().enumerate().all(|(index, exp)| {
                let Some(slot) = find_slot_of_exp(exp) else {
                    return false;
                };
                if let Some(index_var) = vars.iter().position(|var| var.slot == slot) {
                    steps[index_var] = step(index);
                    assigned[index_var] = true;
                }
                true
            });
            if simple && assigned[..vars.len()].iter().all(|assigned| *assigned) {
                let paths: [&[Step]; 4] =
                    std::array::from_fn(|index| std::slice::from_ref(&steps[index]));
                return Some(arena.value.project_rows(
                    &arena.mixop,
                    value.node,
                    &paths[..vars.len()],
                    vars_outer.iter().map(|var| var.typ.clone()),
                ));
            }
        }
    }
    let mut paths_out = vec![None; vars.len()];
    find_paths(exp, vars, &mut Vec::new(), &mut paths_out)?;
    let paths_out: Vec<_> = paths_out.into_iter().collect::<Option<_>>()?;
    let paths: Vec<_> = paths_out.iter().map(Vec::as_slice).collect();
    Some(arena.value.project_rows(
        &arena.mixop,
        value.node,
        &paths,
        vars_outer.iter().map(|var| var.typ.clone()),
    ))
}

/// Maps a fixed constructor over columns; computed row expressions fall back.
pub(super) fn map(
    arena: &mut Arena,
    ctx: &impl crate::interp::shared::context::IterContext,
    exp: &ast::Exp,
    exp_inner: &ast::Exp,
    exp_iter: &ast::ExpIter,
) -> Option<crate::interp::shared::backtrack::Backtrack<Value>> {
    use crate::interp::shared::backtrack::{ok, unwrap_from_result};
    use crate::interp::shared::util::iterate_vars;
    use crate::lang::data::value::flat::projection::Shape;
    if exp_iter.iter != ast::Iter::List {
        return None;
    }
    let (shape, exps) = match &exp_inner.node {
        ast::ExpKind::Tuple(exps) => (Shape::Tuple, exps.as_slice()),
        ast::ExpKind::List(exps) => (Shape::List, exps.as_slice()),
        ast::ExpKind::Case(not_exp) => (Shape::Case(*not_exp.mixop()), not_exp.args()),
        _ => return None,
    };
    // Each row argument must be one iterated input
    let indices: Vec<usize> = exps
        .iter()
        .map(|exp| {
            let slot = find_slot_of_exp(exp)?;
            exp_iter.vars.iter().position(|var| var.slot == slot)
        })
        .collect::<Option<_>>()?;
    // Every iteration input must be accounted for, including length checks
    if (0..exp_iter.vars.len()).any(|idx| !indices.contains(&idx)) {
        return None;
    }
    Some((|| {
        // Inputs must be lists of one length
        let vars_outer = iterate_vars(&exp_iter.vars);
        unwrap_from_result!(ctx.find_list_values_by_var(arena, &vars_outer), &exp.span);
        let columns: Vec<_> = indices
            .iter()
            .map(|idx| {
                ctx.find_value_at_slot(vars_outer[*idx].slot)
                    .expect("value must be bound")
                    .node
            })
            .collect();
        let value = arena.value.map_columns(
            &arena.mixop,
            shape,
            exp_inner.note.clone(),
            exp.note.clone(),
            &columns,
        );
        ok!(unwrap_from_result!(value, &exp.span))
    })())
}
