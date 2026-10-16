//! Projection of destructured list rows into binding columns
//!
//! Fixed tuple, case, struct, option, and list patterns only select children.
//! Resolve each output slot's final selection once, then collect without
//! constructing a context for every row. Patterns that construct tails or
//! nested iterations retain the general assignment path.

use crate::interp::shared::{context::ReadContext, util::find_slot_of_exp};
use crate::lang::data::{
    arena::Arena,
    value::{
        ValueError,
        flat::{Value, projection::Step},
    },
};
use crate::lang::il::prepared as ast;

/// Records the last projection assigned to each output variable.
fn paths(
    ctx: &impl ReadContext,
    exp: &ast::Exp,
    vars: &[ast::Var],
    path: &mut Vec<Step>,
    paths: &mut [Option<Vec<Step>>],
) -> Option<()> {
    // Identity iterations already denote one prepared slot
    if let Some(slot) = find_slot_of_exp(ctx, exp) {
        if let Some(idx) = vars.iter().position(|var| var.slot == slot) {
            paths[idx] = Some(path.clone());
        }
        return Some(());
    }
    // Other fixed patterns select children without allocating values
    match &exp.node {
        ast::ExpKind::Tuple(exps) | ast::ExpKind::List(exps) => {
            for (idx, exp_inner) in exps.iter().enumerate() {
                path.push(if matches!(exp.node, ast::ExpKind::Tuple(_)) {
                    Step::Tuple(idx)
                } else {
                    Step::List(idx)
                });
                paths_inner(ctx, exp_inner, vars, path, paths)?;
            }
        }
        ast::ExpKind::Case(not_exp) => {
            for (idx, exp_inner) in not_exp.args().iter().enumerate() {
                path.push(Step::Case(idx));
                paths_inner(ctx, exp_inner, vars, path, paths)?;
            }
        }
        ast::ExpKind::Str(fields) => {
            for (idx, field) in fields.iter().enumerate() {
                path.push(Step::Struct(idx));
                paths_inner(ctx, &field.exp, vars, path, paths)?;
            }
        }
        ast::ExpKind::Opt(Some(exp_inner)) => {
            path.push(Step::Opt);
            paths_inner(ctx, exp_inner, vars, path, paths)?;
        }
        ast::ExpKind::Opt(None) => {}
        _ => return None,
    }
    Some(())
}

fn paths_inner(
    ctx: &impl ReadContext,
    exp: &ast::Exp,
    vars: &[ast::Var],
    path: &mut Vec<Step>,
    paths_out: &mut [Option<Vec<Step>>],
) -> Option<()> {
    paths(ctx, exp, vars, path, paths_out)?;
    path.pop();
    Some(())
}

/// Collects fixed-pattern bindings, or requests general row assignment.
pub(super) fn collect(
    arena: &mut Arena,
    ctx: &impl ReadContext,
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
                let Some(slot) = find_slot_of_exp(ctx, exp) else {
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
    paths(ctx, exp, vars, &mut Vec::new(), &mut paths_out)?;
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
    use super::iter::ListInputs;
    use crate::interp::shared::backtrack::{ok, unwrap_from_result};
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
            let slot = find_slot_of_exp(ctx, exp)?;
            exp_iter.vars.iter().position(|var| var.slot == slot)
        })
        .collect::<Option<_>>()?;
    // Every iteration input must be accounted for, including length checks
    if (0..exp_iter.vars.len()).any(|idx| !indices.contains(&idx)) {
        return None;
    }
    Some((|| {
        let inputs = unwrap_from_result!(ListInputs::new(arena, ctx, &exp_iter.vars), &exp.span);
        let columns: Vec<_> = indices.iter().map(|index| inputs.body(*index)).collect();
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
