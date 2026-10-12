//! Shared path access and update evaluation
//!
//! A path such as `.f[i]` is read innermost-first from the base value
//! and written back outermost-first:
//! `x.f[i] := v` reads `x.f`, replaces element `i`,
//! then replaces field `f` in `x`.

use crate::lang::{
    common::source::Span,
    data::value::flat::{self as value, Value},
};

use crate::lang::il::prepared as ast;

use crate::runner::{Extern, Interface, RunnerContext};

use crate::interp::shared::backtrack::{Backtrack, ok, unwrap, unwrap_from_result};

use super::{Invoker, expr::eval_exp, ops};

// - Access

/// Reads the value at `path` inside `value_base`.
fn eval_access_path<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    value_base: &Value,
    path: &ast::Path,
) -> Backtrack<Value> {
    match &path.node {
        // The root is the base value itself
        ast::PathKind::Root => ok!(*value_base),
        ast::PathKind::Idx(path, exp_idx) => {
            eval_access_idx_path(runner_ctx, ctx, value_base, path, exp_idx)
        }
        ast::PathKind::Slice(path, exp_idx, exp_len) => {
            eval_access_slice_path(runner_ctx, ctx, value_base, path, exp_idx, exp_len)
        }
        ast::PathKind::Dot(path, atom) => {
            eval_access_dot_path(runner_ctx, ctx, value_base, path, atom)
        }
    }
}

// - Index access path

/// Reads the prefix, then indexes it.
fn eval_access_idx_path<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    value_base: &Value,
    path: &ast::Path,
    exp_idx: &ast::Exp,
) -> Backtrack<Value> {
    // Read the prefix, then index it
    let value = unwrap!(eval_access_path(runner_ctx, ctx, value_base, path));
    let value_idx = unwrap!(eval_exp(runner_ctx, ctx, exp_idx));
    ops::access_index(runner_ctx.arena_mut(), &value, &value_idx, &path.span, &exp_idx.span)
}

// - Slice access path

/// Reads the prefix, then slices it.
fn eval_access_slice_path<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    value_base: &Value,
    path: &ast::Path,
    exp_idx: &ast::Exp,
    exp_len: &ast::Exp,
) -> Backtrack<Value> {
    // Read the prefix, then slice it with the evaluated bounds
    let typ = &path.note;
    let value = unwrap!(eval_access_path(runner_ctx, ctx, value_base, path));
    let value_idx = unwrap!(eval_exp(runner_ctx, ctx, exp_idx));
    let value_len = unwrap!(eval_exp(runner_ctx, ctx, exp_len));
    ops::access_slice(
        runner_ctx.arena_mut(),
        &value,
        &value_idx,
        &value_len,
        typ,
        &path.span,
        &path.span,
        &exp_idx.span,
        &exp_len.span,
        &exp_len.span,
    )
}

// - Field access path

/// Reads the prefix, then reads its field.
fn eval_access_dot_path<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    value_base: &Value,
    path: &ast::Path,
    atom: &ast::Atom,
) -> Backtrack<Value> {
    let value = unwrap!(eval_access_path(runner_ctx, ctx, value_base, path));
    ops::access_dot(runner_ctx.arena(), &value, atom, &path.span)
}

// - Update

/// Writes `value_upd` at `path`, rebuilding the values that enclose it.
pub(crate) fn eval_update_path<
    'global,
    Interp: Invoker<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    value_base: &Value,
    path: &ast::Path,
    value_upd: Value,
) -> Backtrack<Value> {
    match &path.node {
        // At the root the update replaces the whole value
        ast::PathKind::Root => ok!(value_upd),
        ast::PathKind::Idx(path, exp_idx) => {
            eval_update_idx_path(runner_ctx, ctx, value_base, path, exp_idx, value_upd)
        }
        ast::PathKind::Slice(path, exp_idx, exp_len) => {
            eval_update_slice_path(runner_ctx, ctx, value_base, path, exp_idx, exp_len, value_upd)
        }
        ast::PathKind::Dot(path, atom) => {
            eval_update_dot_path(runner_ctx, ctx, value_base, path, atom, value_upd)
        }
    }
}

// - Index update path

/// Reads the prefix, replaces the element, and writes the prefix back.
fn eval_update_idx_path<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    value_base: &Value,
    path: &ast::Path,
    exp_idx: &ast::Exp,
    value_upd: Value,
) -> Backtrack<Value> {
    let typ = crate::phrase!(node: path.note.clone(), span: path.span.clone());
    let value = unwrap!(eval_access_path(runner_ctx, ctx, value_base, path));
    let value_idx = unwrap!(eval_exp(runner_ctx, ctx, exp_idx));
    // Replace the element, then write the prefix back
    let value = unwrap!(ops::update_index(
        runner_ctx.arena_mut(),
        &value,
        &value_idx,
        value_upd,
        &typ,
        &path.span,
        &exp_idx.span
    ));
    eval_update_path(runner_ctx, ctx, value_base, path, value)
}

// - Slice update path

/// Reads the prefix, replaces the range, and writes the prefix back.
fn eval_update_slice_path<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    value_base: &Value,
    path: &ast::Path,
    exp_idx: &ast::Exp,
    exp_len: &ast::Exp,
    value_upd: Value,
) -> Backtrack<Value> {
    let typ = crate::phrase!(node: path.note.clone(), span: path.span.clone());
    let value = unwrap!(eval_access_path(runner_ctx, ctx, value_base, path));
    let value_idx = unwrap!(eval_exp(runner_ctx, ctx, exp_idx));
    let value_len = unwrap!(eval_exp(runner_ctx, ctx, exp_len));
    // Replace the range, then write the prefix back
    let value = unwrap!(ops::update_slice(
        runner_ctx.arena_mut(),
        &value,
        &value_idx,
        &value_len,
        value_upd,
        &typ,
        &path.span,
        &exp_idx.span,
        &exp_len.span
    ));
    eval_update_path(runner_ctx, ctx, value_base, path, value)
}

// - Field update path

/// Reads the prefix, replaces the field, and writes the prefix back.
fn eval_update_dot_path<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    value_base: &Value,
    path: &ast::Path,
    atom: &ast::Atom,
    value_upd: Value,
) -> Backtrack<Value> {
    let typ = crate::phrase!(node: path.note.clone(), span: path.span.clone());
    let value = unwrap!(eval_access_path(runner_ctx, ctx, value_base, path));
    let value_fields = value::get::structure(runner_ctx.arena(), &value)
        .expect("field update base must be a struct");
    // Replace the named field, keep the others
    let value_fields = value_fields
        .iter()
        .map(|value_field| ast::ValueField {
            atom: value_field.atom.clone(),
            value: if value_field.atom.node == atom.node { value_upd } else { value_field.value },
        })
        .collect();
    let value = unwrap_from_result!(
        value::make::structure(
            runner_ctx.arena_mut(),
            typ.node.clone(),
            value_fields,
            Span::default()
        ),
        &Span::default()
    );
    eval_update_path(runner_ctx, ctx, value_base, path, value)
}
