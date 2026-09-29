//! AL invocation and ordered candidate selection
//!
//! `invoke_rel` and `invoke_func` look the callee up,
//! serve it from the cache when eligible, and dispatch on its kind:
//! extern and builtin calls go to the host,
//! table rows and clauses are tried as candidates,
//! rule paths of a defined relation likewise, then the otherwise group.
//! With `guard` on, inputs and outputs are type-checked at the boundary.

use super::super::backtrack::{choose_deterministic, choose_sequential};
use crate::diagnostic::Diagnostic;
use crate::interp::shared::context::ReadContext;
use crate::interp::shared::error;
use crate::interp::shared::eval::assign::assign_tparams;
use crate::lang::hints::input;
use crate::runtime::envs::interp::al::ast_prepared as ast;
use crate::runtime::envs::interp::shared::frame::FrameLayout;
use crate::runtime::ops::{typ as typ_ops, value as value_ops};
use std::rc::Rc;

use super::super::{
    AlInterp,
    context::{Context, Scope},
};
use super::{assign, expr, prem::eval_prems};
use crate::interp::shared::{
    backtrack::{self, Backtrack, WithFrame, fatal, ok, unmatch, unwrap, unwrap_from_result},
    cache::CallKey,
};
use crate::lang::data::value::{ValueArena, ValueKind};
use crate::{
    lang::data::value::Value,
    runner::{Extern, Interface, RunnerContext},
};

// = Input and output checks

/// Checks the input count and, with `guard`, the input types.
pub(in crate::interp::al) fn check_rel_inputs(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    values: &[Value],
    guard: bool,
) -> Backtrack<()> {
    let rel = unwrap_from_result!(ctx.find_rel(id), &id.span);
    // Extern and defined relations share the signature shape
    let (not_typ, inputs) = match &rel.def {
        ast::RelDef::Extern(rel) => (&rel.not_typ, &rel.input_hint),
        ast::RelDef::Defined(rel) => (&rel.not_typ, &rel.input_hint),
    };
    let typs = not_typ.node.args();
    // The hint must fit the notation arity
    unwrap_from_result!(input::validate(inputs, typs.len()), &id.span);
    // Always check the input count, even without type guards
    unwrap!(backtrack::check(inputs.indices().len() == values.len(), id.span.clone(), || {
        error::guard::relation_input_arity_mismatch(inputs.indices().len(), values.len())
    }));
    if !guard {
        return ok!(());
    }
    // Input types are the notation arguments the hint selects
    let typs = inputs
        .indices()
        .iter()
        .map(|idx| typs[idx.node].clone())
        .collect::<Vec<_>>();
    check_values(arena, ctx, id, &typs, values, || {
        error::guard::relation_input_type_mismatch(id.node.clone())
    })
}

/// Checks argument counts and, with `guard`, the argument types.
pub(in crate::interp::al) fn check_func_inputs(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    targs: &[ast::Typ],
    values: &[Value],
    guard: bool,
) -> Backtrack<()> {
    let typ = unwrap_from_result!(ctx.find_func_typ(id), &id.span);
    // Check type and value argument counts before binding them
    unwrap!(backtrack::check(typ.tparams.len() == targs.len(), id.span.clone(), || {
        error::call::type_argument_arity_mismatch(typ.tparams.len(), targs.len())
    }));
    unwrap!(backtrack::check(typ.typs_params.len() == values.len(), id.span.clone(), || {
        error::guard::function_input_arity_mismatch(typ.typs_params.len(), values.len())
    }));
    if !guard {
        return ok!(());
    }
    // Bind type arguments before checking parameter types
    let ctx_local = unwrap!(assign_tparams(ctx.localize(), &typ.tparams, targs, &id.span));
    // Parameter types resolve against the bound type parameters
    check_values(arena, &ctx_local, id, &typ.typs_params, values, || {
        error::guard::function_input_type_mismatch(id.node.clone())
    })
}

/// Checks each value against its type, failing with `error`.
fn check_values(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    typs: &[ast::Typ],
    values: &[Value],
    diagnostic: impl FnOnce() -> Diagnostic,
) -> Backtrack<()> {
    // Subtyping resolves type names and function types through the context
    let find_typdef_opt = |id: &ast::Id| ctx.find_typdef_opt(id);
    let find_func = |name: &str| {
        let id = crate::phrase!(node: name.to_owned(), span: id.span.clone());
        ctx.find_func_typ(&id).ok()
    };
    // Check all values against their types at once
    let matches = unwrap_from_result!(
        value_ops::subs(arena, &find_typdef_opt, &find_func, typs, values),
        &id.span
    );
    backtrack::check(matches, id.span.clone(), diagnostic)
}

/// Type-checks a function result with the type parameters substituted.
fn check_func_output(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    tparams: &[ast::TParam],
    typ: &ast::Typ,
    targs: &[ast::Typ],
    value: &Value,
) -> Backtrack<()> {
    // Substitute the type arguments into the declared result type
    let theta = unwrap_from_result!(typ_ops::Theta::from_lists(tparams, targs), &id.span);
    let typ = unwrap_from_result!(typ_ops::subst_typ(&|id| theta.get(id), typ), &id.span);
    // Check the single result
    check_values(arena, ctx, id, &[typ], std::slice::from_ref(value), || {
        error::guard::function_output_type_mismatch(id.node.clone())
    })
}

// = Cache eligibility

/// Whether a relation call may be memoized: caching on, relation defined.
pub(in crate::interp::al) fn cache_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
) -> bool {
    runner_ctx.interp().config.cache
        && matches!(ctx.find_rel(id), Ok(rel) if matches!(&rel.def, ast::RelDef::Defined(_)))
}

/// Whether a function call may be memoized.
///
/// Requires caching on, a global non-extern function,
/// and no function-valued argument.
pub(in crate::interp::al) fn cache_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    values: &[Value],
) -> bool {
    runner_ctx.interp().config.cache
        && matches!(ctx.find_func_with_scope(id), Ok((Scope::Global, func))
            if !matches!(&func.def, ast::MetaFuncDef::Extern(_)))
        && !values
            .iter()
            .any(|value| matches!(runner_ctx.arena().kind(value), ValueKind::Func(_)))
}

// = Relation invocation

/// Invokes a relation, memoizing pure results of defined relations.
pub fn invoke_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    values: &[Value],
) -> Backtrack<Vec<Value>> {
    // Serve from the cache when eligible
    let cache = cache_rel(runner_ctx, ctx, id);
    if cache
        && let Some(values) =
            runner_ctx
                .interp()
                .cache
                .find_rel(runner_ctx.arena(), &id.node, values)
    {
        return ok!(values.clone());
    }
    let key = cache.then(|| CallKey::new(runner_ctx.arena(), &id.node, values));
    // Track effects for memoization; grow the stack for deep recursion
    runner_ctx.interp_mut().cache.begin();
    let result = stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
        let rel = unwrap_from_result!(ctx.find_rel(id), &id.span);
        let layout = &rel.layout;
        match &rel.def {
            ast::RelDef::Extern(rel) => invoke_extern_rel(runner_ctx, ctx, id, rel, values),
            ast::RelDef::Defined(rel) => {
                invoke_defined_rel(runner_ctx, ctx, layout, id, rel, values)
            }
        }
    });
    // Memoize a pure result
    let pure = runner_ctx.interp_mut().cache.end();
    if pure && let (Some(key), ok!(values)) = (key, &result) {
        runner_ctx
            .interp_mut()
            .cache
            .rels
            .insert(key, values.clone());
    }
    // Nest failures under the invocation trace
    result.with_frame(id.span.clone(), || format!("while invoking {}", id.node))
}

// - Extern relation

/// Calls a host relation, recording its effect and guarding its outputs.
fn invoke_extern_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    rel: &ast::ExternRel,
    values: &[Value],
) -> Backtrack<Vec<Value>> {
    // The host reports whether the call had a side effect
    let result = runner_ctx.call_extern_rel(&id.node, values);
    runner_ctx
        .interp_mut()
        .cache
        .mark_effect(result.as_ref().map_or(true, |(_, effect)| *effect));
    // Return extern failures without turning mismatches into fatal errors
    let (values, _) = unwrap!(result);
    // Check the number of extern outputs before assigning them
    let len = rel.not_typ.node.args().len() - rel.input_hint.indices().len();
    unwrap!(backtrack::check(len == values.len(), id.span.clone(), || {
        error::guard::relation_output_arity_mismatch(len, values.len())
    }));
    if runner_ctx.interp().config.guard {
        // Output types are the notation arguments the hint leaves
        let typs = rel.not_typ.node.args().into_iter().cloned().collect();
        let (_, typs) = unwrap_from_result!(input::split(&rel.input_hint, typs), &id.span);
        unwrap!(check_values(runner_ctx.arena(), ctx, id, &typs, &values, || {
            error::guard::relation_output_type_mismatch(id.node.clone())
        }));
    }
    ok!(values)
}

// - Defined relation

/// Runs one rule path: bind inputs, check premises, evaluate outputs.
fn eval_rule_path<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    rule_match: &ast::RuleMatch,
    path: &ast::RulePath,
    values: &[Value],
) -> Backtrack<Vec<Value>> {
    // Input count must match the rule's input patterns
    assert_eq!(rule_match.exps_input.len(), values.len(), "validated rule input arity");
    // Inputs bind into a fresh frame for the rule
    let ctx = unwrap!(assign::assign_exps(
        runner_ctx.arena_mut(),
        ctx.localize_with_layout(layout),
        &rule_match.exps_input,
        values
    ));
    // Shared premises of the group, then the path's own
    let ctx = unwrap!(eval_prems(runner_ctx, ctx, &rule_match.prems));
    let ctx = unwrap!(eval_prems(runner_ctx, ctx, &path.prems));
    // Outputs evaluate in the extended context
    expr::eval_exps(runner_ctx, &ctx, &path.exps_output)
}

/// Tries the rule paths in order or all at once, then the otherwise group.
fn invoke_defined_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    id: &ast::Id,
    rel: &ast::DefinedRel,
    values: &[Value],
) -> Backtrack<Vec<Value>> {
    let det = runner_ctx.interp().config.det;
    // Flatten the groups into (group, path) candidates
    let paths: Vec<_> = rel
        .rule_groups
        .iter()
        .flat_map(|group| {
            group
                .node
                .rule_paths
                .iter()
                .map(move |path| (&group.node, path))
        })
        .collect();
    // Evaluate each candidate without adding a rule-path frame
    let mut evaluate = |&(group, path): &(&ast::RuleGroupKind, &ast::RulePath)| {
        eval_rule_path(runner_ctx, ctx, layout, &group.rule_match, path, values)
    };
    // Deterministic mode rejects two matching paths
    let result = if det {
        choose_deterministic(paths, &mut evaluate, |(group_a, path_a), (group_b, path_b)| {
            error::at(
                error::call::relation_nondeterministic(
                    id.node.clone(),
                    group_a.id.node.clone(),
                    path_a.id.node.clone(),
                    group_b.id.node.clone(),
                    path_b.id.node.clone(),
                ),
                id.span.clone(),
            )
        })
    // Sequential mode takes the first matching path
    } else {
        choose_sequential(paths, &mut evaluate)
    };
    match result {
        // A match is the answer
        ok!(values) => ok!(values),
        // A fatal error aborts
        fatal!(errors) => fatal!(errors),
        // No path matched: the otherwise group is the fallback
        unmatch!(errors) => match &rel.else_group {
            // The otherwise group runs like any path
            Some(group) => eval_rule_path(
                runner_ctx,
                ctx,
                layout,
                &group.node.rule_match,
                &group.node.rule_path,
                values,
            ),
            // No fallback: propagate the selected mismatches
            None => unmatch!(errors),
        },
    }
}

// = Function invocation

/// Invokes a function, memoizing pure results of eligible calls.
pub fn invoke_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    targs: &[ast::Typ],
    values: &[Value],
) -> Backtrack<Value> {
    // Serve from the cache when eligible
    let cache = cache_func(runner_ctx, ctx, id, values);
    if cache
        && let Some(value) =
            runner_ctx
                .interp()
                .cache
                .find_func(runner_ctx.arena(), &id.node, values)
    {
        return ok!(*value);
    }
    let key = cache.then(|| CallKey::new(runner_ctx.arena(), &id.node, values));
    // Track effects for memoization; grow the stack for deep recursion
    runner_ctx.interp_mut().cache.begin();
    let result = stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
        let func = unwrap_from_result!(ctx.find_func(id), &id.span);
        let layout = &func.layout;
        match &func.def {
            ast::MetaFuncDef::Extern(func) => {
                invoke_extern_func(runner_ctx, ctx, id, func, targs, values)
            }
            ast::MetaFuncDef::Builtin(func) => {
                invoke_builtin_func(runner_ctx, ctx, id, func, targs, values)
            }
            ast::MetaFuncDef::Table(func) => {
                invoke_table_func(runner_ctx, ctx, layout, func, values)
            }
            ast::MetaFuncDef::Defined(func) => {
                invoke_defined_func(runner_ctx, ctx, layout, func, targs, values)
            }
        }
    });
    // Memoize a pure result
    let pure = runner_ctx.interp_mut().cache.end();
    if pure && let (Some(key), ok!(value)) = (key, &result) {
        runner_ctx.interp_mut().cache.funcs.insert(key, *value);
    }
    // Nest failures under the invocation trace
    result.with_frame(id.span.clone(), || error::trace::function(id, targs))
}

// - Extern function

/// Calls a host function, recording its effect and guarding its result.
fn invoke_extern_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    extern_func: &ast::ExternFunc,
    targs: &[ast::Typ],
    values: &[Value],
) -> Backtrack<Value> {
    // The host reports whether the call had a side effect
    let result = runner_ctx.call_extern_func(&id.node, &[], values);
    runner_ctx
        .interp_mut()
        .cache
        .mark_effect(result.as_ref().map_or(true, |(_, effect)| *effect));
    // Return extern failures without turning mismatches into fatal errors
    let (value, _) = unwrap!(result);
    // Guard the result against the declared type
    if runner_ctx.interp().config.guard {
        unwrap!(check_func_output(
            runner_ctx.arena(),
            ctx,
            id,
            &extern_func.tparams,
            &extern_func.typ,
            targs,
            &value
        ));
    }
    ok!(value)
}

// - Builtin function

/// Calls a builtin; its own failure is a mismatch, other errors are fatal.
fn invoke_builtin_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    builtin_func: &ast::BuiltinFunc,
    targs: &[ast::Typ],
    values: &[Value],
) -> Backtrack<Value> {
    // Builtins report effects like host calls
    let result = runner_ctx.call_builtin(id, targs, values);
    runner_ctx
        .interp_mut()
        .cache
        .mark_effect(result.as_ref().map_or(true, |(_, effect)| *effect));
    match result {
        // Guard the result against the declared type
        Ok((value, _)) => {
            if runner_ctx.interp().config.guard {
                unwrap!(check_func_output(
                    runner_ctx.arena(),
                    ctx,
                    id,
                    &builtin_func.tparams,
                    &builtin_func.typ,
                    targs,
                    &value
                ));
            }
            ok!(value)
        }
        // Builtin failures let the caller try another candidate
        Err(failure) => Err(failure.at_if_missing(&id.span)),
    }
}

// - Table function

/// Matches one table row: bind arguments, check premises, evaluate the body.
fn eval_table_row<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    table_row: &ast::TableRow,
    values: &[Value],
) -> Backtrack<Value> {
    // Argument count must match the row
    assert_eq!(table_row.node.args.len(), values.len(), "validated table row argument arity");
    // Arguments bind into a fresh frame for the row
    let ctx = unwrap!(assign::assign_args(
        runner_ctx.arena_mut(),
        ctx,
        ctx.localize_with_layout(layout),
        &table_row.node.args,
        values
    ));
    // Premises, then the row body
    let ctx = unwrap!(eval_prems(runner_ctx, ctx, &table_row.node.prems));
    expr::eval_exp(runner_ctx, &ctx, &table_row.node.exp)
}

/// Tries the table rows in order; the first matching row decides.
fn invoke_table_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    table_func: &ast::TableFunc,
    values: &[Value],
) -> Backtrack<Value> {
    choose_sequential(&table_func.table_rows, |table_row| {
        eval_table_row(runner_ctx, ctx, layout, table_row, values)
    })
}

// - Defined function

/// Runs one clause: bind arguments, check premises, evaluate the body.
fn eval_clause<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx_caller: &Context<'_>,
    ctx_callee: &Context<'_>,
    clause: &ast::Clause,
    values: &[Value],
) -> Backtrack<Value> {
    // Argument count must match the clause
    assert_eq!(clause.node.args.len(), values.len(), "validated clause argument arity");
    // Arguments bind into the callee scope, evaluated in the caller's
    let ctx = unwrap!(assign::assign_args(
        runner_ctx.arena_mut(),
        ctx_caller,
        ctx_callee.clone(),
        &clause.node.args,
        values
    ));
    // Premises, then the clause body
    let ctx = unwrap!(eval_prems(runner_ctx, ctx, &clause.node.prems));
    expr::eval_exp(runner_ctx, &ctx, &clause.node.exp)
}

/// Binds the type parameters, tries the clauses, then the otherwise clause.
fn invoke_defined_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, AlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    defined_func: &ast::DefinedFunc,
    targs: &[ast::Typ],
    values: &[Value],
) -> Backtrack<Value> {
    // Bind type arguments in the callee frame
    let ctx_local = unwrap!(assign_tparams(
        ctx.localize_with_layout(layout),
        &defined_func.tparams,
        targs,
        &defined_func.id.span
    ));
    let det = runner_ctx.interp().config.det;
    // Clauses are candidates by index
    let mut evaluate =
        |idx: &usize| eval_clause(runner_ctx, ctx, &ctx_local, &defined_func.clauses[*idx], values);
    // Deterministic mode rejects two matching clauses
    let result = if det {
        choose_deterministic(0..defined_func.clauses.len(), &mut evaluate, |idx_a, idx_b| {
            error::at(
                error::call::function_nondeterministic(defined_func.id.node.clone(), idx_a, idx_b),
                defined_func.id.span.clone(),
            )
        })
    // Sequential mode takes the first matching clause
    } else {
        choose_sequential(0..defined_func.clauses.len(), &mut evaluate)
    };
    match result {
        // A match is the answer
        ok!(value) => ok!(value),
        // A fatal error aborts
        fatal!(errors) => fatal!(errors),
        // No clause matched: the otherwise clause is the fallback
        unmatch!(errors) => match &defined_func.else_clause {
            // The otherwise clause runs like any clause
            Some(clause) => eval_clause(runner_ctx, ctx, &ctx_local, clause, values),
            // No fallback: propagate the selected mismatches
            None => unmatch!(errors),
        },
    }
}
