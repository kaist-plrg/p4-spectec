//! SL invocation, memoization and tail-call dispatch
//!
//! `invoke_rel` and `invoke_func` look the callee up,
//! serve it from the cache when eligible, and dispatch on its kind.
//! A body that ends in a tail call yields `TailCall`;
//! the invoker then swaps in the new callee and loops instead of recursing,
//! remembering the caller so a failure still shows the whole call chain.
//! With `guard` on, inputs and outputs are type-checked at the boundary.

use super::super::{
    SlInterp,
    context::{Context, Scope},
    flow::Flow,
};
use super::{assign, instr};
use crate::diagnostic::Diagnostic;
use crate::interp::shared::context::ReadContext;
use crate::interp::shared::error;
use crate::interp::shared::eval::assign::assign_tparams;
use crate::lang::hints::input;
use crate::runtime::envs::interp::shared::frame::FrameLayout;
use crate::runtime::envs::interp::sl::ast_prepared as ast;
use crate::runtime::ops::{typ as typ_ops, value as value_ops};
use crate::{
    interp::shared::{
        backtrack::{self, Backtrack, WithFrame, fatal, ok, unmatch, unwrap, unwrap_from_result},
        cache::CallKey,
    },
    lang::data::value::{Value, ValueArena, ValueKind},
    runner::{Extern, Interface, RunnerContext},
};
use std::{borrow::Cow, rc::Rc};

// = Invocation results

/// What a function body produced: a value, or a tail call to make.
enum FuncResult {
    Return(Value),
    TailCall(ast::Id, Vec<ast::Typ>, Vec<Value>),
}

/// What a relation body produced: outputs, or a tail call to make.
enum RelResult {
    Result(Vec<Value>),
    TailCall(ast::Id, Vec<Value>),
}

// = Input and output checks

/// Checks the input count and, with `guard`, the input types.
pub(in crate::interp::sl) fn check_rel_inputs(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    values: &[Value],
    guard: bool,
) -> Backtrack<()> {
    let rel = unwrap_from_result!(ctx.find_rel(id), &id.span);
    // Extern and defined relations share the signature shape
    let (not_typ, inputs) = match &rel.def {
        ast::RelDef::Extern(rel) => (&rel.rel_signature.not_typ, &rel.rel_signature.input_hint),
        ast::RelDef::Defined(rel) => (&rel.rel_signature.not_typ, &rel.rel_signature.input_hint),
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
pub(in crate::interp::sl) fn check_func_inputs(
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
pub(in crate::interp::sl) fn cache_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &RunnerContext<'_, SlInterp, Iface, Ext>,
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
pub(in crate::interp::sl) fn cache_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &RunnerContext<'_, SlInterp, Iface, Ext>,
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

/// Invokes a relation, looping through tail calls and memoizing pure results.
pub fn invoke_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    values: &[Value],
) -> Backtrack<Vec<Value>> {
    let mut id = Cow::Borrowed(id);
    let mut values = Cow::Borrowed(values);
    let mut ids_pending: Vec<ast::Id> = Vec::new();
    loop {
        // Serve from the cache when eligible
        let cache = cache_rel(runner_ctx, ctx, &id);
        if cache
            && let Some(values) =
                runner_ctx
                    .interp()
                    .cache
                    .find_rel(runner_ctx.arena(), &id.node, &values)
        {
            return ok!(values.clone());
        }
        let key = cache.then(|| CallKey::new(runner_ctx.arena(), &id.node, &values));
        // Track effects for memoization; grow the stack for deep recursion
        runner_ctx.interp_mut().cache.begin();
        let result = stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
            let rel = unwrap_from_result!(ctx.find_rel(&id), &id.span);
            let layout = &rel.layout;
            match &rel.def {
                ast::RelDef::Extern(rel) => {
                    let values = unwrap!(invoke_extern_rel(runner_ctx, ctx, &id, rel, &values));
                    ok!(RelResult::Result(values))
                }
                ast::RelDef::Defined(rel) => {
                    invoke_defined_rel(runner_ctx, ctx, layout, rel, &values)
                }
            }
        });
        // Nest failures under this call and then under the tail-calling callers
        let pure = runner_ctx.interp_mut().cache.end();
        let mut result =
            result.with_frame(id.span.clone(), || error::trace::message_rel_invocation(&id));
        if result.is_err() {
            for id in ids_pending.iter().rev() {
                result =
                    result.with_frame(id.span.clone(), || error::trace::message_rel_invocation(id));
            }
        }
        // Fatal errors and mismatches leave the loop here
        let result = unwrap!(result);
        match result {
            // Memoize a pure result
            RelResult::Result(values) => {
                if pure && let Some(key) = key {
                    runner_ctx
                        .interp_mut()
                        .cache
                        .rels
                        .insert(key, values.clone());
                }
                return ok!(values);
            }
            // Tail call: remember this callee for the trace and loop
            RelResult::TailCall(id_tail, values_tail) => {
                ids_pending.push(id.into_owned());
                id = Cow::Owned(id_tail);
                values = Cow::Owned(values_tail);
            }
        }
    }
}

// - Extern relation

/// Calls a host relation, recording its effect and guarding its outputs.
fn invoke_extern_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
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
    let len =
        rel.rel_signature.not_typ.node.args().len() - rel.rel_signature.input_hint.indices().len();
    unwrap!(backtrack::check(len == values.len(), id.span.clone(), || {
        error::guard::relation_output_arity_mismatch(len, values.len())
    }));
    if runner_ctx.interp().config.guard {
        // Output types are the notation arguments the hint leaves
        let typs = rel
            .rel_signature
            .not_typ
            .node
            .args()
            .into_iter()
            .cloned()
            .collect();
        let (_, typs) =
            unwrap_from_result!(input::split(&rel.rel_signature.input_hint, typs), &id.span);
        unwrap!(check_values(runner_ctx.arena(), ctx, id, &typs, &values, || {
            error::guard::relation_output_type_mismatch(id.node.clone())
        }));
    }
    ok!(values)
}

// - Defined relation

/// Binds the inputs and runs the body with its otherwise block.
fn invoke_defined_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    rel: &ast::DefinedRel,
    values: &[Value],
) -> Backtrack<RelResult> {
    // Inputs bind into a fresh frame
    let ctx = unwrap!(assign::assign_exps(
        runner_ctx.arena_mut(),
        ctx.localize_with_layout(layout),
        &rel.exps_input,
        values
    ));
    // Run the body, falling back to the otherwise block
    let flow = unwrap!(instr::eval_block_with_else(
        runner_ctx,
        ctx,
        &rel.block,
        rel.block_else.as_deref()
    ));
    match flow {
        // Results finish the relation
        Flow::Result(values) => ok!(RelResult::Result(values)),
        // Tail calls go back to the invoker loop
        Flow::TailRel(id, values) => ok!(RelResult::TailCall(id, values)),
        // Falling through the whole body is a mismatch
        Flow::Cont(errors) => unmatch!(errors),
        // Function flows cannot appear in a relation
        Flow::Return(_) | Flow::TailFunc(..) => unreachable!("function flow in relation body"),
    }
}

// = Function invocation

/// Invokes a function, looping through tail calls and memoizing pure results.
pub fn invoke_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    targs: &[ast::Typ],
    values: &[Value],
) -> Backtrack<Value> {
    let mut id = Cow::Borrowed(id);
    let mut targs = Cow::Borrowed(targs);
    let mut values = Cow::Borrowed(values);
    let mut calls_pending: Vec<(ast::Id, Vec<ast::Typ>)> = Vec::new();
    loop {
        // Serve from the cache when eligible
        let cache = cache_func(runner_ctx, ctx, &id, &values);
        if cache
            && let Some(value) =
                runner_ctx
                    .interp()
                    .cache
                    .find_func(runner_ctx.arena(), &id.node, &values)
        {
            return ok!(*value);
        }
        let key = cache.then(|| CallKey::new(runner_ctx.arena(), &id.node, &values));
        // Track effects for memoization; grow the stack for deep recursion
        runner_ctx.interp_mut().cache.begin();
        let result = stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
            let func = unwrap_from_result!(ctx.find_func(&id), &id.span);
            let layout = &func.layout;
            match &func.def {
                ast::MetaFuncDef::Extern(func) => ok!(FuncResult::Return(unwrap!(
                    invoke_extern_func(runner_ctx, ctx, &id, func, &targs, &values)
                ))),
                ast::MetaFuncDef::Builtin(func) => ok!(FuncResult::Return(unwrap!(
                    invoke_builtin_func(runner_ctx, ctx, &id, func, &targs, &values)
                ))),
                ast::MetaFuncDef::Table(func) => {
                    invoke_table_func(runner_ctx, ctx, layout, &id, func, &values)
                }
                ast::MetaFuncDef::Defined(func) => {
                    invoke_defined_func(runner_ctx, ctx, layout, &id, func, &targs, &values)
                }
            }
        });
        // Nest failures under this call and then under the tail-calling callers
        let pure = runner_ctx.interp_mut().cache.end();
        let mut result = result
            .with_frame(id.span.clone(), || error::trace::message_func_invocation(&id, &targs));
        if result.is_err() {
            for (id, targs) in calls_pending.iter().rev() {
                result = result.with_frame(id.span.clone(), || {
                    error::trace::message_func_invocation(id, targs)
                });
            }
        }
        // Fatal errors and mismatches leave the loop here
        let result = unwrap!(result);
        match result {
            // Memoize a pure result
            FuncResult::Return(value) => {
                if pure && let Some(key) = key {
                    runner_ctx.interp_mut().cache.funcs.insert(key, value);
                }
                return ok!(value);
            }
            // Tail call: remember this callee for the trace and loop
            FuncResult::TailCall(id_tail, targs_tail, values_tail) => {
                calls_pending.push((id.into_owned(), targs.into_owned()));
                id = Cow::Owned(id_tail);
                targs = Cow::Owned(targs_tail);
                values = Cow::Owned(values_tail);
            }
        }
    }
}

// - Extern function

/// Calls a host function, recording its effect and guarding its result.
fn invoke_extern_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
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
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
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
        Err(failure) => Err(failure.with_span(&id.span)),
    }
}

// - Table function

/// Runs all row blocks as one sequence; the first block that returns decides.
fn invoke_table_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    id: &ast::Id,
    func: &ast::TableFunc,
    values: &[Value],
) -> Backtrack<FuncResult> {
    // Parameters bind into a fresh frame
    let ctx_local = unwrap!(assign::assign_params(
        runner_ctx.arena_mut(),
        ctx,
        ctx.localize_with_layout(layout),
        &func.params,
        values
    ));
    // Rows are concatenated and run in tail position
    let instrs = func.table_rows.iter().flat_map(|row| row.block.iter());
    let flow =
        unwrap!(instr::eval_block_sequential(runner_ctx, Cow::Owned(ctx_local), instrs, true));
    match flow {
        // A return is the table result
        Flow::Return(value) => ok!(FuncResult::Return(value)),
        // Falling through or any other flow is an invalid table
        _ => fatal!(id.span.clone(), error::call::flow_invalid("table did not return a value"),),
    }
}

// - Defined function

/// Binds type parameters and arguments, then runs the body and otherwise block.
fn invoke_defined_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, SlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    id: &ast::Id,
    func: &ast::DefinedFunc,
    targs: &[ast::Typ],
    values: &[Value],
) -> Backtrack<FuncResult> {
    // Bind type arguments in the callee frame
    let ctx_local =
        unwrap!(assign_tparams(ctx.localize_with_layout(layout), &func.tparams, targs, &id.span));
    // Parameters bind after the type parameters, so their types resolve
    let ctx_local = unwrap!(assign::assign_params(
        runner_ctx.arena_mut(),
        ctx,
        ctx_local,
        &func.params,
        values
    ));
    // Run the body, falling back to the otherwise block
    let flow = unwrap!(instr::eval_block_with_else(
        runner_ctx,
        ctx_local,
        &func.block,
        func.block_else.as_deref()
    ));
    match flow {
        // Returns finish the function
        Flow::Return(value) => ok!(FuncResult::Return(value)),
        // Tail calls go back to the invoker loop
        Flow::TailFunc(id, targs, values) => ok!(FuncResult::TailCall(id, targs, values)),
        // Falling through the whole body is a mismatch
        Flow::Cont(errors) => unmatch!(errors),
        // Relation flows cannot appear in a function
        Flow::Result(_) | Flow::TailRel(..) => unreachable!("relation flow in function body"),
    }
}
