//! PL invocation, memoization, and call-boundary checks
//!
//! `invoke_rel` and `invoke_func` look up prepared callables,
//! serve eligible calls from the cache, and dispatch on the definition kind.
//! Defined bodies bind inputs in a fresh frame and run through `instr`.
//! Only pure results are memoized; failures retain their invocation trace.
//! With `guard` enabled, host outputs and public inputs are type-checked.

use crate::interp::shared::eval::assign::assign_tparams;
use std::rc::Rc;

use super::{
    assign::{assign_exps, assign_params},
    instr::{eval_block, eval_dispatch_block, eval_group_block, eval_group_instr},
};
use crate::{
    interp::{
        pl::{
            PlInterp,
            context::{Context, Scope},
            flow::Flow,
        },
        shared::{
            backtrack::{Backtrack, err, ok, unmatch, unwrap, unwrap_from_result},
            cache::CallKey,
            context::ReadContext,
            error::{CallErrorKind, ErrorKind, GuardErrorKind, HostErrorKind, TraceErrorKind},
        },
    },
    lang::data::value::{Value, ValueArena, ValueKind},
    runner::{Extern, Interface, InterfaceError, RunnerContext},
    runtime::envs::interp::{pl::ast_prepared as ast, shared::frame::FrameLayout},
};

// = Input and output checks

/// Type-checks relation inputs at the positions selected by the input hint.
pub(crate) fn check_rel_inputs(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    values: &[Value],
) -> Backtrack<()> {
    // Extern and defined relations share the signature shape
    let rel = unwrap_from_result!(ctx.find_rel(id), &id.span);
    let signature = match &rel.def {
        ast::RelDef::Extern(rel) => &rel.rel_signature,
        ast::RelDef::Defined(rel) => &rel.rel_signature,
    };
    let typs = signature.not_typ.node.args();
    // The hint must fit the notation arity
    unwrap_from_result!(
        crate::lang::hints::input::validate(&signature.input_hint, typs.len()),
        &id.span
    );
    // Select input types at the positions named by the hint
    let typs = signature
        .input_hint
        .indices()
        .iter()
        .map(|idx| typs[idx.node].clone())
        .collect::<Vec<_>>();
    check_values(
        arena,
        ctx,
        id,
        &typs,
        values,
        GuardErrorKind::RelationInputMismatch { relation: id.node.clone() },
    )
}

/// Type-checks function arguments with local type parameters bound.
pub(crate) fn check_func_inputs(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    targs: &[ast::Typ],
    values: &[Value],
) -> Backtrack<()> {
    let typ = unwrap_from_result!(ctx.find_func_typ(id), &id.span);
    // Bind type arguments before checking parameter types
    let ctx_local = unwrap!(assign_tparams(ctx.localize(), &typ.tparams, targs, &id.span));
    // Parameter types resolve against the local type bindings
    check_values(
        arena,
        &ctx_local,
        id,
        &typ.typs_params,
        values,
        GuardErrorKind::FunctionInputMismatch { func: id.node.clone() },
    )
}

/// Checks each value against its type, failing with the supplied guard error.
fn check_values(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    typs: &[ast::Typ],
    values: &[Value],
    error: GuardErrorKind,
) -> Backtrack<()> {
    // Resolve type names and function types through the context
    let find_typdef_opt = |id: &ast::Id| ctx.find_typdef_opt(id);
    let find_func = |name: &str| {
        let id = crate::phrase!(node: name.to_owned(), span: id.span.clone());
        ctx.find_func_typ(&id).ok()
    };
    // Check all values against their declared types
    let matches = unwrap_from_result!(
        crate::runtime::ops::value::subs(arena, &find_typdef_opt, &find_func, typs, values),
        &id.span
    );
    Backtrack::check(matches, id.span.clone(), ErrorKind::Guard(error))
}

/// Type-checks a function result with its type arguments substituted.
fn check_func_output(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    tparams: &[ast::TParam],
    typ: &ast::Typ,
    targs: &[ast::Typ],
    value: &Value,
) -> Backtrack<()> {
    // Substitute type arguments into the declared result type
    let theta =
        unwrap_from_result!(crate::runtime::ops::typ::Theta::from_lists(tparams, targs), &id.span);
    let typ = unwrap_from_result!(
        crate::runtime::ops::typ::subst_typ(&|id| theta.get(id), typ),
        &id.span
    );
    // Check the single result
    check_values(
        arena,
        ctx,
        id,
        &[typ],
        std::slice::from_ref(value),
        GuardErrorKind::FunctionOutputMismatch { func: id.node.clone() },
    )
}

// = Cache eligibility

/// Checks whether memoization is enabled for this defined relation.
pub(crate) fn cache_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
) -> bool {
    runner_ctx.interp().config.cache
        && matches!(ctx.find_rel(id), Ok(rel) if matches!(&rel.def, ast::RelDef::Defined(_)))
}

/// Checks whether a function call may be memoized.
///
/// Requires caching on, a global non-extern function,
/// and no function-valued argument.
pub(crate) fn cache_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &RunnerContext<'_, PlInterp, Iface, Ext>,
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
pub(crate) fn invoke_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
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
        let callable = unwrap_from_result!(ctx.find_rel(id), &id.span);
        match &callable.def {
            ast::RelDef::Extern(rel) => invoke_extern_rel(runner_ctx, ctx, id, rel, values),
            ast::RelDef::Defined(rel) => {
                invoke_defined_rel(runner_ctx, ctx, &callable.layout, id, rel, values)
            }
        }
    });
    let pure = runner_ctx.interp_mut().cache.end();
    // Nest failures under the invocation trace
    let result = result.nest(id.span.clone(), || {
        ErrorKind::Trace(TraceErrorKind::Invocation { text: id.node.clone() })
    });
    let values = unwrap!(result);
    // Memoize only a pure result
    if pure && let Some(key) = key {
        runner_ctx
            .interp_mut()
            .cache
            .rels
            .insert(key, values.clone());
    }
    ok!(values)
}

// - Extern relation

/// Calls a host relation, recording its effect and guarding its outputs.
fn invoke_extern_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
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
    // A host error is fatal
    let (values, _) = unwrap_from_result!(result, &id.span);
    // Guard the outputs against their declared types
    if runner_ctx.interp().config.guard {
        let typs = rel
            .rel_signature
            .not_typ
            .node
            .args()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();
        // Output types occupy the positions the input hint leaves
        let (_, typs) = unwrap_from_result!(
            crate::lang::hints::input::split(&rel.rel_signature.input_hint, typs),
            &id.span
        );
        unwrap!(check_values(
            runner_ctx.arena(),
            ctx,
            id,
            &typs,
            &values,
            GuardErrorKind::RelationOutputMismatch { relation: id.node.clone() },
        ));
    }
    ok!(values)
}

// - Defined relation

/// Binds the inputs and runs the body with its otherwise block.
fn invoke_defined_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    id: &ast::Id,
    rel: &ast::DefinedRel,
    values: &[Value],
) -> Backtrack<Vec<Value>> {
    // Inputs bind into a fresh frame
    let ctx_local = unwrap!(assign_exps(
        runner_ctx.arena_mut(),
        ctx.localize_with_layout(layout),
        &rel.exps_input,
        values
    ));
    // Run the body, retaining recoverable mismatches as continuations
    let mut flow = match eval_dispatch_block(runner_ctx, ctx_local.clone(), &rel.block) {
        // A completed block reports its own conclusion or continuation
        ok!((_, flow)) => flow,
        // A recoverable mismatch permits the otherwise block
        unmatch!(errors) => Flow::Cont(errors),
        // A fatal error aborts the call
        err!(errors) => return err!(errors),
    };
    // Try the otherwise block from the original input bindings
    if matches!(flow, Flow::Cont(_))
        && let Some(block) = &rel.block_else_opt
    {
        flow = unwrap!(eval_dispatch_block(runner_ctx, ctx_local, block)).1;
    }
    match flow {
        // Results finish the relation
        Flow::Result(values) => ok!(values),
        // Falling through the entire body is a mismatch
        Flow::Cont(errors) => unmatch!(errors),
        // A relation cannot return a function result
        Flow::Return(_) => err!(
            id.span.clone(),
            ErrorKind::Call(CallErrorKind::InvalidFlow {
                message: "relation cannot return a value",
            })
        ),
    }
}

// = Function invocation

/// Invokes a function, memoizing pure results of eligible calls.
pub(crate) fn invoke_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
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
        match &func.def {
            ast::MetaFuncDef::Extern(func) => {
                invoke_extern_func(runner_ctx, ctx, id, func, targs, values)
            }
            ast::MetaFuncDef::Builtin(func) => {
                invoke_builtin_func(runner_ctx, ctx, id, func, targs, values)
            }
            ast::MetaFuncDef::Table(func_def) => {
                invoke_table_func(runner_ctx, ctx, &func.layout, id, func_def, values)
            }
            ast::MetaFuncDef::Defined(func_def) => {
                invoke_defined_func(runner_ctx, ctx, &func.layout, id, func_def, targs, values)
            }
        }
    });
    let pure = runner_ctx.interp_mut().cache.end();
    // Nest failures under the invocation trace
    let result =
        result.nest(id.span.clone(), || ErrorKind::Trace(TraceErrorKind::function(id, targs)));
    let value = unwrap!(result);
    // Memoize only a pure result
    if pure && let Some(key) = key {
        runner_ctx.interp_mut().cache.funcs.insert(key, value);
    }
    ok!(value)
}

// - Extern function

/// Calls a host function, recording its effect and guarding its result.
fn invoke_extern_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    func: &ast::ExternFunc,
    targs: &[ast::Typ],
    values: &[Value],
) -> Backtrack<Value> {
    // The host reports whether the call had a side effect
    let result = runner_ctx.call_extern_func(&id.node, &[], values);
    runner_ctx
        .interp_mut()
        .cache
        .mark_effect(result.as_ref().map_or(true, |(_, effect)| *effect));
    // A host error is fatal
    let (value, _) = unwrap_from_result!(result, &id.span);
    // Guard the outputs against their declared types
    if runner_ctx.interp().config.guard {
        unwrap!(check_func_output(
            runner_ctx.arena(),
            ctx,
            id,
            &func.tparams,
            &func.typ,
            targs,
            &value,
        ));
    }
    ok!(value)
}

// - Builtin function

/// Calls a builtin; its own failure is a mismatch, other errors are fatal.
fn invoke_builtin_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    id: &ast::Id,
    func: &ast::BuiltinFunc,
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
        // A successful builtin result may still fail its output guard
        Ok((value, _)) => {
            if runner_ctx.interp().config.guard {
                unwrap!(check_func_output(
                    runner_ctx.arena(),
                    ctx,
                    id,
                    &func.tparams,
                    &func.typ,
                    targs,
                    &value,
                ));
            }
            ok!(value)
        }
        // Builtin failures allow another candidate; other errors are fatal
        Err(error) => {
            let recoverable = matches!(
                error.kind.as_ref(),
                ErrorKind::Host(HostErrorKind::Interface(InterfaceError::Builtin(_)))
            );
            let error = error.at_if_missing(&id.span);
            if recoverable { unmatch!(vec![error]) } else { err!(vec![error]) }
        }
    }
}

// - Table function

/// Runs row blocks in sequence until one returns a value.
fn invoke_table_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    id: &ast::Id,
    func: &ast::TableFunc,
    values: &[Value],
) -> Backtrack<Value> {
    // Parameters bind into a fresh frame
    let ctx_local = unwrap!(assign_params(
        runner_ctx.arena_mut(),
        ctx,
        ctx.localize_with_layout(layout),
        &func.params,
        values
    ));
    // Table rows stay sequential and borrow their instructions
    let instrs = func.rows.iter().flat_map(|row| &row.block);
    let (_, flow) = unwrap!(eval_block(runner_ctx, ctx_local, instrs, eval_group_instr));
    match flow {
        // The first return is the table result
        Flow::Return(value) => ok!(value),
        // Falling through or producing relation outputs is invalid
        _ => err!(
            id.span.clone(),
            ErrorKind::Call(CallErrorKind::InvalidFlow { message: "table did not return a value" })
        ),
    }
}

// - Defined function

/// Binds type parameters and arguments, then runs the body and otherwise block.
fn invoke_defined_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
    id: &ast::Id,
    func: &ast::DefinedFunc,
    targs: &[ast::Typ],
    values: &[Value],
) -> Backtrack<Value> {
    // Bind type arguments in the callee frame
    let ctx_local =
        unwrap!(assign_tparams(ctx.localize_with_layout(layout), &func.tparams, targs, &id.span));
    // Bind parameters after their type arguments are in scope
    let ctx_local =
        unwrap!(assign_params(runner_ctx.arena_mut(), ctx, ctx_local, &func.params, values));
    // Run the body, retaining recoverable mismatches as continuations
    let mut flow = match eval_group_block(runner_ctx, ctx_local.clone(), &func.block) {
        // A completed block reports its own conclusion or continuation
        ok!((_, flow)) => flow,
        // A recoverable mismatch permits the otherwise block
        unmatch!(errors) => Flow::Cont(errors),
        // A fatal error aborts the call
        err!(errors) => return err!(errors),
    };
    // Try the otherwise block from the original input bindings
    if matches!(flow, Flow::Cont(_))
        && let Some(block) = &func.block_else_opt
    {
        flow = unwrap!(eval_group_block(runner_ctx, ctx_local, block)).1;
    }
    match flow {
        // Returns finish the function
        Flow::Return(value) => ok!(value),
        // Falling through the entire body is a mismatch
        Flow::Cont(errors) => unmatch!(errors),
        // A function cannot produce relation outputs
        Flow::Result(_) => err!(
            id.span.clone(),
            ErrorKind::Call(CallErrorKind::InvalidFlow {
                message: "function cannot produce a relation result",
            })
        ),
    }
}
