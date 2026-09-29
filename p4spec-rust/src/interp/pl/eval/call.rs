//! PL invocation, memoization, and call-boundary checks
//!
//! `invoke_rel` and `invoke_func` look up prepared callables,
//! serve eligible calls from the cache, and dispatch on the definition kind.
//! Defined bodies bind inputs in a fresh frame and run through `instr`.
//! Only pure results are memoized; failures retain their invocation trace.
//! With `guard` enabled, host outputs and public inputs are type-checked.

use crate::diagnostic::Diagnostic;
use crate::interp::shared::error;
use crate::interp::shared::eval::assign::assign_tparams;
use crate::lang::hints::input;
use crate::runtime::ops::{typ as typ_ops, value as value_ops};
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
            backtrack::{
                self, Backtrack, WithFrame, fatal, ok, unmatch, unwrap, unwrap_from_result,
            },
            cache::CallKey,
            context::ReadContext,
        },
    },
    lang::data::value::{Value, ValueArena, ValueKind},
    runner::{Extern, Interface, RunnerContext},
    runtime::envs::interp::{pl::ast_prepared as ast, shared::frame::FrameLayout},
};

// = Input and output checks

/// Checks the input count and, with `guard`, the input types.
pub(crate) fn check_rel_inputs(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    values: &[Value],
    guard: bool,
) -> Backtrack<()> {
    // Extern and defined relations share the signature shape
    let rel = unwrap_from_result!(ctx.find_rel(id), &id.span);
    let signature = match &rel.def {
        ast::RelDef::Extern(rel) => &rel.rel_signature,
        ast::RelDef::Defined(rel) => &rel.rel_signature,
    };
    // Always check the input count, even without type guards
    unwrap!(backtrack::check(
        signature.input_hint.indices().len() == values.len(),
        id.span.clone(),
        || {
            error::guard::relation_input_arity_mismatch(
                signature.input_hint.indices().len(),
                values.len(),
            )
        }
    ));
    if !guard {
        return ok!(());
    }
    // Select input types at the positions named by the hint
    let typs = signature.not_typ.node.args();
    let typs = signature
        .input_hint
        .indices()
        .iter()
        .map(|idx| typs[idx.node].clone())
        .collect::<Vec<_>>();
    check_values(arena, ctx, id, &typs, values, || {
        error::guard::relation_input_type_mismatch(id.node.clone())
    })
}

/// Checks argument counts and, with `guard`, the argument types.
pub(crate) fn check_func_inputs(
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
    // Parameter types resolve against the local type bindings
    check_values(arena, &ctx_local, id, &typ.typs_params, values, || {
        error::guard::function_input_type_mismatch(id.node.clone())
    })
}

/// Checks each value against its type, failing with the supplied guard error.
fn check_values(
    arena: &ValueArena,
    ctx: &Context<'_>,
    id: &ast::Id,
    typs: &[ast::Typ],
    values: &[Value],
    diagnostic: impl FnOnce() -> Diagnostic,
) -> Backtrack<()> {
    // Resolve type names and function types through the context
    let find_typdef_opt = |id: &ast::Id| ctx.find_typdef_opt(id);
    let find_func = |name: &str| {
        let id = crate::phrase!(node: name.to_owned(), span: id.span.clone());
        ctx.find_func_typ(&id).ok()
    };
    // Check all values against their declared types
    let matches = unwrap_from_result!(
        value_ops::subs(arena, &find_typdef_opt, &find_func, typs, values),
        &id.span
    );
    backtrack::check(matches, id.span.clone(), diagnostic)
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
    let theta = unwrap_from_result!(typ_ops::Theta::from_lists(tparams, targs), &id.span);
    let typ = unwrap_from_result!(typ_ops::subst_typ(&|id| theta.get(id), typ), &id.span);
    // Check the single result
    check_values(arena, ctx, id, &[typ], std::slice::from_ref(value), || {
        error::guard::function_output_type_mismatch(id.node.clone())
    })
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
                invoke_defined_rel(runner_ctx, ctx, &callable.layout, rel, values)
            }
        }
    });
    let pure = runner_ctx.interp_mut().cache.end();
    // Nest failures under the invocation trace
    let result = result.with_frame(id.span.clone(), || error::trace::message_rel_invocation(id));
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
    // Return extern failures without turning mismatches into fatal errors
    let (values, _) = unwrap!(result);
    // Check the number of extern outputs before assigning them
    let len =
        rel.rel_signature.not_typ.node.args().len() - rel.rel_signature.input_hint.indices().len();
    unwrap!(backtrack::check(len == values.len(), id.span.clone(), || {
        error::guard::relation_output_arity_mismatch(len, values.len())
    }));
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
        let (_, typs) = input::split(&rel.rel_signature.input_hint, typs)
            .expect("input hint must fit relation");
        unwrap!(check_values(runner_ctx.arena(), ctx, id, &typs, &values, || {
            error::guard::relation_output_type_mismatch(id.node.clone())
        },));
    }
    ok!(values)
}

// - Defined relation

/// Binds the inputs and runs the body with its otherwise block.
fn invoke_defined_rel<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
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
        fatal!(errors) => return fatal!(errors),
    };
    // Try the otherwise block from the original input bindings
    if matches!(flow, Flow::Cont(_))
        && let Some(block) = &rel.block_else_opt
    {
        flow = unwrap!(eval_dispatch_block(runner_ctx, ctx_local, block)).1;
    }
    match flow {
        // Results finish the relation
        Flow::Result(values) => ok!(values.node),
        // Falling through the entire body is a mismatch
        Flow::Cont(errors) => unmatch!(errors),
        // A relation cannot return a function result
        Flow::Return(_) => unreachable!("function flow in relation body"),
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
                invoke_table_func(runner_ctx, ctx, &func.layout, func_def, values)
            }
            ast::MetaFuncDef::Defined(func_def) => {
                invoke_defined_func(runner_ctx, ctx, &func.layout, id, func_def, targs, values)
            }
        }
    });
    let pure = runner_ctx.interp_mut().cache.end();
    // Nest failures under the invocation trace
    let result =
        result.with_frame(id.span.clone(), || error::trace::message_func_invocation(id, targs));
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
    // Return extern failures without turning mismatches into fatal errors
    let (value, _) = unwrap!(result);
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
        Err(failure) => Err(failure.with_span(&id.span)),
    }
}

// - Table function

/// Runs row blocks in sequence until one returns a value.
fn invoke_table_func<Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, PlInterp, Iface, Ext>,
    ctx: &Context<'_>,
    layout: &Rc<FrameLayout>,
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
        Flow::Return(value) => ok!(value.node),
        // Exhausting the rows preserves their recoverable failures
        Flow::Cont(errors) => unmatch!(errors),
        // Relation flows cannot appear in a function table
        Flow::Result(_) => unreachable!("relation flow in table body"),
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
        fatal!(errors) => return fatal!(errors),
    };
    // Try the otherwise block from the original input bindings
    if matches!(flow, Flow::Cont(_))
        && let Some(block) = &func.block_else_opt
    {
        flow = unwrap!(eval_group_block(runner_ctx, ctx_local, block)).1;
    }
    match flow {
        // Returns finish the function
        Flow::Return(value) => ok!(value.node),
        // Falling through the entire body is a mismatch
        Flow::Cont(errors) => unmatch!(errors),
        // A function cannot produce relation outputs
        Flow::Result(_) => unreachable!("relation flow in function body"),
    }
}
