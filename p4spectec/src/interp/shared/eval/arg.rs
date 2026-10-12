//! Shared argument evaluation
//!
//! An expression argument evaluates to its value;
//! a function argument `$f` becomes a function value carrying the type of `$f`.

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
    traits::print::Print,
};

use crate::lang::il::prepared as ast;

use crate::runner::{Eval, Extern, Interface, RunnerContext};

use crate::interp::shared::backtrack::{Backtrack, WithFrame, ok, unwrap, unwrap_from_result};

use super::super::context::ReadContext;

use super::{Invoker, expr::eval_exp};

/// Evaluates one argument, nesting failures under an evaluation trace.
fn eval_arg<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    arg: &ast::Arg,
) -> Eval<Value> {
    Eval::with(|effect| {
        let result = match &arg.node {
            ast::ArgKind::Exp(exp) => effect.include(eval_exp(runner_ctx, ctx, exp)),
            ast::ArgKind::Def(id) => eval_def_arg(runner_ctx.arena_mut(), ctx, id, &arg.span),
        };
        result.with_frame(arg.span, || {
            format!("while evaluating argument {}", arg.view(&runner_ctx.arena().mixop).to_string())
        })
    })
}

/// Evaluates arguments left to right.
pub(crate) fn eval_args<'global, Interp: Invoker<Iface, Ext>, Iface: Interface, Ext: Extern>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Interp::Context<'global>,
    args: &[ast::Arg],
) -> Eval<Vec<Value>> {
    Eval::with(|effect| {
        let mut values = Vec::with_capacity(args.len());
        for arg in args {
            values.push(unwrap!(effect.include(eval_arg(runner_ctx, ctx, arg))));
        }
        ok!(values)
    })
}

// - Function argument

/// Builds the function value for a function argument from the function's type.
fn eval_def_arg(
    arena: &mut Arena,
    ctx: &impl ReadContext,
    id: &ast::Id,
    span: &Span,
) -> Backtrack<Value> {
    // A function value carries the referenced function's type
    let typ_func = unwrap_from_result!(ctx.find_func_typ(id), span);
    let value = unwrap_from_result!(
        value::make::func(
            arena,
            id.clone(),
            typ_func.tparams,
            typ_func.typs_params,
            *typ_func.typ_ret,
            Span::default()
        ),
        span
    );
    ok!(value)
}
