//! Iterated expression evaluation and binding collection
//!
//! `map` evaluates a body once per element of the iterated variables
//! and collects the results into an option or list value;
//! `yield` runs a premise the same way
//! and gathers the variables it binds
//! into lists or options one iteration outward.

use std::rc::Rc;

use crate::lang::{
    common::source::Span,
    data::value::flat::{self as value, Value},
};

use crate::lang::il::prepared as ast;

use crate::runner::{Extern, Interface, Interpreter, RunnerContext};

use crate::interp::shared::util::iterate_vars;

use super::super::{
    backtrack::{Backtrack, ok, unwrap, unwrap_from_result},
    context::IterContext,
};

// = Expression mapping

/// Maps `eval` over the iterated variables, building an option or list value.
pub fn map<Ctx, Interp, Iface, Ext>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    ctx: &Ctx,
    span: &Span,
    typ: &Rc<ast::TypKind>,
    exp_iter: &ast::ExpIter,
    mut eval: impl FnMut(&mut RunnerContext<'_, Interp, Iface, Ext>, &Ctx) -> Backtrack<Value>,
) -> Backtrack<Value>
where
    Ctx: IterContext,
    Interp: Interpreter<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
{
    // Slots of the variables one iteration outward
    let vars = &exp_iter.vars;
    let vars_outer = iterate_vars(ctx, vars, exp_iter.iter);
    let value = match exp_iter.iter {
        // Option: evaluate once with the payloads, or produce none
        ast::Iter::Opt => {
            let values = unwrap_from_result!(
                ctx.find_opt_values_by_var(runner_ctx.arena(), &vars_outer),
                span
            );
            let value = if let Some(values) = values {
                let mut ctx_sub = ctx.clone();
                for (var, value) in vars.iter().zip(values) {
                    ctx_sub.add_value_at_slot(var.slot, value);
                }
                Some(unwrap!(eval(runner_ctx, &ctx_sub)))
            } else {
                None
            };
            value::make::opt(runner_ctx.arena_mut(), typ.clone(), value, Span::default())
        }
        ast::Iter::List => {
            let values_by_var = unwrap_from_result!(
                ctx.find_list_values_by_var(runner_ctx.arena(), &vars_outer),
                span
            );
            // Copy handles before the callback can allocate in the arena
            let values_by_var: Vec<_> = values_by_var.into_iter().map(<[Value]>::to_vec).collect();
            // Evaluate row by row, binding each variable to its element
            let len = values_by_var.first().map_or(0, Vec::len);
            let mut ctx_sub = ctx.clone();
            let mut values = Vec::with_capacity(len);
            for idx in 0..len {
                for (var, values) in vars.iter().zip(&values_by_var) {
                    ctx_sub.add_value_at_slot(var.slot, values[idx]);
                }
                values.push(unwrap!(eval(runner_ctx, &ctx_sub)));
            }
            value::make::list(runner_ctx.arena_mut(), typ.clone(), values, Span::default())
        }
    };
    ok!(unwrap_from_result!(value, span))
}

// = Binding iteration

/// Runs `eval` per element and gathers its bindings one iteration outward.
pub fn r#yield<Ctx, Interp, Iface, Ext>(
    runner_ctx: &mut RunnerContext<'_, Interp, Iface, Ext>,
    mut ctx: Ctx,
    span: &Span,
    prem_iter: &ast::PremIter,
    mut eval: impl FnMut(&mut RunnerContext<'_, Interp, Iface, Ext>, Ctx) -> Backtrack<Ctx>,
) -> Backtrack<Ctx>
where
    Ctx: IterContext,
    Interp: Interpreter<Iface, Ext>,
    Iface: Interface,
    Ext: Extern,
{
    let vars_bound = &prem_iter.vars_bound;
    let vars_bind = &prem_iter.vars_bind;
    let vars_bound_outer = iterate_vars(&ctx, vars_bound, prem_iter.iter);
    let vars_bind_outer = iterate_vars(&ctx, vars_bind, prem_iter.iter);
    let mut values_bind_by_var = vec![Vec::new(); vars_bind.len()];
    match prem_iter.iter {
        // Option: run once when inputs are present; outputs become options
        ast::Iter::Opt => {
            let values = unwrap_from_result!(
                ctx.find_opt_values_by_var(runner_ctx.arena(), &vars_bound_outer),
                span
            );
            if let Some(values) = values {
                let mut ctx_sub = ctx.clone();
                for (var, value) in vars_bound.iter().zip(values) {
                    ctx_sub.add_value_at_slot(var.slot, value);
                }
                let ctx_post = unwrap!(eval(runner_ctx, ctx_sub));
                unwrap!(ctx_post.collect_values_by_var(vars_bind, &mut values_bind_by_var));
            }
            unwrap!(ctx.bind_opt_values_by_var(
                runner_ctx.arena_mut(),
                &vars_bind_outer,
                values_bind_by_var
            ));
        }
        // List: run per row, collecting each bound variable's column
        ast::Iter::List => {
            let values_by_var = unwrap_from_result!(
                ctx.find_list_values_by_var(runner_ctx.arena(), &vars_bound_outer),
                span
            );
            // Copy handles before the callback can allocate in the arena
            let values_by_var: Vec<_> = values_by_var.into_iter().map(<[Value]>::to_vec).collect();
            let len = values_by_var.first().map_or(0, Vec::len);
            let mut ctx_sub = ctx.clone();
            for idx in 0..len {
                for (var, values) in vars_bound.iter().zip(&values_by_var) {
                    ctx_sub.add_value_at_slot(var.slot, values[idx]);
                }
                // Keep callback writes out of the reusable input context
                let ctx_post = unwrap!(eval(runner_ctx, ctx_sub.clone()));
                unwrap!(ctx_post.collect_values_by_var(vars_bind, &mut values_bind_by_var));
            }
            // Bind each output variable to its collected column
            unwrap!(ctx.bind_list_values_by_var(
                runner_ctx.arena_mut(),
                &vars_bind_outer,
                values_bind_by_var
            ));
        }
    }
    ok!(ctx)
}
