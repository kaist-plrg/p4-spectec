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
    let value = match exp_iter.iter {
        // Option: evaluate once with the payloads, or produce none
        ast::Iter::Opt => {
            let vars_outer = iterate_vars(ctx, vars, exp_iter.iter);
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
            let inputs = unwrap_from_result!(ListInputs::new(runner_ctx.arena(), ctx, vars), span);
            let mut ctx_sub = ctx.clone();
            let mut values = Vec::with_capacity(inputs.len());
            for idx in 0..inputs.len() {
                inputs.bind(runner_ctx.arena(), &mut ctx_sub, vars, idx);
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
    let vars_bind_outer = iterate_vars(&ctx, vars_bind, prem_iter.iter);
    let mut values_bind_by_var = vec![Vec::new(); vars_bind.len()];
    match prem_iter.iter {
        // Option: run once when inputs are present; outputs become options
        ast::Iter::Opt => {
            let vars_bound_outer = iterate_vars(&ctx, vars_bound, prem_iter.iter);
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
            let inputs =
                unwrap_from_result!(ListInputs::new(runner_ctx.arena(), &ctx, vars_bound), span);
            for values in &mut values_bind_by_var {
                values.reserve(inputs.len());
            }
            let mut ctx_sub = ctx.clone();
            for idx in 0..inputs.len() {
                inputs.bind(runner_ctx.arena(), &mut ctx_sub, vars_bound, idx);
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

/// Stable list handles remain valid while a callback grows value storage.
pub(crate) struct ListInputs {
    bodies: ListBodies,
    len: usize,
}

enum ListBodies {
    Empty,
    One(crate::lang::data::intern::Interned<value::ValueKind>),
    Two([crate::lang::data::intern::Interned<value::ValueKind>; 2]),
    Many(Vec<crate::lang::data::intern::Interned<value::ValueKind>>),
}

impl ListBodies {
    fn as_slice(&self) -> &[crate::lang::data::intern::Interned<value::ValueKind>] {
        match self {
            Self::Empty => &[],
            Self::One(body) => std::slice::from_ref(body),
            Self::Two(bodies) => bodies,
            Self::Many(bodies) => bodies,
        }
    }
}

impl ListInputs {
    pub(crate) fn new(
        arena: &crate::lang::data::arena::Arena,
        ctx: &impl crate::interp::shared::context::ReadContext,
        vars: &[ast::Var],
    ) -> Result<Self, crate::interp::shared::error::Error> {
        let body = |var: &ast::Var| {
            let slot = ctx.find_slot_iterated(var, ast::Iter::List);
            let value = ctx.find_value_at_slot(slot).expect("value must be bound");
            value::get::list(arena, value).expect("iteration input must be a list");
            value.node
        };
        let bodies = match vars {
            [] => ListBodies::Empty,
            [var] => ListBodies::One(body(var)),
            [var_a, var_b] => ListBodies::Two([body(var_a), body(var_b)]),
            _ => ListBodies::Many(vars.iter().map(body).collect()),
        };
        let mut len = None;
        for body in bodies.as_slice() {
            let value::ValueKind::List(values) = arena.value.kind(*body) else { unreachable!() };
            if let Some(len) = len {
                if values.len() != len {
                    return Err(Box::new(
                        crate::interp::shared::error::context::iteration_length_mismatch(
                            len,
                            values.len(),
                        )
                        .into(),
                    ));
                }
            } else {
                len = Some(values.len());
            }
        }
        Ok(Self { bodies, len: len.unwrap_or(0) })
    }

    pub(crate) fn len(&self) -> usize {
        self.len
    }

    pub(super) fn body(
        &self,
        index: usize,
    ) -> crate::lang::data::intern::Interned<value::ValueKind> {
        self.bodies.as_slice()[index]
    }

    pub(crate) fn bind(
        &self,
        arena: &crate::lang::data::arena::Arena,
        ctx: &mut impl crate::interp::shared::context::WriteContext,
        vars: &[ast::Var],
        index: usize,
    ) {
        for (var, body) in vars.iter().zip(self.bodies.as_slice()) {
            let value::ValueKind::List(values) = arena.value.kind(*body) else { unreachable!() };
            ctx.add_value_at_slot(var.slot, values[index]);
        }
    }
}
