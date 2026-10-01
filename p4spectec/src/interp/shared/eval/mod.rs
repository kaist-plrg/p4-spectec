//! Shared assignment, expression, argument, and path evaluation
//!
//! `Invoker` is what AL and SL supply: how to call a function or a relation.
//! Everything else here is stage-independent:
//! `expr`, `assign`, `arg`, `path`, `ops`, and `iter`.

pub(crate) mod arg;
pub mod assign;
pub(crate) mod expr;
pub mod iter;
pub(crate) mod ops;
pub(crate) mod path;

use crate::lang::data::value::Value;

use crate::runner::{Extern, Interface, Interpreter, RunnerContext};

use crate::interp::shared::prepare::ast;

use super::{backtrack::Backtrack, context::IterContext};

// = Invocation

/// AL/SL-specific function and relation invocation.
pub(crate) trait Invoker<Iface, Ext>: Interpreter<Iface, Ext>
where
    Iface: Interface,
    Ext: Extern,
{
    /// The stage's evaluation context.
    type Context<'global>: IterContext;

    /// Calls function `id` with type arguments and evaluated argument values.
    fn invoke_func<'global>(
        runner_ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        ctx: &Self::Context<'global>,
        id: &ast::Id,
        targs: &[ast::Typ],
        values: &[Value],
    ) -> Backtrack<Value>;

    /// Calls relation `id` with its evaluated inputs, returning the outputs.
    fn invoke_rel<'global>(
        runner_ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        ctx: &Self::Context<'global>,
        id: &ast::Id,
        values: &[Value],
    ) -> Backtrack<Vec<Value>>;
}
