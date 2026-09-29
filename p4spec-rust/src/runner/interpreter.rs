//! Stage-specific evaluation contract used by a composed runner
//!
//! An interpreter owns its configuration and cache for one language stage.
//! Evaluation receives the assembled runner context, including the interpreter,
//! so extern calls can reenter without a second mutable interpreter borrow.

use crate::{
    lang::{data::value::Value, il::ast::Typ},
    runner::{ExternError, InterfaceError},
};

use super::{Extern, Interface, RunnerContext};

// == Interpreter contract

/// A language stage's evaluator, parameterized by its host components.
pub trait Interpreter<Iface, Ext>: Sized
where
    Iface: Interface,
    Ext: Extern,
{
    /// Loaded global definitions.
    type Spec;
    /// The interpreter's error type, including builtin and extern failures.
    ///
    /// AL, SL, and PL use `Failure` for fatal errors and mismatches.
    type Error: From<InterfaceError> + From<ExternError>;

    /// Clears cached results without invalidating arena values.
    fn clear(&mut self);

    /// Resets program-owned execution state while retaining configuration.
    fn reset(&mut self);

    /// Evaluates an already parsed program through the selected entry.
    fn eval_program(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        program: Value,
    ) -> Result<Vec<Value>, Self::Error>;

    /// Calls a relation by name.
    ///
    /// AL, SL, and PL validate the name and input count on every call.
    /// With type guards disabled, values must match the declared input types.
    fn eval_rel(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, Self::Error>;

    /// Calls a function by name with type arguments.
    ///
    /// AL, SL, and PL validate the name and argument counts on every call.
    /// With type guards disabled, values and type arguments must match
    /// the declared signature.
    fn eval_func(
        ctx: &mut RunnerContext<'_, Self, Iface, Ext>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, Self::Error>;
}
