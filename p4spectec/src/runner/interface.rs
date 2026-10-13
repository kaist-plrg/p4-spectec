//! Runner-facing interface for stateful specification builtins
//!
//! Calls are delegated to an interface implementation
//! and report whether they changed interface state.
//! For example, `$fresh_typeId` returns `Impure` with its fresh identifier,
//! while a pure builtin such as `$sum_nat` returns `Pure` with its value.
//! Host failures with unknown effects conservatively return `Impure`.
//! Failures remain independent of source locations;
//! the interpreter that evaluates a call owns that location.

use crate::runner::{Eval, Extern, Interpreter, RunnerContext, SideEffect};

use thiserror::Error;

use crate::lang::data::value::flat::Value;

use crate::lang::il::ast::{Id, Typ};

use crate::diagnostic::{Diagnostic, Report, Severity};

use crate::interface::builtin::{BuiltinError, call::Builtins};

// == Interface errors

/// A fatal diagnostic produced by the builtin interface.
#[derive(Debug, Error)]
#[error(transparent)]
pub struct InterfaceError(#[from] pub Box<Report>);

const INTERFACE_UNCONFIGURED: &str = "runtime/interface-unconfigured";

impl InterfaceError {
    /// Describes an interface that has not been configured.
    pub fn diagnostic_unconfigured() -> Self {
        Self(Box::new(
            Diagnostic::new(
                "runtime",
                Severity::Error,
                Some(INTERFACE_UNCONFIGURED.to_owned()),
                "interface is not configured",
                vec![],
                vec![],
            )
            .into(),
        ))
    }

    /// Returns the interface diagnostic without reconstructing it.
    pub fn into_report(self) -> Box<Report> {
        self.0
    }
}

impl From<BuiltinError> for InterfaceError {
    fn from(error: BuiltinError) -> Self {
        Self(error.into_report())
    }
}

// == Interface contract

/// Stateful specification builtins, called by name.
pub trait Interface: Sized {
    /// Calls a builtin; retains its effect even if the call fails.
    fn call_builtin<Interp, Ext>(
        ctx: &mut RunnerContext<'_, Interp, Self, Ext>,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterfaceError>
    where
        Ext: Extern,
        Interp: Interpreter<Self, Ext>;

    /// Resets builtin state between programs.
    fn clear(&mut self);
}

// == Standard implementations

/// The standard builtin table.
pub struct BuiltinInterface {
    builtins: Builtins,
}

impl BuiltinInterface {
    pub fn new(builtins: Builtins) -> Self {
        Self { builtins }
    }
}

impl Interface for BuiltinInterface {
    fn call_builtin<Interp, Ext>(
        ctx: &mut RunnerContext<'_, Interp, Self, Ext>,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterfaceError>
    where
        Ext: Extern,
        Interp: Interpreter<Self, Ext>,
    {
        let (arena, interface) = ctx.arena_and_interface_mut();
        interface
            .builtins
            .invoke(arena, id, targs, values)
            .map_err(InterfaceError::from)
    }

    fn clear(&mut self) {
        self.builtins.init();
    }
}

/// An interface with no builtins; every call fails as not configured.
pub struct NullInterface;

impl Interface for NullInterface {
    fn call_builtin<Interp, Ext>(
        _ctx: &mut RunnerContext<'_, Interp, Self, Ext>,
        _id: &Id,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, InterfaceError>
    where
        Ext: Extern,
        Interp: Interpreter<Self, Ext>,
    {
        Eval::new(Err(InterfaceError::diagnostic_unconfigured()), SideEffect::Impure)
    }

    fn clear(&mut self) {}
}
