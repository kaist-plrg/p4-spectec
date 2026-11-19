//! Runner-facing interface for program parsing and specification builtins
//!
//! Parsing builds program values in the supplied arena.
//! Builtin calls report whether they changed interface state.
//! For example, `$fresh_typeId` returns `Impure` with its fresh identifier,
//! while a pure builtin such as `$sum_nat` returns `Pure` with its value.
//! Host failures with unknown effects conservatively return `Impure`.
//! Parser failures retain input locations;
//! builtin call locations belong to the interpreter.

use std::path::PathBuf;

use crate::runner::{Eval, Extern, Interpreter, RunnerContext, SideEffect};

use thiserror::Error;

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::il::ast::{Id, Typ};

use crate::diagnostic::{Diagnostic, Report, Severity};

use crate::interface::builtin::BuiltinError;

// == Interface errors

/// A diagnostic produced by program parsing or a builtin call.
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

/// Program parsing and stateful builtins for the defined language.
pub trait Interface: Sized {
    /// Parses input files into a program value in the supplied arena.
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError>;

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

    /// Resets interface state between programs.
    fn clear(&mut self);
}

// == Standard implementations

/// An unconfigured interface whose parsing and builtin calls fail.
pub struct NullInterface;

impl Interface for NullInterface {
    fn parse_program(
        &mut self,
        _arena: &mut Arena,
        _includes: &[PathBuf],
        _paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        Err(InterfaceError::diagnostic_unconfigured())
    }

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
