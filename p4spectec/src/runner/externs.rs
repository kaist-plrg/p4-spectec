//! Host extern contract for a composed specification runner
//!
//! Each call receives the operation-local runner context
//! and may reenter the interpreter through it.
//! The result carries its own SideEffect,
//! so the extern keeps all architecture-specific state bookkeeping.
//! Failures describe the host operation;
//! the calling interpreter owns the source location.

use crate::runner::{Eval, SideEffect};

use thiserror::Error;

use crate::lang::{
    common::{
        prim::num::{self, NumericError},
        source::Span,
    },
    data::{
        arena::Arena,
        typ,
        value::{
            ValueError,
            flat::{self as value, Value},
        },
    },
};

use crate::{phrase, util::json::json};

use crate::lang::il::ast::Typ;

use crate::diagnostic::{Diagnostic, Report, Severity};

use crate::runner::InterpreterError;

use super::{Interface, Interpreter, RunnerContext};

// == Extern errors

/// A fatal diagnostic produced by a host extern.
#[derive(Debug, Error)]
#[error(transparent)]
pub struct ExternError(#[from] pub Box<Report>);

const EXTERN_UNCONFIGURED: &str = "runtime/extern-unconfigured";
const EXTERN_VALUE_INVALID: &str = "runtime/extern-value-invalid";
const EXTERN_NUMERIC_INVALID: &str = "runtime/extern-numeric-invalid";
const EXTERN_STATE_INVALID: &str = "runtime/extern-state-invalid";
const EXTERN_ALLOCATION_FAILED: &str = "runtime/extern-allocation-failed";

impl ExternError {
    /// Creates a host diagnostic without a source location.
    fn diagnostic(code: Option<&str>, message: impl Into<String>) -> Self {
        Self(Box::new(
            Diagnostic::new(
                "runtime",
                Severity::Error,
                code.map(str::to_owned),
                message,
                vec![],
                vec![],
            )
            .into(),
        ))
    }

    /// Describes an extern that has not been configured.
    pub fn diagnostic_unconfigured() -> Self {
        Self::diagnostic(Some(EXTERN_UNCONFIGURED), "extern is not configured")
    }

    /// Retains an external message without inventing a diagnostic code.
    pub fn diagnostic_message(message: impl Into<String>) -> Self {
        Self::diagnostic(None, message)
    }

    /// Returns the host diagnostic without reconstructing it.
    pub fn into_report(self) -> Box<Report> {
        self.0
    }
}

impl From<ValueError> for ExternError {
    fn from(error: ValueError) -> Self {
        Self::diagnostic(Some(EXTERN_VALUE_INVALID), error.to_string())
    }
}

impl From<NumericError> for ExternError {
    fn from(error: NumericError) -> Self {
        Self::diagnostic(Some(EXTERN_NUMERIC_INVALID), error.to_string())
    }
}

impl From<std::io::Error> for ExternError {
    fn from(error: std::io::Error) -> Self {
        Self::diagnostic(Some(EXTERN_STATE_INVALID), error.to_string())
    }
}

impl From<serde_json::Error> for ExternError {
    fn from(error: serde_json::Error) -> Self {
        Self::diagnostic(Some(EXTERN_STATE_INVALID), error.to_string())
    }
}

impl From<std::collections::TryReserveError> for ExternError {
    fn from(error: std::collections::TryReserveError) -> Self {
        Self::diagnostic(Some(EXTERN_ALLOCATION_FAILED), error.to_string())
    }
}

impl From<num_bigint::TryFromBigIntError<()>> for ExternError {
    fn from(_: num_bigint::TryFromBigIntError<()>) -> Self {
        Self::diagnostic(Some(EXTERN_NUMERIC_INVALID), "fixed-width value exceeds a machine word")
    }
}

impl From<InterpreterError> for ExternError {
    fn from(failure: InterpreterError) -> Self {
        // Finalize exhausted reentry before returning from the host call
        Self(failure.into_report())
    }
}

// == Extern contract

/// Total host relations and functions: each call returns values or a fatal error.
pub trait Extern: Sized {
    /// Evaluates a host relation; retains effects on success and failure.
    fn eval_rel<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Evaluates a host function; retains effects on success and failure.
    fn eval_func<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Drops program-owned state between programs.
    fn clear(&mut self);
}

// == Null implementation

/// An extern with no operations; every call fails as not configured.
pub struct NullExtern;

impl Extern for NullExtern {
    fn eval_rel<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Eval::new(Err(ExternError::diagnostic_unconfigured()), SideEffect::Impure)
    }

    fn eval_func<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Eval::new(Err(ExternError::diagnostic_unconfigured()), SideEffect::Impure)
    }

    fn clear(&mut self) {}
}

// == Script implementation

/// An extern answering the extern functions a script may declare.
///
/// For `extern syntax opaque` and `extern dec $opaque(nat) : opaque`,
/// `$opaque(1)` is the extern value `EXT "1"` of type `opaque`.
/// A tower whose top is a script answers its extern functions with `call_func`,
/// so a script evaluates them alike natively and through the tower.
pub struct ScriptExtern;

impl ScriptExtern {
    /// Calls extern function `name` of a script on `values`.
    pub fn call_func(
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Result<Value, ExternError> {
        match name {
            // Wrap a natural as an extern value of type `opaque`
            "opaque" => {
                let value_nat = value::get::one(values)?;
                let text = num::to_int(value::get::num(arena, value_nat)?).to_string();
                let typ =
                    typ::make::var(phrase!(node: "opaque".into(), span: Span::default()), vec![]);
                Ok(value::make::external(
                    arena,
                    typ.node.into(),
                    json::String(text).into(),
                    Span::default(),
                )?)
            }
            // Every other extern function stays unconfigured
            _ => Err(ExternError::diagnostic_unconfigured()),
        }
    }
}

impl Extern for ScriptExtern {
    // A script declares no extern relations
    fn eval_rel<Interp, Iface>(
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _values: &[Value],
    ) -> Eval<Vec<Value>, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        Eval::new(Err(ExternError::diagnostic_unconfigured()), SideEffect::Impure)
    }

    fn eval_func<Interp, Iface>(
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        _targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let (arena, _) = ctx.arena_and_external_mut();
        Eval::new(Self::call_func(arena, name, values), SideEffect::Pure)
    }

    fn clear(&mut self) {}
}
