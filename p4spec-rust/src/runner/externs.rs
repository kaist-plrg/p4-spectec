//! Host extern contract for a composed specification runner
//!
//! Each call receives the operation-local runner context
//! and may reenter the interpreter through it.
//! The result carries its own side-effect flag,
//! so the extern keeps all architecture-specific state bookkeeping.
//! Failures describe the host operation;
//! the calling interpreter owns the source location.

use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, Report, Severity},
    lang::common::prim::num::NumericError,
    lang::data::value::{Value, ValueError},
    lang::il::ast::Typ,
};

use super::{Interface, Interpreter, RunnerContext};

// == Extern errors

/// A failure inside a host extern.
#[derive(Debug, Error)]
pub enum ExternError {
    /// No extern is installed.
    #[error("extern is not configured")]
    ExternUnconfigured,
    /// A value operation failed.
    #[error(transparent)]
    Value(#[from] ValueError),
    /// An arbitrary external message, retained without an invented code.
    #[error("{0}")]
    Message(String),
    /// A structured fatal diagnostic supplied by the host.
    #[error(transparent)]
    Report(#[from] Box<Report>),
    /// A numeric operation failed.
    #[error(transparent)]
    Numeric(#[from] NumericError),
    /// A host I/O operation failed.
    #[error(transparent)]
    Io(#[from] std::io::Error),
    /// Serializing or decoding host state failed.
    #[error(transparent)]
    Encoding(#[from] serde_json::Error),
    /// Allocating host state failed.
    #[error(transparent)]
    Allocation(#[from] std::collections::TryReserveError),
    /// A fixed-width value did not fit a machine word.
    #[error("fixed-width value exceeds a machine word")]
    MachineWordOverflow(#[from] num_bigint::TryFromBigIntError<()>),
}

const EXTERN_UNCONFIGURED: &str = "runtime/extern-unconfigured";
const EXTERN_VALUE_INVALID: &str = "runtime/extern-value-invalid";
const EXTERN_NUMERIC_INVALID: &str = "runtime/extern-numeric-invalid";
const EXTERN_STATE_INVALID: &str = "runtime/extern-state-invalid";
const EXTERN_ALLOCATION_FAILED: &str = "runtime/extern-allocation-failed";

impl ExternError {
    /// Converts a host failure to its diagnostic at the interpreter boundary.
    pub fn into_report(self) -> Box<Report> {
        // Structured host failures cross the boundary without reconstruction
        match self {
            Self::Report(report) => report,
            error => {
                // Local failures acquire meaning here; external text stays uncoded
                let code = match &error {
                    Self::ExternUnconfigured => Some(EXTERN_UNCONFIGURED),
                    Self::Value(_) => Some(EXTERN_VALUE_INVALID),
                    Self::Numeric(_) | Self::MachineWordOverflow(_) => Some(EXTERN_NUMERIC_INVALID),
                    Self::Io(_) | Self::Encoding(_) => Some(EXTERN_STATE_INVALID),
                    Self::Allocation(_) => Some(EXTERN_ALLOCATION_FAILED),
                    Self::Message(_) => None,
                    Self::Report(_) => unreachable!(),
                };
                Box::new(
                    Diagnostic::new(
                        "runtime",
                        Severity::Error,
                        code.map(str::to_owned),
                        error.to_string(),
                        vec![],
                        vec![],
                    )
                    .into(),
                )
            }
        }
    }
}

// == Extern contract

/// Total host relations and functions: each call returns values or a fatal error.
pub trait Extern: Sized {
    /// Evaluates a host relation; the flag reports a side effect.
    fn eval_rel<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>;

    /// Evaluates a host function; the flag reports a side effect.
    fn eval_func<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<(Value, bool), ExternError>
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
        &self,
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _values: &[Value],
    ) -> Result<(Vec<Value>, bool), ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let error = ExternError::ExternUnconfigured;
        Err(error)
    }

    fn eval_func<Interp, Iface>(
        &self,
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Result<(Value, bool), ExternError>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let error = ExternError::ExternUnconfigured;
        Err(error)
    }

    fn clear(&mut self) {}
}
