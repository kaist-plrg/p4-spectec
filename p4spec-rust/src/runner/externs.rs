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
    lang::data::value::{Value, ValueError},
    lang::il::ast::Typ,
};

use super::{Interface, Interpreter, RunnerContext};

// == Extern errors

/// A failure inside a host extern.
#[derive(Clone, Debug, Error, Eq, PartialEq)]
pub enum ExternError {
    /// No extern is installed.
    #[error("extern is not configured")]
    NotConfigured,
    /// A value operation failed.
    #[error(transparent)]
    Value(#[from] ValueError),
    /// An architecture-specific failure, described by the extern.
    #[error("{0}")]
    Failure(String),
    /// A fixed-width value did not fit a machine word.
    #[error("fixed-width value exceeds a machine word")]
    MachineWord(#[from] num_bigint::TryFromBigIntError<()>),
}

// == Extern contract

/// Host relations and functions callable from a specification.
pub trait Extern: Sized {
    /// Evaluates a host relation; the flag reports a side effect.
    fn eval_rel<Interp, Iface>(
        &self,
        ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        name: &str,
        values: &[Value],
    ) -> Result<(Vec<Value>, bool), Interp::Error>
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
    ) -> Result<(Value, bool), Interp::Error>
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
    ) -> Result<(Vec<Value>, bool), Interp::Error>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let error = ExternError::NotConfigured;
        Err(error.into())
    }

    fn eval_func<Interp, Iface>(
        &self,
        _ctx: &mut RunnerContext<'_, Interp, Iface, Self>,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> Result<(Value, bool), Interp::Error>
    where
        Iface: Interface,
        Interp: Interpreter<Iface, Self>,
    {
        let error = ExternError::NotConfigured;
        Err(error.into())
    }

    fn clear(&mut self) {}
}
