//! Native execution of the base specification at one reentry depth
//!
//! The base is the only level that runs natively;
//! an `Engine` calls its relations and functions,
//! sending the host calls it makes to the levels lent above it.
//! Depth 0 borrows the base runner's interpreter; each deeper depth owns one.
//! `EngineKind` stores the engines; `spectec` executes AL or SL.
//! `InterfaceKind` answers the builtins of a base executed by `spectec`.

mod spectec;

pub(super) use spectec::{SpecTecEngine, SpecTecEngineState, SpecTecSpec};

use super::tower::UpperMut;
use crate::{
    interface::meta::MetaInterface,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Extern, Interface, InterfaceError, InterpreterError},
};
use std::path::PathBuf;

/// A native executor of the base specification at one depth.
pub(in crate::metacirc) trait Engine {
    /// Runs the program entry `name`, sending host calls to `levels`.
    fn call_program<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError>;

    /// Calls relation `name`, sending host calls to `levels`.
    fn call_rel<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError>;

    /// Calls function `name`, sending host calls to `levels`.
    fn call_func<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError>;

    /// Clears cached results without invalidating arena values.
    fn clear(&mut self);

    /// Clears cached results and builtin state between programs.
    fn reset(&mut self);
}

/// A lent engine.
pub(in crate::metacirc) enum EngineKind<'s> {
    /// The AL or SL interpreter of a P4-SpecTec specification.
    SpecTec(SpecTecEngine<'s>),
}

impl Engine for EngineKind<'_> {
    fn call_program<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        match self {
            Self::SpecTec(engine) => engine.call_program(arena, upper, effects, name, program),
        }
    }

    fn call_rel<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, InterpreterError> {
        match self {
            Self::SpecTec(engine) => engine.call_rel(arena, upper, effects, name, values),
        }
    }

    fn call_func<Ext: Extern>(
        &mut self,
        arena: &mut Arena,
        upper: &mut UpperMut<'_, Ext>,
        effects: &mut u64,
        name: &str,
        targs: &[Typ],
        values: &[Value],
    ) -> Result<Value, InterpreterError> {
        match self {
            Self::SpecTec(engine) => engine.call_func(arena, upper, effects, name, targs, values),
        }
    }

    fn clear(&mut self) {
        match self {
            Self::SpecTec(engine) => engine.clear(),
        }
    }

    fn reset(&mut self) {
        match self {
            Self::SpecTec(engine) => engine.reset(),
        }
    }
}

// == Base builtins

/// The builtins of a base specification that `SpecTecEngine` executes,
/// by the language the base interprets.
pub(in crate::metacirc) enum InterfaceKind {
    /// spec-meta, interpreting AL or IL.
    Meta(MetaInterface),
}

impl Interface for InterfaceKind {
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        match self {
            Self::Meta(interface) => interface.parse_program(arena, includes, paths),
        }
    }

    fn call_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterfaceError> {
        match self {
            Self::Meta(interface) => interface.call_builtin(arena, id, targs, values),
        }
    }

    fn clear(&mut self) {
        match self {
            Self::Meta(interface) => interface.clear(),
        }
    }
}
