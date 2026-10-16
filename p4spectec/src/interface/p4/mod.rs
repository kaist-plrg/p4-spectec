//! P4 source frontend and runner interface
//!
//! `preprocessor` runs `cc -E`, `lexer` tokenizes with context-sensitive names,
//! `parse` drives the LALRPOP grammar into a value tree,
//! `context`, `declare`, and `extract` keep the name-resolution state
//! the grammar needs,
//! `binary` folds operator precedence, and `unparse` renders values back to P4.
//! `P4Interface` connects program parsing and builtins to the runner.

use std::path::PathBuf;

use crate::lang::{
    common::source::Span,
    data::{arena::Arena, value::flat::Value},
    il::ast::{Id, Typ},
};

use crate::runner::{Eval, Extern, Interface, InterfaceError, Interpreter, RunnerContext};

use self::error::{P4Error, P4ErrorKind};
use super::builtin::call::Builtins;

pub mod context;
pub mod error;
pub mod lexer;
pub mod parse;
pub mod preprocessor;
pub mod unparse;

mod binary;
mod declare;
mod extract;

// == Runner interface

/// Provides P4 program parsing and specification builtins.
pub struct P4Interface {
    builtins: Builtins,
}

impl P4Interface {
    /// Assembles the P4 interface with its configured builtin table.
    pub(crate) fn new(builtins: Builtins) -> Self {
        Self { builtins }
    }
}

impl Interface for P4Interface {
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        // Require one P4 translation unit
        let [path] = paths else {
            let error = P4Error::new(Span::default(), P4ErrorKind::InputCountInvalid(paths.len()));
            return Err(InterfaceError::from(error.into_report()));
        };

        // Preserve the parser diagnostic at the interface boundary
        parse::parse_file(arena, includes, path)
            .map_err(|error| InterfaceError::from(error.into_report()))
    }

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

/// The LALRPOP-generated parser for `interface/p4/parser.lalrpop`.
#[allow(clippy::extra_unused_lifetimes, clippy::let_unit_value, clippy::type_complexity)]
pub(crate) mod parser {
    include!(concat!(env!("OUT_DIR"), "/interface/p4/parser.rs"));
}
