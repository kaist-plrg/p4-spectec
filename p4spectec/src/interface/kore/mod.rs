//! KORE frontend and runner interface for the K specification (`spec-k`)
//!
//! `parse` reads KORE text into `ast`, and `load` turns it into values
//! of the specification as it is.
//! `KoreInterface` loads a kompiled definition (`definition.kore`)
//! and then parses initial terms against its symbols.
//! `krun` runs the specification's `Krun` relation from an initial term.

use std::{collections::HashSet, path::Path, path::PathBuf};

use crate::lang::data::{arena::Arena, value::flat::Value};

use crate::lang::il::ast::{Id, Typ};

use crate::diagnostic::{Diagnostic, Report, Severity};

use crate::runner::{Eval, Extern, Interface, InterfaceError, Interpreter, RunnerContext};

use super::builtin::call::Builtins;

pub mod ast;
pub mod krun;
pub mod load;
pub mod parse;

// == Errors

const KORE_READ: &str = "kore/read";
const KORE_PARSE: &str = "kore/parse";
const KORE_LOAD: &str = "kore/load";
const KORE_INPUT: &str = "kore/input";

fn diagnostic(code: &str, message: impl Into<String>) -> InterfaceError {
    let report: Report =
        Diagnostic::new("kore", Severity::Error, Some(code.to_owned()), message, vec![], vec![])
            .into();
    InterfaceError::from(Box::new(report))
}

fn read(path: &Path) -> Result<Vec<u8>, InterfaceError> {
    std::fs::read(path)
        .map_err(|error| diagnostic(KORE_READ, format!("{}: {error}", path.display())))
}

fn parse_error(path: &Path, error: parse::ParseError) -> InterfaceError {
    let message = format!("{}:{}: {}", path.display(), error.line, error.message);
    diagnostic(KORE_PARSE, message)
}

fn load_error(path: &Path, error: load::LoadError) -> InterfaceError {
    let message = match error {
        load::LoadError::SymbolUndeclared(name) => format!("undeclared symbol {name}"),
        load::LoadError::Malformed(what) => format!("malformed {what}"),
        load::LoadError::Value(error) => error.to_string(),
    };
    diagnostic(KORE_LOAD, format!("{}: {message}", path.display()))
}

// == Runner interface

/// Reads KORE for the K specification, and provides its builtins.
pub struct KoreInterface {
    builtins: Builtins,
    /// The symbols and aliases of the loaded definition.
    known: Option<HashSet<String>>,
}

impl KoreInterface {
    /// Assembles the KORE interface with its builtin table.
    pub(crate) fn new(builtins: Builtins) -> Self {
        Self { builtins, known: None }
    }

    /// Loads a definition as a `kdefinition` value, with its number of axioms.
    ///
    /// Initial terms parsed afterwards may use only its symbols.
    pub fn load_definition(
        &mut self,
        arena: &mut Arena,
        path: &Path,
    ) -> Result<(Value, usize), InterfaceError> {
        let src = read(path)?;
        let definition = parse::parse_definition(&src).map_err(|error| parse_error(path, error))?;
        let known = load::known_symbols(&definition);
        let loaded = load::load_definition(arena, &known, &definition)
            .map_err(|error| load_error(path, error))?;
        self.known = Some(known);
        Ok(loaded)
    }
}

impl Interface for KoreInterface {
    /// Parses one initial term as a `kore` value.
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        _includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        let [path] = paths else {
            return Err(diagnostic(KORE_INPUT, format!("expected one term, got {}", paths.len())));
        };
        let Some(known) = &self.known else {
            return Err(diagnostic(KORE_INPUT, "a term is read after its definition"));
        };
        let src = read(path)?;
        let pattern = parse::parse_pattern(&src).map_err(|error| parse_error(path, error))?;
        load::load_term(arena, known, &pattern).map_err(|error| load_error(path, error))
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

    /// Keeps the loaded definition; resets builtin state.
    fn clear(&mut self) {
        self.builtins.init();
    }
}
