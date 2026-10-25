//! KORE frontend and runner interface for the K specification (`spec-k`)
//!
//! `parse` reads KORE text into `ast`, and `load` turns it into values
//! of the specification as it is.
//! `KoreInterface` loads a kompiled definition (`definition.kore`)
//! and then parses initial terms against its symbols.
//! `krun` runs the specification's `Krun` relation from an initial term.
//!
//! Hooks on domain values are computed by the K runtime of the definition,
//! the specification's extern (`externs`, `llvm`): the arguments go as KORE
//! (`term`), and the answer comes back as KORE (`binary`) for the
//! specification to read. Hooks on K terms are specified.
//! `builtins` has the text builtins for reading domain values.

use std::{collections::HashSet, path::Path, path::PathBuf, rc::Rc};

use num_bigint::BigInt;

use crate::lang::{
    common::{prim::num::Natural, source::Span},
    data::{
        arena::Arena,
        value::flat::{self as value, Value},
    },
};

use crate::lang::il::ast::{Id, Typ};

use crate::diagnostic::{Diagnostic, Report, Severity};

use crate::runner::{
    Eval, Extern, Interface, InterfaceError, Interpreter, RunnerContext, SideEffect,
};

use super::builtin::call::Builtins;

use self::{ast::Pattern, info::Info, term::Shapes};

pub mod ast;
pub mod binary;
pub mod builtins;
pub mod check;
pub mod externs;
pub mod info;
pub mod krun;
pub mod llvm;
pub mod load;
pub mod normal;
pub mod parse;
pub mod term;

// == Errors

/// Diagnostic codes of the KORE interface.
const KORE_READ: &str = "kore/read";
const KORE_PARSE: &str = "kore/parse";
const KORE_LOAD: &str = "kore/load";
const KORE_INPUT: &str = "kore/input";
pub(crate) const KORE_RUNTIME: &str = "kore/runtime";

/// An error of the KORE interface, without a location.
pub(crate) fn diagnostic(code: &str, message: impl Into<String>) -> InterfaceError {
    let report: Report =
        Diagnostic::new("kore", Severity::Error, Some(code.to_owned()), message, vec![], vec![])
            .into();
    InterfaceError::from(Box::new(report))
}

/// The bytes of a file.
fn read(path: &Path) -> Result<Vec<u8>, InterfaceError> {
    std::fs::read(path)
        .map_err(|error| diagnostic(KORE_READ, format!("{}: {error}", path.display())))
}

/// A syntax error in a file, with its line.
fn parse_error(path: &Path, error: parse::ParseError) -> InterfaceError {
    let message = format!("{}:{}: {}", path.display(), error.line, error.message);
    diagnostic(KORE_PARSE, message)
}

/// An error loading a file into values.
fn load_error(path: &Path, error: load::LoadError) -> InterfaceError {
    diagnostic(KORE_LOAD, format!("{}: {error}", path.display()))
}

// == Runner interface

/// What reading and writing KORE needs of the loaded definition.
pub struct Loaded {
    /// The symbols and aliases of the definition.
    pub known: HashSet<String>,
    /// The sorts, collections, and binders of the definition.
    pub info: Info,
    /// The notation of the specification's values.
    pub shapes: Shapes,
}

/// Reads KORE for the K specification, and provides its builtins.
pub struct KoreInterface {
    builtins: Builtins,
    /// The loaded definition, shared with the extern.
    loaded: Option<Rc<Loaded>>,
    /// The next `$fresh_nat`.
    fresh: u64,
}

impl KoreInterface {
    /// Assembles the KORE interface with its builtin table.
    pub(crate) fn new(builtins: Builtins) -> Self {
        Self { builtins, loaded: None, fresh: 0 }
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
        let shapes = Shapes::new(arena).map_err(|message| diagnostic(KORE_LOAD, message))?;
        self.loaded = Some(Rc::new(Loaded { known, info: Info::new(&definition), shapes }));
        Ok(loaded)
    }

    /// The loaded definition, for the extern.
    pub fn loaded(&self) -> Option<Rc<Loaded>> {
        self.loaded.clone()
    }

    /// A term of the specification as a KORE pattern.
    pub fn kore_of_term(&self, arena: &Arena, value: &Value) -> Result<Pattern, String> {
        let Some(loaded) = &self.loaded else {
            return Err("no definition is loaded".to_owned());
        };
        term::to_pattern(arena, &loaded.info, &loaded.shapes, value)
    }

    /// The normal form of a pattern for comparing runs (`normal`).
    pub fn normal_text(&self, pattern: &Pattern) -> Result<String, String> {
        let Some(loaded) = &self.loaded else {
            return Err("no definition is loaded".to_owned());
        };
        Ok(normal::normal_text(&loaded.info, pattern))
    }

    /// `$fresh_nat() : nat`: 0, 1, 2, ... within a program.
    fn fresh_nat(&mut self, arena: &mut Arena) -> Result<Value, String> {
        let n = Natural::try_from(BigInt::from(self.fresh)).map_err(|e| e.to_string())?;
        self.fresh += 1;
        value::make::nat(arena, n, Span::default()).map_err(|e| e.to_string())
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
        let Some(loaded) = &self.loaded else {
            return Err(diagnostic(KORE_INPUT, "a term is read after its definition"));
        };
        let src = read(path)?;
        let pattern = parse::parse_pattern(&src).map_err(|error| parse_error(path, error))?;
        load::load_term(arena, &loaded.known, &pattern).map_err(|error| load_error(path, error))
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
        if id.node.as_ref() != "fresh_nat" {
            return interface
                .builtins
                .invoke(arena, id, targs, values)
                .map_err(InterfaceError::from);
        }
        // The one builtin with a state
        let result = interface
            .fresh_nat(arena)
            .map_err(|message| diagnostic(KORE_RUNTIME, message));
        Eval::new(result, SideEffect::Impure)
    }

    /// Keeps the loaded definition and runtime; resets builtin state.
    fn clear(&mut self) {
        self.builtins.init();
        self.fresh = 0;
    }
}
