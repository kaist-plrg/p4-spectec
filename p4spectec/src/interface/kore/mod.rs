//! The interface of spec-k, the K semantics written in P4-SpecTec
//!
//! `KoreInterface::parse_program` reads a K definition (`definition.kore` of a
//! kompiled directory) and an initial term as the inputs of spec-k's `Krun`:
//! `parse` reads KORE into `ast`, and `encode` turns it into spec-k's values;
//! `decode` writes them back. Builtins are the standard ones and those of `hooks`,
//! whose K hooks run in the definition's K library (`offload`).
//! `KoreInterface::print_runres` prints the `runres` of `Krun`.

pub mod ast;
mod binary;
mod decode;
mod encode;
mod ffi;
mod hooks;
pub mod info;
mod mixops;
pub mod normal;
mod offload;
pub mod parse;
mod texts;

use self::{decode::Runres, ffi::Runtime, info::Info, mixops::Mixops, offload::Offload};
use crate::{
    diagnostic::{Diagnostic, Report, Severity},
    interface::builtin::call::Builtins,
    lang::{
        common::{prim::num::Natural, source::Span},
        data::{
            arena::Arena,
            value::flat::{self as value, Value},
        },
        il::ast::{Id, Typ},
    },
    runner::{Eval, Interface, InterfaceError, SideEffect},
};
use std::{
    collections::HashSet,
    path::{Path, PathBuf},
    rc::Rc,
};

// == Errors

/// Makes a KORE interface diagnostic without a source location.
fn diagnostic(code: &str, message: impl Into<String>) -> Box<Report> {
    Box::new(
        Diagnostic::new("kore", Severity::Error, Some(code.to_owned()), message, vec![], vec![])
            .into(),
    )
}

/// Reports a part of the KORE interface without an implementation yet.
fn unimplemented(what: &str) -> Box<Report> {
    diagnostic("kore/unimplemented", format!("{what} is not implemented yet"))
}

/// The bytes of a file.
fn read(path: &Path) -> Result<Vec<u8>, InterfaceError> {
    std::fs::read(path).map_err(|error| {
        InterfaceError(diagnostic("kore/read", format!("{}: {error}", path.display())))
    })
}

/// A syntax error in a file, with its line.
fn parse_error(path: &Path, error: parse::ParseError) -> InterfaceError {
    let message = format!("{}:{}: {}", path.display(), error.line, error.message);
    InterfaceError(diagnostic("kore/parse", message))
}

/// An error reading a file into values.
fn load_error(path: &Path, error: impl std::fmt::Display) -> InterfaceError {
    InterfaceError(diagnostic("kore/load", format!("{}: {error}", path.display())))
}

// == Reading KORE

/// What reading and writing KORE needs of the definition read.
pub struct Loaded {
    /// The symbols and aliases of the definition.
    pub known: HashSet<String>,
    /// The sorts, collections, and binders of the definition.
    pub info: Info,
    /// The notation of spec-k's values.
    pub mixops: Mixops,
}

/// Reads `path_definition` and `path_init`, both KORE text,
/// as a tuple of spec-k's `kdefinition` and `kore` values,
/// with what writing terms of the definition back needs; `runtime`, the
/// definition's K runtime, writes Float literals in its output format.
///
/// The values are those spec-k's `Krun` takes (`spec-k/8-entry.watsup`).
fn parse_kore(
    arena: &mut Arena,
    runtime: &Runtime,
    path_definition: &Path,
    path_init: &Path,
) -> Result<(Value, Rc<Loaded>), InterfaceError> {
    let src = read(path_definition)?;
    let definition =
        parse::parse_definition(&src).map_err(|error| parse_error(path_definition, error))?;
    let mixops = Mixops::new(arena).map_err(|message| load_error(path_definition, message))?;
    let loaded = Rc::new(Loaded {
        known: encode::known_symbols(&definition),
        info: Info::new(&definition),
        mixops,
    });
    let value_definition = encode::definition(arena, &loaded, runtime, &definition)
        .map_err(|error| load_error(path_definition, error))?;
    let src = read(path_init)?;
    let pattern = parse::parse_pattern(&src).map_err(|error| parse_error(path_init, error))?;
    let value_init = encode::kore(arena, &loaded, runtime, &pattern)
        .map_err(|error| load_error(path_init, error))?;
    let value_program = encode::program(arena, value_definition, value_init)
        .map_err(|error| load_error(path_definition, error))?;
    Ok((value_program, loaded))
}

// == Interface

/// The parser and builtins of spec-k.
pub struct KoreInterface {
    /// The standard builtins and spec-k's of `hooks::hooks`.
    builtins: Builtins,
    /// The definition being run and its K library, which offloaded hooks call
    /// and printing a configuration reads.
    offload: Rc<Offload>,
    /// The next `$fresh_nat`.
    fresh: u64,
}

impl KoreInterface {
    /// Builds the interface with spec-k's builtins over the standard builtins.
    pub fn new() -> Self {
        let offload = Rc::new(Offload::new());
        let builtins = Builtins::with_extensions(hooks::hooks(&offload));
        Self { builtins, offload, fresh: 0 }
    }

    /// Prints `runres` as the interpreter krun runs prints a run with `--statistics`:
    /// for `FINAL n term`, `n` and then `term` as KORE; for `ERROR n term`,
    /// `error after n steps` and `term`; for `INITFAIL`, `error on the initial term`.
    pub fn print_runres(&self, arena: &Arena, runres: Value) -> Result<String, InterfaceError> {
        let loaded = self.offload.loaded().ok_or_else(|| {
            InterfaceError(diagnostic(
                "kore/print",
                "a run is printed after its definition is read",
            ))
        })?;
        let runres = decode::runres(arena, &loaded.info, &loaded.mixops, &runres)
            .map_err(|message| InterfaceError(diagnostic("kore/print", message)))?;
        Ok(match runres {
            Runres::Final(steps, term) => format!("{steps}\n{term}"),
            Runres::Error(steps, term) => format!("error after {steps} steps\n{term}"),
            Runres::InitFail => "error on the initial term".to_owned(),
        })
    }

    /// `$fresh_nat() : nat`: 0, 1, 2, ... within a program.
    fn fresh_nat(&mut self, arena: &mut Arena) -> Result<Value, InterfaceError> {
        let n = Natural::from(self.fresh);
        self.fresh += 1;
        value::make::nat(arena, n, Span::default())
            .map_err(|error| InterfaceError(diagnostic("kore/runtime", error.to_string())))
    }
}

impl Default for KoreInterface {
    fn default() -> Self {
        Self::new()
    }
}

impl Interface for KoreInterface {
    /// Reads `paths`, `definition.kore` of a kompiled directory and an initial term,
    /// after loading the K library of that directory, which reads Float literals.
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        _includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        let [path_definition, path_init] = paths else {
            return Err(InterfaceError(diagnostic(
                "kore/input-invalid",
                "a KORE input is a definition and an initial term".to_owned(),
            )));
        };
        // The definition's hooks run in the library kompiled next to it
        let dir_kompiled = path_definition.parent().unwrap_or(Path::new("."));
        let runtime = self.offload.load(dir_kompiled)?;
        let (value_program, loaded) = parse_kore(arena, runtime, path_definition, path_init)?;
        self.offload.attach(loaded);
        Ok(value_program)
    }

    fn call_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterfaceError> {
        // The one builtin with a state, never memoized
        if id.node.as_ref() == "fresh_nat" {
            return Eval::new(self.fresh_nat(arena), SideEffect::Impure);
        }
        self.builtins
            .invoke(arena, id, targs, values)
            .map_err(InterfaceError::from)
    }

    /// Resets builtin state; the definition and its library stay.
    fn clear(&mut self) {
        self.builtins.init();
        self.fresh = 0;
    }
}
