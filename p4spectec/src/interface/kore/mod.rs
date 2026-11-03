//! The interface of spec-k, the K semantics written in P4-SpecTec
//!
//! `KoreInterface::parse_program` reads a K definition (`definition.kore` of a
//! kompiled directory) and an initial term as the inputs of spec-k's `Krun`:
//! `parse` reads KORE into `ast`, and `encode` turns it into spec-k's values;
//! `decode` writes them back. Builtins are the standard ones and the text
//! builtins of `texts`. spec-k's extern relation `Call_hook` runs in the
//! definition's K library (`offload`), which `KLibrary` lends to the top of the
//! tower, the K definition above spec-k.
//! `KoreInterface::print_runres` prints the `runres` of `Krun`.

pub mod ast;
mod binary;
mod decode;
mod encode;
mod ffi;
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
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, ExternError, Interface, InterfaceError},
};
use std::{
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

/// The definition read, as reading and writing its KORE needs it.
pub struct Loaded {
    /// The K runtime of the definition's library, which `Call_hook` calls
    /// and which reads Float literals.
    pub runtime: &'static Runtime,
    /// The symbols, sorts, collections, and binders of the definition.
    pub info: Info,
    /// The notation of spec-k's values.
    pub mixops: Mixops,
}

/// Reads `path_definition` and `path_init`, both KORE text,
/// as a tuple of spec-k's `kdefinition` and `kore` values,
/// and the definition read, with `runtime`, its K runtime.
///
/// The values are those spec-k's `Krun` takes (`spec-k/8-entry.watsup`).
fn parse_kore(
    arena: &mut Arena,
    runtime: &'static Runtime,
    path_definition: &Path,
    path_init: &Path,
) -> Result<(Value, Rc<Loaded>), InterfaceError> {
    let src = read(path_definition)?;
    let definition =
        parse::parse_definition(&src).map_err(|error| parse_error(path_definition, error))?;
    let mixops = Mixops::new(arena).map_err(|message| load_error(path_definition, message))?;
    let loaded = Rc::new(Loaded { runtime, info: Info::new(&definition), mixops });
    let value_definition = encode::definition(arena, &loaded, &definition)
        .map_err(|error| load_error(path_definition, error))?;
    let src = read(path_init)?;
    let pattern = parse::parse_pattern(&src).map_err(|error| parse_error(path_init, error))?;
    let value_init =
        encode::kore(arena, &loaded, &pattern).map_err(|error| load_error(path_init, error))?;
    let value_program = encode::program(arena, value_definition, value_init)
        .map_err(|error| load_error(path_definition, error))?;
    Ok((value_program, loaded))
}

// == Interface

/// The parser and builtins of spec-k.
pub struct KoreInterface {
    /// The standard builtins and the text builtins of `texts`.
    builtins: Builtins,
    /// The definition being run, with its K library, which `Call_hook` calls
    /// and printing a configuration reads.
    offload: Rc<Offload>,
}

impl KoreInterface {
    /// Builds the interface with spec-k's builtins over the standard builtins.
    pub fn new() -> Self {
        let offload = Rc::new(Offload::new());
        let builtins = Builtins::with_extensions(texts::entries());
        Self { builtins, offload }
    }

    /// Returns the K library of the definition this interface reads,
    /// which answers spec-k's extern relations once a definition is read.
    pub fn library(&self) -> KLibrary {
        KLibrary(Rc::clone(&self.offload))
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
}

/// The K library of the definition being run: the top of a K-in-ST tower,
/// the K definition above spec-k, answering spec-k's extern relation `Call_hook`.
pub struct KLibrary(Rc<Offload>);

impl KLibrary {
    /// Evaluates extern relation `name` of spec-k on `values`.
    pub fn eval_rel(
        &self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Result<Vec<Value>, ExternError> {
        self.0.call(arena, name, values)
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
        let runtime = offload::open(dir_kompiled)?;
        let (value_program, loaded) = parse_kore(arena, runtime, path_definition, path_init)?;
        self.offload.set(loaded);
        Ok(value_program)
    }

    fn call_builtin(
        &mut self,
        arena: &mut Arena,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterfaceError> {
        self.builtins
            .invoke(arena, id, targs, values)
            .map_err(InterfaceError::from)
    }

    /// Resets builtin state; the definition and its library stay.
    fn clear(&mut self) {
        self.builtins.init();
    }
}
