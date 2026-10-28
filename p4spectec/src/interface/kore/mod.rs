//! The interface of spec-k, the K semantics written in P4-SpecTec
//!
//! K-in-ST runs spec-k as the base of a single-level tower over KORE.
//! `KoreInterface::parse_program` reads a K definition (`definition.kore`
//! in a kompiled directory) and an initial term as spec-k's `kdefinition`
//! and `kore` values, the inputs of its entry relation `Krun`,
//! and loads the K library of that directory for `offload`.
//! `parse` reads KORE text into `ast`, and `encode` turns it into values
//! of spec-k as it is, its domain values read; `decode` writes them back as KORE.
//! Builtins are the standard ones plus those of `hooks`: the K hooks of domain
//! values, which spec-k delegates with one builtin (`builtin dec $call_hook`)
//! and which are offloaded to the K framework's own implementation in that library,
//! and spec-k's text builtins. Hooks on K terms are specified.
//! `KoreInterface::print_runres` prints the `runres` that `Krun` returns
//! as the LLVM backend's interpreter prints a run; `normal` gives the normal form
//! in which a run is compared with `krun`'s.

pub mod ast;
pub mod binary;
pub mod decode;
pub mod encode;
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
    /// The K runtime of the definition, which writes Float literals in its output format.
    runtime: Option<&'static Runtime>,
}

/// Reads `path_definition` and `path_init`, both KORE text,
/// as a tuple of spec-k's `kdefinition` and `kore` values,
/// with what writing terms of the definition back needs.
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
    let loaded = Rc::new(Loaded {
        known: encode::known_symbols(&definition),
        info: Info::new(&definition),
        mixops,
        runtime: Some(runtime),
    });
    let (value_definition, _) = encode::definition(arena, &loaded, &definition)
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
    /// The standard builtins and spec-k's of `hooks::hooks`.
    builtins: Builtins,
    /// The K library of the definition being run, which offloaded hooks call.
    offload: Rc<Offload>,
    /// The definition read, for printing its configurations.
    loaded: Option<Rc<Loaded>>,
    /// The next `$fresh_nat`.
    fresh: u64,
}

impl KoreInterface {
    /// Builds the interface with spec-k's builtins over the standard builtins.
    pub fn new() -> Self {
        let offload = Rc::new(Offload::new());
        let builtins = Builtins::with_extensions(hooks::hooks(&offload));
        Self { builtins, offload, loaded: None, fresh: 0 }
    }

    /// Prints `runres`, the result of spec-k's `Krun`, as the LLVM backend's
    /// interpreter prints a run with `--statistics`, which `krun` runs.
    ///
    /// `FINAL n term` prints `n`, the steps taken, on its first line, and
    /// then `term`, the final configuration, as KORE text, so that it compares
    /// with `krun`'s output. `ERROR n term` prints `error after n steps`
    /// and the configuration where a step failed, and `INITFAIL`
    /// `error on the initial term`: `krun` only reports such a failure.
    pub fn print_runres(&self, arena: &Arena, runres: Value) -> Result<String, InterfaceError> {
        let loaded = self.loaded.as_deref().ok_or_else(|| {
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
    /// Reads `paths`, a K definition then an initial term, as `parse_kore` does,
    /// after loading the K library kompiled with the definition for its hooks:
    /// reading a Float literal asks the library for its normal form.
    ///
    /// The definition is `definition.kore` in a kompiled directory,
    /// as `krun -d` reads it; the library is in that directory.
    /// `includes` is unused, since KORE is not preprocessed.
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
        self.offload.attach(Rc::clone(&loaded));
        self.loaded = Some(loaded);
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
