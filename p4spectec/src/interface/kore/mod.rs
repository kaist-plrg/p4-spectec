//! The interface of spec-k, the K semantics written in P4-SpecTec
//!
//! K-in-ST runs spec-k as the base of a single-level tower over KORE.
//! `KoreInterface::parse_program` reads a K definition (`definition.kore`
//! in a kompiled directory) and an initial term as spec-k's `kdefinition`
//! and `kore` values, the inputs of its entry relation `Krun`,
//! and loads the K library of that directory for `offload`.
//! Builtins are the standard ones plus the K hooks that spec-k delegates
//! (`builtin dec $…`), registered in `hooks`; most are offloaded
//! to the K framework's own implementation in that library.
//! `KoreInterface::print_runres` prints the `runres` that `Krun` returns
//! as `krun` reports the final configuration.
//! The parser, the printer, the offload, and the hooks are slots
//! that the K-in-ST implementation fills; `docs/metacirc/k-in-st.md` says how.

mod ffi;
mod hooks;
mod offload;

use self::offload::Offload;
use crate::{
    diagnostic::{Diagnostic, Report, Severity},
    interface::builtin::call::Builtins,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Interface, InterfaceError},
};
use std::{
    path::{Path, PathBuf},
    rc::Rc,
};

/// Makes a KORE interface diagnostic without a source location.
fn diagnostic(code: &str, message: String) -> Box<Report> {
    Box::new(
        Diagnostic::new("kore", Severity::Error, Some(code.to_owned()), message, vec![], vec![])
            .into(),
    )
}

/// Reports a part of the KORE interface without an implementation yet.
fn unimplemented(what: &str) -> Box<Report> {
    diagnostic("kore/unimplemented", format!("{what} is not implemented yet"))
}

/// Reads `path_definition` and `path_init`, both KORE text,
/// as a tuple of spec-k's `kdefinition` and `kore` values.
///
/// The values are those spec-k's `Krun` takes (`spec-k/7-entry.watsup`);
/// #395 builds them in `interface/kore/parse.ml` and `load.ml`.
fn parse_kore(
    _arena: &mut Arena,
    _path_definition: &Path,
    _path_init: &Path,
) -> Result<Value, InterfaceError> {
    Err(InterfaceError(unimplemented("KoreInterface::parse_program")))
}

/// The parser and builtins of spec-k.
pub struct KoreInterface {
    /// The standard builtins and the K hooks of `hooks::hooks`.
    builtins: Builtins,
    /// The K library of the definition being run, which offloaded hooks call.
    offload: Rc<Offload>,
}

impl KoreInterface {
    /// Builds the interface with the K hooks over the standard builtins.
    pub fn new() -> Self {
        let offload = Rc::new(Offload::new());
        let builtins = Builtins::with_extensions(hooks::hooks(&offload));
        Self { builtins, offload }
    }

    /// Prints `runres`, the result of spec-k's `Krun`, as `krun` prints a run.
    ///
    /// `FINAL n term` prints `term`, the final configuration, as KORE text,
    /// so that it compares with `krun`'s output;
    /// `ERROR n term` and `INITFAIL` report how the run failed.
    /// #395 prints them in `interface/kore/unparse.ml` and `krun.ml`.
    pub fn print_runres(&self, _arena: &Arena, _runres: Value) -> Result<String, InterfaceError> {
        Err(InterfaceError(unimplemented("KoreInterface::print_runres")))
    }
}

impl Default for KoreInterface {
    fn default() -> Self {
        Self::new()
    }
}

impl Interface for KoreInterface {
    /// Reads `paths`, a K definition then an initial term, as `parse_kore` does,
    /// and loads the K library kompiled with the definition for its hooks.
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
        let value_program = parse_kore(arena, path_definition, path_init)?;
        // The definition's hooks run in the library kompiled next to it
        let dir_kompiled = path_definition.parent().unwrap_or(Path::new("."));
        self.offload.load(dir_kompiled)?;
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

    fn clear(&mut self) {
        self.builtins.init();
    }
}
