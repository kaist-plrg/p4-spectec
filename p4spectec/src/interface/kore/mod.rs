//! The interface of spec-k, the K semantics written in P4-SpecTec
//!
//! K-in-ST runs spec-k as the base of a single-level tower over KORE.
//! `KoreInterface::parse_program` reads a K definition (`definition.kore`)
//! and an initial term as spec-k's `kdefinition` and `kore` values,
//! the inputs of its entry relation `Krun`.
//! Builtins are the standard ones plus the K hooks that spec-k delegates
//! (`builtin dec $…`), registered in `hooks`.
//! `KoreInterface::print_runres` prints the `runres` that `Krun` returns
//! as `krun` reports the final configuration.
//! The parser, the printer, and the hooks are slots that the K-in-ST implementation fills;
//! `docs/metacirc/k-in-st.md` says how.

mod hooks;

use crate::{
    diagnostic::{Diagnostic, Severity},
    interface::builtin::call::Builtins,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Interface, InterfaceError},
};
use std::path::PathBuf;

/// Reports a part of the KORE interface without an implementation yet.
fn unimplemented(what: &str) -> InterfaceError {
    InterfaceError(Box::new(
        Diagnostic::new(
            "kore",
            Severity::Error,
            Some("kore/unimplemented".to_owned()),
            format!("{what} is not implemented yet"),
            vec![],
            vec![],
        )
        .into(),
    ))
}

/// The parser and builtins of spec-k.
pub struct KoreInterface {
    /// The standard builtins and the K hooks of `hooks::hooks`.
    builtins: Builtins,
}

impl KoreInterface {
    /// Builds the interface with the K hooks over the standard builtins.
    pub fn new() -> Self {
        Self { builtins: Builtins::with_extensions(hooks::hooks()) }
    }

    /// Prints `runres`, the result of spec-k's `Krun`, as `krun` prints a run.
    ///
    /// `FINAL n term` prints `term`, the final configuration, as KORE text,
    /// so that it compares with `krun`'s output;
    /// `ERROR n term` and `INITFAIL` report how the run failed.
    /// #395 prints them in `interface/kore/unparse.ml` and `krun.ml`.
    pub fn print_runres(&self, _arena: &Arena, _runres: Value) -> Result<String, InterfaceError> {
        Err(unimplemented("KoreInterface::print_runres"))
    }
}

impl Default for KoreInterface {
    fn default() -> Self {
        Self::new()
    }
}

impl Interface for KoreInterface {
    /// Reads `paths`, a K definition then an initial term, both KORE text,
    /// as a tuple of spec-k's `kdefinition` and `kore` values.
    ///
    /// The values are those spec-k's `Krun` takes (`spec-k/7-entry.watsup`);
    /// #395 builds them in `interface/kore/parse.ml` and `load.ml`.
    /// `includes` is unused, since KORE is not preprocessed.
    fn parse_program(
        &mut self,
        _arena: &mut Arena,
        _includes: &[PathBuf],
        _paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        Err(unimplemented("KoreInterface::parse_program"))
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
