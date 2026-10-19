//! K and PLT Redex inputs from Rust, and the host they call back into
//!
//! `spec-meta-k/` and `spec-meta-redex/` take a specification as a `script`
//! and a P4 program as a `val`. `kast::K` prints them as KAST JSON and
//! `sexp::Redex` as s-expressions, both through `Emit`, walking the AL
//! syntax of `lang::al::ast` and runtime values directly, as the OCaml
//! `kast.ml` and `sexp.ml` did. `lower_script` and `parse_p4` produce the
//! two inputs from source paths. `host` serves the builtin and extern
//! requests the specifications make at run time, in the JSON of `json`, and
//! `ffi` exports it from `bin/libp4spectec.so`.
//!
//! Nothing here depends on the metacircular interpreter or its codec:
//! `interface::al` and `metacirc` are never imported.

mod error;
pub mod ffi;
pub mod host;
pub mod json;
pub mod kast;
pub mod sexp;

use std::{
    fmt,
    path::{Path, PathBuf},
};

use crate::{
    diagnostic::Report,
    interface::p4::parse::parse_file,
    lang::{
        al::ast as al,
        data::{
            arena::Arena,
            notation::{AtomPhrase, flat::Mixop as FlatMixop, items::Item, tree::Mixop},
            value::flat::Value,
        },
        hints::input::{self, InputHint},
    },
};

pub use error::{Error, Result, render, render_to_string};

// == Emitters

/// An emitter of the two inputs a target consumes.
pub trait Emit {
    /// The printed form of a `script`.
    type Script: fmt::Display;
    /// The printed form of a P4 program's `val`.
    type Program: fmt::Display;

    /// Emits a specification as a `script`.
    fn script(spec: &al::Spec) -> Result<Self::Script>;

    /// Emits the `val` of a P4 program.
    fn program(arena: &Arena, value: Value) -> Result<Self::Program>;
}

// == Front half shared by the emitters

/// Lowers specification paths to AL.
///
/// Elaboration warnings are returned beside the result, as `algo_with_warnings` does.
pub fn lower_script(paths: &[PathBuf]) -> (Result<al::Spec>, Vec<Report>) {
    let (result, warnings) = crate::algo_with_warnings(paths);
    (result.map_err(Error::from), warnings)
}

/// Parses a P4 program into the arena.
pub fn parse_p4(arena: &mut Arena, includes: &[PathBuf], path: &Path) -> Result<Value> {
    Ok(parse_file(arena, includes, path).map_err(|error| error.into_report())?)
}

// == Notation and hints shared by the emitters

/// The atoms between the argument positions of tree notation.
///
/// A mixop with n holes gives n + 1 rows, including empty boundary rows.
fn atom_rows(mixop: &Mixop) -> Vec<Vec<&AtomPhrase>> {
    let mut rows = vec![vec![]];
    mixop.visit(|item| match item {
        Item::Arg(_) => rows.push(vec![]),
        Item::Atom(atom) => rows.last_mut().expect("rows start nonempty").push(atom),
    });
    rows
}

/// The atoms between the argument positions of a runtime value's notation.
fn atom_rows_flat(arena: &Arena, mixop: FlatMixop) -> Vec<Vec<&AtomPhrase>> {
    let mut rows = vec![vec![]];
    mixop.visit(&arena.mixop, |item| match item {
        Item::Arg(_) => rows.push(vec![]),
        Item::Atom(atom) => rows.last_mut().expect("rows start nonempty").push(atom),
    });
    rows
}

/// The input and output positions of a relation's notation.
fn split_inputs<'a, T>(hint: &InputHint, items: &'a [T]) -> Result<(Vec<&'a T>, Vec<&'a T>)> {
    Ok(input::split(hint, items.iter().collect())?)
}
