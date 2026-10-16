//! K and PLT Redex inputs from Rust, and the host they call back into
//!
//! `spec-meta-k/` and `spec-meta-redex/` consume the meta `script` and `val`
//! values of `interface::al::encode`: `kast::K` prints them as KAST JSON and
//! `sexp::Redex` as s-expressions, both through `Emit` as generic walks over
//! the meta value whose case shapes follow `spec-meta/1-syntax.watsup`.
//! `encode_script` and `encode_p4` produce those values from source paths.
//! `host` serves the builtin and extern requests the specifications make at
//! run time, in the JSON of `json`, and `ffi` exports it from
//! `bin/libp4spectec.so`.

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
    interface::{al::encode::Encode, p4::parse::parse_file},
    lang::{
        common::{notation::atom::Atom, source::Span},
        data::{
            arena::Arena,
            notation::{flat::Mixop, items::Item},
            typ::TypKind,
            value::flat::Value,
        },
        traits::print::Print,
    },
    phrase,
};

pub use error::{Error, Result, render, render_to_string};

// == Emitters

/// An emitter of the two meta values a target consumes.
pub trait Emit {
    /// The printed form of a `script`.
    type Script: fmt::Display;
    /// The printed form of a P4 program's `val`.
    type Program: fmt::Display;

    /// Emits a `script` meta value.
    fn script(arena: &Arena, value: Value) -> Result<Self::Script>;

    /// Emits the `val` meta value of a P4 program.
    fn program(arena: &Arena, value: Value) -> Result<Self::Program>;
}

// == Front half shared by the emitters

/// Lowers specification paths to AL and encodes the script as a meta value.
///
/// Elaboration warnings are returned beside the result, as `algo_with_warnings` does.
pub fn encode_script(arena: &mut Arena, paths: &[PathBuf]) -> (Result<Value>, Vec<Report>) {
    let (result, warnings) = crate::algo_with_warnings(paths);
    let result = result
        .map_err(Error::from)
        .and_then(|spec_al| Ok(spec_al.encode(arena).map_err(|error| error.into_report())?));
    (result, warnings)
}

/// Parses a P4 program and encodes its value as a meta `val`.
pub fn encode_p4(arena: &mut Arena, includes: &[PathBuf], path: &Path) -> Result<Value> {
    let value = parse_file(arena, includes, path).map_err(|error| error.into_report())?;
    Ok(value.encode(arena).map_err(|error| error.into_report())?)
}

// == Meta value inspection shared by the emitters

/// The name of a meta type `VAR id`, which every encoded case and list element carries.
fn typ_name(typ: &TypKind) -> Option<&str> {
    match typ {
        TypKind::Var(id, _) => Some(&id.node),
        _ => None,
    }
}

/// A meta type rendered for diagnostics.
fn typ_text(typ: &TypKind) -> String {
    Print::to_string(&phrase!(node: typ.clone(), span: Span::default()))
}

/// The keyword atom a case notation starts with, if any.
fn leading_keyword(arena: &Arena, mixop: Mixop) -> Option<&str> {
    let mut first = None;
    mixop.visit(&arena.mixop, |item| {
        if first.is_none() {
            first = Some(match item {
                Item::Atom(atom) => match &atom.node {
                    Atom::Keyword(keyword) => Some(keyword.as_str()),
                    _ => None,
                },
                Item::Arg(_) => None,
            });
        }
    });
    first.flatten()
}
