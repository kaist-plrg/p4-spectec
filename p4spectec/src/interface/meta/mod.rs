//! spec-meta's representations of specifications and its runner interface
//!
//! `shared` encodes and decodes the syntax of `spec-meta/1-syntax/1.0-common`,
//! together with the conversion cache and errors;
//! `al` adds AL and runtime values.
//! `MetaInterface::parse_program` encodes specification paths as an AL script;
//! include search paths are unused because scripts are not preprocessed.

use std::path::PathBuf;

use crate::{
    interface::builtin::call::Builtins,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Interface, InterfaceError},
};

use self::shared::encode::Encode;

pub mod al;
pub mod shared;

/// Provides AL script parsing and the standard specification builtins.
pub struct MetaInterface {
    builtins: Builtins,
}

impl MetaInterface {
    /// Assembles the meta interface for AL scripts with its builtin table.
    pub(crate) fn new(builtins: Builtins) -> Self {
        Self { builtins }
    }
}

impl Interface for MetaInterface {
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        _includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        // Encode the ordered specification inputs in the caller's arena,
        // retaining codec diagnostics
        crate::algo(paths)
            .map_err(InterfaceError::from)?
            .encode(arena)
            .map_err(|error| InterfaceError::from(error.into_report()))
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
