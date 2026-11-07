//! spec-meta-ext's representations of specifications and its runner interface
//!
//! `shared` encodes and decodes the syntax IL and AL have in common,
//! together with the conversion cache and errors;
//! `il` adds the elaborated IL given to the IL-to-AL specification,
//! and `al` adds AL and runtime values.
//! `MetaExtInterface::parse_program` encodes specification paths as an AL or IL script,
//! as the interpreting meta specification accepts;
//! include search paths are unused because scripts are not preprocessed.

use std::path::PathBuf;

use crate::{
    interface::{Lang, builtin::call::Builtins},
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Interface, InterfaceError},
};

use self::shared::encode::Encode;

pub mod al;
pub mod il;
pub mod shared;

/// Provides script parsing in one language and the standard specification builtins.
pub struct MetaExtInterface {
    lang: Lang,
    builtins: Builtins,
}

impl MetaExtInterface {
    /// Assembles the meta interface for scripts in `lang` with its builtin table.
    pub(crate) fn new(lang: Lang, builtins: Builtins) -> Self {
        Self { lang, builtins }
    }
}

impl Interface for MetaExtInterface {
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        _includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        // Encode the ordered specification inputs in the caller's arena,
        // retaining codec diagnostics
        let encoded = match self.lang {
            Lang::AlExt => crate::algo(paths)
                .map_err(InterfaceError::from)?
                .encode(arena),
            Lang::IlExt => crate::elab(paths)
                .map_err(InterfaceError::from)?
                .encode(arena),
            // spec-meta-ext never interprets spec-meta's AL, P4, or KORE directly
            Lang::Al | Lang::P4 | Lang::Kore => {
                return Err(InterfaceError::diagnostic_unconfigured());
            }
        };
        encoded.map_err(|error| InterfaceError::from(error.into_report()))
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
