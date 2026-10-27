//! Meta-language representations of specifications and the meta runner interface
//!
//! `shared` encodes and decodes the syntax IL and AL have in common,
//! together with the conversion cache and errors;
//! `il` adds the elaborated IL given to the IL-to-AL specification,
//! and `al` adds AL and runtime values.
//! `MetaInterface::parse_program` encodes specification paths as an AL or IL script,
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
pub struct MetaInterface {
    lang: Lang,
    builtins: Builtins,
}

impl MetaInterface {
    /// Assembles the meta interface for scripts in `lang` with its builtin table.
    pub(crate) fn new(lang: Lang, builtins: Builtins) -> Self {
        Self { lang, builtins }
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
        let encoded = match self.lang {
            Lang::Al => crate::algo(paths)
                .map_err(InterfaceError::from)?
                .encode(arena),
            Lang::Il => crate::elab(paths)
                .map_err(InterfaceError::from)?
                .encode(arena),
            // A meta specification never interprets P4 or KORE directly
            Lang::P4 | Lang::Kore => return Err(InterfaceError::diagnostic_unconfigured()),
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
