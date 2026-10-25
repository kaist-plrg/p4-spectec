//! Meta-language representations of specifications and the meta runner interface
//!
//! `shared` encodes and decodes the syntax IL and AL have in common,
//! together with the conversion cache and errors;
//! `al` adds AL and runtime values.
//! `MetaInterface::parse_program` lowers specification paths through `algo`
//! and encodes the AL script in the caller's arena;
//! include search paths are unused because scripts are not preprocessed.

use std::path::PathBuf;

use crate::{
    interface::builtin::call::Builtins,
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    runner::{Eval, Extern, Interface, InterfaceError, Interpreter, RunnerContext},
};

use self::shared::encode::Encode;

pub mod al;
pub mod shared;

/// Provides AL script parsing and the standard specification builtins.
pub struct MetaInterface {
    builtins: Builtins,
}

impl MetaInterface {
    /// Assembles the meta interface with its builtin table.
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
        // Lower ordered specification inputs to source AL
        let spec_al = crate::algo(paths).map_err(InterfaceError::from)?;
        // Encode the program in the caller's arena and retain codec diagnostics
        spec_al
            .encode(arena)
            .map_err(|error| InterfaceError::from(error.into_report()))
    }

    fn call_builtin<Interp, Ext>(
        ctx: &mut RunnerContext<'_, Interp, Self, Ext>,
        id: &Id,
        targs: &[Typ],
        values: &[Value],
    ) -> Eval<Value, InterfaceError>
    where
        Ext: Extern,
        Interp: Interpreter<Self, Ext>,
    {
        let (arena, interface) = ctx.arena_and_interface_mut();
        interface
            .builtins
            .invoke(arena, id, targs, values)
            .map_err(InterfaceError::from)
    }

    fn clear(&mut self) {
        self.builtins.init();
    }
}
