//! AL program parsing and specification builtins
//!
//! `AlInterface::parse_program` lowers specification paths through `algo`
//! and encodes source AL as a meta `script` in the caller's arena
//! Paths follow the compiler's ordered file and directory loading rules
//! Include search paths are unused because AL input is not preprocessed

use std::path::PathBuf;

use crate::{
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::{Id, Typ},
    },
    metacirc::al::encode,
    runner::{Eval, Extern, Interface, InterfaceError, Interpreter, RunnerContext},
};

use super::builtin::call::Builtins;

/// Provides AL script parsing and the standard specification builtins.
pub struct AlInterface {
    builtins: Builtins,
}

impl AlInterface {
    /// Assembles the AL interface with its configured builtin table.
    pub(crate) fn new(builtins: Builtins) -> Self {
        Self { builtins }
    }
}

impl Interface for AlInterface {
    fn parse_program(
        &mut self,
        arena: &mut Arena,
        _includes: &[PathBuf],
        paths: &[PathBuf],
    ) -> Result<Value, InterfaceError> {
        // Lower ordered specification inputs to source AL
        let spec_al = crate::algo(paths).map_err(InterfaceError::from)?;
        // Encode the program in the caller's arena and retain codec diagnostics
        encode::spec(arena, &spec_al).map_err(|error| InterfaceError::from(error.into_report()))
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
