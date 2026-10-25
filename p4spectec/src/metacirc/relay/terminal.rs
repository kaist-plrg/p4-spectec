//! Builtins of the AL or IL program above a tower's top level
//!
//! The top meta level interprets the program itself,
//! so only the program's builtins reach the host side.

use super::{codec, unsupported};
use crate::{
    interface::meta::{MetaInterface, shared::Cache},
    lang::data::{arena::Arena, value::flat::Value},
    runner::{Eval, ExternError, Interface, SideEffect},
};

/// Builtin calls of the program, answered by the program's interface.
pub(in crate::metacirc) struct TerminalExtern {
    pub cache: Box<Cache>,
    pub interface_program: Box<MetaInterface>,
}

impl TerminalExtern {
    /// Answers a builtin call of the program and rejects other meta relations.
    pub(super) fn eval_rel(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Eval<Vec<Value>, ExternError> {
        // The program exposes only its builtins to the meta interpreter
        if name != "Call_builtin_func" {
            return unsupported(name);
        }
        let (id, targs, values) = match codec::decode::func_args(arena, &mut self.cache, values) {
            Ok(args) => args,
            Err(error) => return Eval::new(Err(error), SideEffect::Pure),
        };
        // Answer the builtin through the program's interface
        let eval = self
            .interface_program
            .call_builtin(arena, &id, &targs, &values)
            .map_err(Into::into);
        codec::encode::func_result(arena, &mut self.cache, eval)
    }

    pub(super) fn clear(&mut self) {
        self.cache.clear();
        self.interface_program.clear();
    }
}
