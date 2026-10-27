//! The K library compiled from spec-meta-k, executing a K base
//!
//! ST-in-K runs spec-meta-k, the AL semantics written in K, as the base.
//! `KRuntime` loads the library `kompile` made of it, once per tower;
//! each reentry depth owns a `KEngineState`, a K thread running
//! spec-meta-k's `serve()` loop, and lends it as a `KEngine`.
//! A call sends a request to that thread as JSON text and waits;
//! a `#ffiCall` the thread makes while serving comes back to the calling thread,
//! which answers it through `UpperMut::host_call` and sends the reply back.
//! Arena values never cross threads, only JSON text does.
//! Each function is a slot that the ST-in-K implementation fills;
//! `docs/metacirc/st-in-k.md` says how.

mod ffi;
mod runtime;

pub(in crate::metacirc) use runtime::KRuntime;

use super::Engine;
use crate::{
    lang::{
        data::{arena::Arena, value::flat::Value},
        il::ast::Typ,
    },
    metacirc::{Result, error, tower::UpperMut},
    runner::{Eval, Extern, InterpreterError, SideEffect},
};

// == Engine

/// spec-meta-k at one depth: the loaded library and that depth's K thread.
pub(in crate::metacirc) struct KEngine<'s> {
    #[expect(dead_code, reason = "a slot for the ST-in-K implementation")]
    runtime: &'s KRuntime,
    #[expect(dead_code, reason = "a slot for the ST-in-K implementation")]
    state: &'s mut KEngineState,
}

impl<'s> KEngine<'s> {
    /// Lends a depth's K thread, running the library of `runtime`.
    pub(in crate::metacirc) fn lend(runtime: &'s KRuntime, state: &'s mut KEngineState) -> Self {
        Self { runtime, state }
    }
}

/// Every call sends JSON text to the depth's K thread and returns JSON text:
/// `MetaKProtocol` makes the requests and reads the responses.
impl Engine for KEngine<'_> {
    /// Runs spec-meta-k's entry `name` on `program`,
    /// the JSON text that `MetaKProtocol::encode_script` made.
    fn call_program<Ext: Extern>(
        &mut self,
        _arena: &mut Arena,
        _upper: &mut UpperMut<'_, Ext>,
        _effects: &mut u64,
        _name: &str,
        _program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        let error = InterpreterError::Fatal(error::unimplemented("KEngine::call_program"));
        Eval::new(Err(error), SideEffect::Pure)
    }

    /// Sends relation `name` with JSON text `values` to `serve()`,
    /// answering each `#ffiCall` of the K thread with
    /// `upper.host_call(arena, "host_eval", &[request])` until the response arrives.
    fn call_rel<Ext: Extern>(
        &mut self,
        _arena: &mut Arena,
        _upper: &mut UpperMut<'_, Ext>,
        _effects: &mut u64,
        _name: &str,
        _values: &[Value],
    ) -> std::result::Result<Vec<Value>, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("KEngine::call_rel")))
    }

    /// Sends function `name` to `serve()`, as `call_rel` does.
    fn call_func<Ext: Extern>(
        &mut self,
        _arena: &mut Arena,
        _upper: &mut UpperMut<'_, Ext>,
        _effects: &mut u64,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> std::result::Result<Value, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("KEngine::call_func")))
    }

    /// Clears what the K thread memoizes within a program, if anything.
    fn clear(&mut self) {}

    /// Returns the K thread to its state before the first program.
    fn reset(&mut self) {}
}

// == Depth state

/// The K thread of one depth, serving requests until the depth is dropped.
///
/// Each thread initializes the K runtime for itself (`kllvm_init`),
/// since the K heap and its collector are per thread.
pub(in crate::metacirc) struct KEngineState;

impl KEngineState {
    /// Starts a depth's K thread on the library of `runtime`.
    pub(in crate::metacirc) fn new(_runtime: &KRuntime) -> Result<Self> {
        Err(error::unimplemented("KEngineState::new"))
    }
}
