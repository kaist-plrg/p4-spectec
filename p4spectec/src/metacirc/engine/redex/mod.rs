//! A `racket` process running spec-meta-redex, executing a Redex base
//!
//! ST-in-Redex runs spec-meta-redex, the AL semantics written in PLT Redex,
//! as the base. `RedexRuntime` runs its entry module in one `racket` process
//! per tower, serving every reentry depth; a depth's `RedexEngineState`
//! is lent as a `RedexEngine`.
//! A call writes a request to the process as one line of JSON text and reads
//! lines until its answer; a host request the process makes while serving
//! is answered through `UpperMut::host_call`, and the response written back.
//! Arena values never leave this process, only JSON text does.
//! `MetaRedexProtocol` makes the requests and reads the answers.
//! Each function is a slot that the ST-in-Redex implementation fills.

mod runtime;

pub(in crate::metacirc) use runtime::RedexRuntime;

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

/// spec-meta-redex at one depth: the tower's process and that depth's state.
pub(in crate::metacirc) struct RedexEngine<'s> {
    #[expect(dead_code, reason = "a slot for the ST-in-Redex implementation")]
    runtime: &'s RedexRuntime,
    #[expect(dead_code, reason = "a slot for the ST-in-Redex implementation")]
    state: &'s mut RedexEngineState,
}

impl<'s> RedexEngine<'s> {
    /// Lends a depth's state, running on the process of `runtime`.
    pub(in crate::metacirc) fn lend(
        runtime: &'s RedexRuntime,
        state: &'s mut RedexEngineState,
    ) -> Self {
        Self { runtime, state }
    }
}

/// Every call sends JSON text to the process and returns JSON text:
/// `MetaRedexProtocol` makes the requests and reads the responses.
impl Engine for RedexEngine<'_> {
    /// Runs spec-meta-redex's entry `name` on `program`,
    /// the JSON text that `MetaRedexProtocol::encode_script` made.
    fn call_program<Ext: Extern>(
        &mut self,
        _arena: &mut Arena,
        _upper: &mut UpperMut<'_, Ext>,
        _effects: &mut u64,
        _name: &str,
        _program: Value,
    ) -> Eval<Vec<Value>, InterpreterError> {
        let error = InterpreterError::Fatal(error::unimplemented("RedexEngine::call_program"));
        Eval::new(Err(error), SideEffect::Pure)
    }

    /// Sends relation `name` with JSON text `values` to the serve loop,
    /// answering each host request of the process through `upper.host_call`
    /// until the answer arrives.
    ///
    /// `load(request)` loads a specification and returns C, its key in the runtime;
    /// `serve(C, request)` sends a request under C and returns the answer.
    fn call_rel<Ext: Extern>(
        &mut self,
        _arena: &mut Arena,
        _upper: &mut UpperMut<'_, Ext>,
        _effects: &mut u64,
        _name: &str,
        _values: &[Value],
    ) -> std::result::Result<Vec<Value>, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("RedexEngine::call_rel")))
    }

    /// Sends function `name` to the serve loop, as `call_rel` does.
    fn call_func<Ext: Extern>(
        &mut self,
        _arena: &mut Arena,
        _upper: &mut UpperMut<'_, Ext>,
        _effects: &mut u64,
        _name: &str,
        _targs: &[Typ],
        _values: &[Value],
    ) -> std::result::Result<Value, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("RedexEngine::call_func")))
    }

    /// The process memoizes nothing that a program must clear.
    fn clear(&mut self) {}

    /// Restarts a stopped process and forgets what it loaded.
    fn reset(&mut self) {}
}

// == Depth state

/// The state of one depth; the process belongs to the runtime.
pub(in crate::metacirc) struct RedexEngineState;

impl RedexEngineState {
    /// Makes a depth's state on the process of `runtime`.
    pub(in crate::metacirc) fn new(_runtime: &RedexRuntime) -> Result<Self> {
        Err(error::unimplemented("RedexEngineState::new"))
    }
}
