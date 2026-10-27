//! spec-meta-k's protocol: an AL level above a base written in K
//!
//! ST-in-K runs spec-meta-k, the AL semantics written in K, as the base,
//! on a `KEngine`. Every payload crossing this boundary is JSON text
//! held in a `text` value (`make::text`, `get::text`),
//! in the format of `spec-meta-k/3-execution/3.04.1-extern-json.k`.
//! `load` passes the AL specification above to the base, making C,
//! and `encode_script` encodes a patched AL script for the base's entry;
//! `lower` and `raise` wrap a call of the level above as a request
//! to spec-meta-k's `serve()` loop and unwrap its response;
//! `host` and `reply` decode a `#ffiCall` request and encode its outcome.
//! Each function is a slot that the ST-in-K implementation fills;
//! `docs/metacirc/st-in-k.md` says how.

use super::{Below, Call, HostCall, Output, Protocol, Script};
use crate::{
    lang::data::{arena::Arena, value::flat::Value},
    metacirc::{Result as MetacircResult, error, tower::PreparedLevel},
    runner::{Eval, ExternError, InterpreterError, SideEffect},
};

// == Protocol

/// The protocol of spec-meta-k toward the AL specification above it.
pub(in crate::metacirc) struct MetaKProtocol;

/// The codec state of a boundary above spec-meta-k.
///
/// Fill it with whatever the JSON codec memoizes, if anything;
/// every reentry depth shares it, and `clear` runs between programs.
pub(in crate::metacirc) struct Cache;

impl Cache {
    /// Clears the memoized conversions between programs.
    pub(in crate::metacirc) fn clear(&mut self) {}
}

impl Protocol for MetaKProtocol {
    type Cache = Cache;

    fn new_cache(&self, _enabled: bool) -> Cache {
        Cache
    }

    /// Loads the AL specification of `above` into spec-meta-k, returning C.
    ///
    /// `above` is `PreparedLevel::Al`; encode its `spec_al` as the JSON
    /// that spec-meta-k reads (the AL syntax of `metacross/kast.rs` in #387),
    /// and pass it to the base with `below.call`.
    /// C is the value the base returns for it, kept for the whole program.
    fn load(
        &self,
        _below: &mut dyn Below,
        _above: &PreparedLevel,
    ) -> Result<Value, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("MetaKProtocol::load")))
    }

    /// Encodes a patched AL script as the JSON text of the base's entry input.
    ///
    /// `script` is `Script::Al`, the specification above with a synthetic `main`,
    /// as `KEngine::call_program` passes it to spec-meta-k's `Eval_AL`.
    fn encode_script(&self, _arena: &mut Arena, _script: Script) -> MetacircResult<Value> {
        Err(error::unimplemented("MetaKProtocol::encode_script"))
    }

    /// Wraps `call` of the AL level above as a request to spec-meta-k under C.
    ///
    /// The result is the call that `KEngine` sends to `serve()`:
    /// a relation or function of spec-meta-k with JSON text values.
    fn lower(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _value_ctx: Value,
        _call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("MetaKProtocol::lower")))
    }

    /// Unwraps the JSON response of spec-meta-k into the output of `call`.
    ///
    /// An OK response gives the outputs; UNMATCH is `InterpreterError::Mismatch`,
    /// and ERROR is `InterpreterError::Fatal`, as `MetaProtocol::raise` does.
    fn raise(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _output: Output,
        _call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("MetaKProtocol::raise")))
    }

    /// Decodes a `#ffiCall` request of spec-meta-k for the level above.
    ///
    /// `KEngine` passes each request it receives from `host_eval` as
    /// `upper.host_call(arena, "host_eval", &[request])`,
    /// so `name` is `host_eval` and `values` is the one JSON text request.
    fn host(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _name: &str,
        _values: &[Value],
    ) -> Result<HostCall, ExternError> {
        Err(error::unimplemented("MetaKProtocol::host").into())
    }

    /// Encodes the outcome of a host call as the JSON text answering `#ffiCall`,
    /// the one value that `KEngine` passes back to `host_eval`.
    fn reply(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _call: &HostCall,
        _eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError> {
        let error = error::unimplemented("MetaKProtocol::reply").into();
        Eval::new(Err(error), SideEffect::Pure)
    }
}
