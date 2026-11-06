//! spec-meta-redex's protocol: an AL level above a base written in PLT Redex
//!
//! ST-in-Redex runs spec-meta-redex, the AL semantics written in PLT Redex,
//! as the base, on a `EngineRedex`. Every payload crossing this boundary is
//! s-expression text held in a `text` value (`make::text`, `get::text`),
//! in the format of `serve`: the Redex terms themselves, which Racket reads
//! with `read`, printed by `sexp` and read back by `read`.
//! `load` passes the AL specification above to the base as `(load spec)`,
//! and the base returns C, its handle of the loaded specification;
//! `encode_entry` encodes the inputs of the base's entry;
//! `lower`, `raise`, `host`, and `reply` are `serve`'s.

use super::{cache::Cache, serve, sexp};
use crate::metacirc::protocol::{Below, Call, Entered, HostCall, Output, Protocol, Values};
use crate::{
    lang::{
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{self as value, Value, make},
        },
    },
    metacirc::{Result as MetacircResult, error, tower::PreparedLevel},
    runner::{Eval, ExternError, InterpreterError},
};

// == Protocol

/// The protocol of spec-meta-redex toward the AL specification above it.
pub(in crate::metacirc) struct ProtocolStRedex {
    /// The codec state of this boundary, shared by every reentry depth.
    cache: Cache,
}

impl ProtocolStRedex {
    /// The engine's call that loads a specification, `load(request)`, returning C.
    pub(in crate::metacirc) const LOAD: &str = serve::LOAD;
    /// The engine's call that serves a request under C, `serve(C, request)`.
    pub(in crate::metacirc) const SERVE: &str = serve::SERVE;
    /// The host call of every request of the process.
    pub(in crate::metacirc) const HOST_EVAL: &str = serve::HOST_EVAL;
    /// The entry calling a relation, `Eval_AL_rel(spec, id, args)`.
    pub(in crate::metacirc) const ENTRY_REL: &str = serve::ENTRY_REL;
    /// The entry calling a function, `Eval_AL_func(spec, id, args)`.
    pub(in crate::metacirc) const ENTRY_FUNC: &str = serve::ENTRY_FUNC;
}

impl Protocol for ProtocolStRedex {
    fn new(cache: bool) -> Self {
        Self { cache: Cache::new(cache) }
    }

    /// Loads the AL specification of `above` into spec-meta-redex, returning C.
    ///
    /// C is the value the base returns for `(load spec)`, kept for the whole program.
    fn load(below: &mut dyn Below, above: &PreparedLevel) -> Result<Value, InterpreterError> {
        let PreparedLevel::Al { spec_al, .. } = above else {
            let message = "spec-meta-redex loads only a level passed to it as AL";
            return Err(ExternError::diagnostic_message(message).into());
        };
        let request = serve::load_request(&sexp::script(spec_al));
        let value_request =
            make::text(below.arena(), request, Span::default()).map_err(ExternError::from)?;
        let call = Call::Rel { name: Self::LOAD, values: Values::Borrowed(&[value_request]) };
        let values = below.call(call)?.into_rel();
        Ok(*value::get::one(&values).map_err(ExternError::from)?)
    }

    /// Encodes the inputs of the base's entry as s-expression text.
    ///
    /// `Eval_AL_rel` and `Eval_AL_func` take the AL specification `above`
    /// as the s-expression text of a `spec` term, the name of its entry,
    /// and `values`, the entry's inputs: `spec`, `"name"`, and `(val ...)`,
    /// which `EngineRedex::call_rel` passes on.
    fn encode_entry(
        arena: &mut Arena,
        above: Entered<'_>,
        name: &str,
        values: &[Value],
    ) -> MetacircResult<Vec<Value>> {
        let Entered::Al(spec_al) = above else {
            let message = "spec-meta-redex enters only a level passed to it as AL";
            return Err(error::config_invalid(message));
        };
        serve::entry_inputs(arena, sexp::script(spec_al), name, values)
    }

    /// Wraps `call` of the AL level above as `serve(C, request)` on spec-meta-redex.
    fn lower(
        &mut self,
        arena: &mut Arena,
        value_ctx: Value,
        call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        serve::lower(arena, &mut self.cache, value_ctx, call)
    }

    /// Unwraps the answer of spec-meta-redex into the output of `call`.
    fn raise(
        &mut self,
        arena: &mut Arena,
        output: Output,
        call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        serve::raise(arena, &mut self.cache, output, call)
    }

    /// Decodes a host request of spec-meta-redex for the level above.
    fn host(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError> {
        serve::host(arena, &mut self.cache, name, values)
    }

    /// Encodes the outcome of a host call as the text answering the request.
    fn reply(
        &mut self,
        arena: &mut Arena,
        _call: &HostCall,
        eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError> {
        serve::reply(arena, &mut self.cache, eval)
    }

    fn clear(&mut self) {
        self.cache.clear();
    }
}

// == Memoized host responses

impl ProtocolStRedex {
    /// The response a pure host call gave to `request` before.
    pub(in crate::metacirc) fn host_response(&self, request: &str) -> Option<String> {
        self.cache.response(request).map(str::to_owned)
    }

    /// Caches the response of a pure host call, if it succeeded.
    pub(in crate::metacirc) fn cache_host_response(&mut self, request: String, response: &str) {
        if serve::is_ok(response) {
            self.cache.cache_response(request, response);
        }
    }
}

// == The engine's side

impl ProtocolStRedex {
    /// The calls that run spec-meta-redex's entry `name` on `values`,
    /// the texts `spec`, `"id"`, and `(val ...)` that `encode_entry` made.
    pub(in crate::metacirc) fn entry_requests(
        arena: &Arena,
        name: &str,
        values: &[Value],
    ) -> Result<[String; 2], InterpreterError> {
        serve::entry_requests(arena, name, values)
    }

    /// The outputs of spec-meta-redex's entry `name` from the answer to its call:
    /// the relation's values for `Eval_AL_rel`, the function's value for `Eval_AL_func`.
    pub(in crate::metacirc) fn entry_outputs(
        arena: &mut Arena,
        name: &str,
        answer: &str,
    ) -> Result<Vec<Value>, InterpreterError> {
        serve::entry_outputs(arena, name, answer)
    }

    /// Checks the answer to a load, `(OK ())`.
    pub(in crate::metacirc) fn load_answer(
        arena: &mut Arena,
        answer: &str,
    ) -> Result<(), InterpreterError> {
        serve::load_answer(arena, answer)
    }

    /// The response telling spec-meta-redex that a host call failed;
    /// the engine reports why.
    pub(in crate::metacirc) const ERROR_RESPONSE: &str = serve::ERROR;

    /// The answer of spec-meta-redex before its first call.
    pub(in crate::metacirc) const READY: &str = serve::READY;

    /// The call that ends spec-meta-redex's serve loop.
    pub(in crate::metacirc) const EXIT: &str = serve::EXIT;
}
