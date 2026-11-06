//! spec-meta-k's protocol: an AL level above a base written in K
//!
//! ST-in-K runs spec-meta-k, the AL semantics written in K, as the base,
//! on a `EngineK`. Every payload crossing this boundary is JSON text
//! held in a `text` value (`make::text`, `get::text`),
//! in the format of `spec-meta-k/3-execution/3.04.1-extern-json.k`;
//! `spec` prints an AL specification, and `serve` the rest.
//! `load` passes the AL specification above to the base as `{"load": spec}`,
//! and the base returns C, its handle of the loaded specification;
//! `encode_entry` encodes the inputs of the base's entry.
//! `lower` wraps a call of the level above as a `call-rel` or `call-func` request
//! to spec-meta-k's `serve()` loop, the engine's `serve` with C,
//! and `raise` unwraps its answer;
//! `host` and `reply` decode a `#ffiCall` request and encode its outcome.

use super::{
    json::{Cache, Writer},
    serve::{self, request},
    spec_json,
};
use crate::metacirc::protocol::{Below, Call, Entered, HostCall, Output, Protocol, Values};
use crate::{
    diagnostic::Report,
    lang::{
        al::ast as al,
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

/// The protocol of spec-meta-k toward the AL specification above it.
pub(in crate::metacirc) struct ProtocolStK;

impl ProtocolStK {
    /// The engine's call that loads a specification, `load(request)`, returning C.
    pub(in crate::metacirc) const LOAD: &str = serve::LOAD;
    /// The engine's call that serves a request under C, `serve(C, request)`.
    pub(in crate::metacirc) const SERVE: &str = serve::SERVE;
    /// The host call of every `#ffiCall` request.
    pub(in crate::metacirc) const HOST_EVAL: &str = serve::HOST_EVAL;
    /// The entry calling a relation, `Eval_AL_rel(spec, id, args)`.
    pub(in crate::metacirc) const ENTRY_REL: &str = serve::ENTRY_REL;
    /// The entry calling a function, `Eval_AL_func(spec, id, args)`.
    pub(in crate::metacirc) const ENTRY_FUNC: &str = serve::ENTRY_FUNC;
}

impl Protocol for ProtocolStK {
    type Cache = Cache;

    fn new_cache(&self, enabled: bool) -> Cache {
        Cache::new(enabled)
    }

    /// Loads the AL specification of `above` into spec-meta-k, returning C.
    ///
    /// C is the value the base returns for `{"load": spec}`, kept for the whole program.
    fn load(
        &self,
        below: &mut dyn Below,
        above: &PreparedLevel,
    ) -> Result<Value, InterpreterError> {
        let PreparedLevel::Al { spec_al, .. } = above else {
            let message = "spec-meta-k loads only a level passed to it as AL";
            return Err(ExternError::diagnostic_message(message).into());
        };
        let request = spec_request(request::LOAD, spec_al);
        let value_request =
            make::text(below.arena(), request, Span::default()).map_err(ExternError::from)?;
        let call = Call::Rel { name: Self::LOAD, values: Values::Borrowed(&[value_request]) };
        let values = below.call(call)?.into_rel();
        Ok(*value::get::one(&values).map_err(ExternError::from)?)
    }

    /// Encodes the inputs of the base's entry as JSON text.
    ///
    /// `Eval_AL_rel` and `Eval_AL_func` take the AL specification `above`,
    /// the name of its entry, and `values`, the entry's inputs:
    /// `[def...]`, `"name"`, and `[val...]`, which `EngineK::call_rel` passes on.
    fn encode_entry(
        &self,
        arena: &mut Arena,
        above: Entered<'_>,
        name: &str,
        values: &[Value],
    ) -> MetacircResult<Vec<Value>> {
        let Entered::Al(spec_al) = above else {
            let message = "spec-meta-k enters only a level passed to it as AL";
            return Err(error::config_invalid(message));
        };
        let mut writer = Writer::new();
        spec_json::spec(&mut writer, spec_al);
        serve::entry_inputs(arena, writer.finish(), name, values)
    }

    /// Wraps `call` of the AL level above as `serve(C, request)` on spec-meta-k.
    fn lower(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        value_ctx: Value,
        call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        serve::lower(arena, cache, value_ctx, call)
    }

    /// Unwraps the JSON answer of spec-meta-k into the output of `call`.
    fn raise(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        output: Output,
        call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        serve::raise(arena, cache, output, call)
    }

    /// Decodes a `#ffiCall` request of spec-meta-k for the level above.
    fn host(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError> {
        serve::host(arena, cache, name, values)
    }

    /// Encodes the outcome of a host call as the JSON text answering `#ffiCall`.
    fn reply(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        _call: &HostCall,
        eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError> {
        serve::reply(arena, cache, eval)
    }
}

// == The engine's side

impl ProtocolStK {
    /// The requests that run spec-meta-k's entry `name` on `values`,
    /// the JSON texts `spec`, `id`, and `args` that `encode_entry` made.
    pub(in crate::metacirc) fn entry_requests(
        arena: &Arena,
        name: &str,
        values: &[Value],
    ) -> Result<[String; 2], InterpreterError> {
        serve::entry_requests(arena, name, values)
    }

    /// The outputs of spec-meta-k's entry `name` from the answer to its call:
    /// the relation's values for `Eval_AL_rel`, the function's value for `Eval_AL_func`.
    pub(in crate::metacirc) fn entry_outputs(
        arena: &mut Arena,
        name: &str,
        value_answer: Value,
    ) -> Result<Vec<Value>, InterpreterError> {
        serve::entry_outputs(arena, name, value_answer)
    }

    /// Checks the answer to a load request, `{"ok": []}`.
    pub(in crate::metacirc) fn load_answer(
        arena: &mut Arena,
        value_answer: Value,
    ) -> Result<(), InterpreterError> {
        serve::load_answer(arena, value_answer)
    }

    /// The response telling spec-meta-k that a host call failed with `report`.
    pub(in crate::metacirc) fn error_response(report: &Report) -> String {
        serve::error_response(report)
    }

    /// The request that ends spec-meta-k's `serve()` loop.
    pub(in crate::metacirc) fn exit_request() -> String {
        serve::exit_request()
    }
}

/// `{key: spec}`, a call of the `serve()` loop carrying a specification.
fn spec_request(key: &str, spec_al: &al::Spec) -> String {
    let mut writer = Writer::new();
    writer.object(|writer| {
        writer.key(key);
        spec_json::spec(writer, spec_al);
    });
    writer.finish()
}
