//! spec-meta-k's protocol: an AL level above a base written in K
//!
//! ST-in-K runs spec-meta-k, the AL semantics written in K, as the base,
//! on a `KEngine`. Every payload crossing this boundary is JSON text
//! held in a `text` value (`make::text`, `get::text`),
//! in the format of `spec-meta-k/3-execution/3.04.1-extern-json.k`;
//! `spec` prints an AL specification, and `serve` the rest.
//! `load` passes the AL specification above to the base as `{"load": spec}`,
//! and the base returns C, its handle of the loaded specification;
//! `encode_script` encodes a patched AL script as `{"eval-al": spec}`.
//! `lower` wraps a call of the level above as a `call-rel` or `call-func` request
//! to spec-meta-k's `serve()` loop, the engine's `serve` with C,
//! and `raise` unwraps its answer;
//! `host` and `reply` decode a `#ffiCall` request and encode its outcome.

mod spec;

use super::{
    Below, Call, HostCall, Output, Protocol, Script, Values,
    json::{Cache, Writer},
    serve::{self, request},
};
use crate::{
    diagnostic::Report,
    interface::meta::shared::decode::Decode,
    lang::{
        al::ast as al,
        common::source::Span,
        data::{
            arena::Arena,
            value::flat::{self as value, Value, get, make},
        },
    },
    metacirc::{Result as MetacircResult, error, tower::PreparedLevel},
    runner::{Eval, ExternError, InterpreterError},
};

// == Protocol

/// The protocol of spec-meta-k toward the AL specification above it.
pub(in crate::metacirc) struct MetaKProtocol;

impl MetaKProtocol {
    /// The engine's call that loads a specification, `load(request)`, returning C.
    pub(in crate::metacirc) const LOAD: &str = serve::LOAD;
    /// The engine's call that serves a request under C, `serve(C, request)`.
    pub(in crate::metacirc) const SERVE: &str = serve::SERVE;
    /// The host call of every `#ffiCall` request.
    pub(in crate::metacirc) const HOST_EVAL: &str = serve::HOST_EVAL;
}

impl Protocol for MetaKProtocol {
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

    /// Encodes a patched AL script as `{"eval-al": spec}`,
    /// which `KEngine::call_program` sends to spec-meta-k's `Eval_AL`.
    fn encode_script(&self, arena: &mut Arena, script: Script) -> MetacircResult<Value> {
        let Script::Al(spec_al) = script else {
            return Err(error::entry_invalid("spec-meta-k runs only an AL script"));
        };
        let request = spec_request(request::EVAL_AL, &spec_al);
        make::text(arena, request, Span::default())
            .map_err(|cause| error::entry_invalid(cause.to_string()))
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

impl MetaKProtocol {
    /// The request `KEngine::call_program` sends for `program`.
    ///
    /// `program` is the text `encode_script` made, or, in a single-level tower,
    /// which patches nothing, the script as `interface::meta` encoded it.
    pub(in crate::metacirc) fn program_request(
        arena: &mut Arena,
        program: Value,
    ) -> Result<String, InterpreterError> {
        if let Ok(request) = get::text(arena, &program) {
            return Ok(request.to_owned());
        }
        let spec_al = al::Spec::decode(arena, program)
            .map_err(|error| InterpreterError::Fatal(error.into_report()))?;
        Ok(spec_request(request::EVAL_AL, &spec_al))
    }

    /// The outputs of `Eval_AL` from its answer: the value of `$main()`.
    pub(in crate::metacirc) fn program_outputs(
        arena: &mut Arena,
        value_answer: Value,
        name: &str,
    ) -> Result<Vec<Value>, InterpreterError> {
        serve::program_outputs(arena, value_answer, name)
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
        spec::spec(writer, spec_al);
    });
    writer.finish()
}
