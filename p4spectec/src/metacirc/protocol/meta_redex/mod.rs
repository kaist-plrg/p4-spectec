//! spec-meta-redex's protocol: an AL level above a base written in PLT Redex
//!
//! ST-in-Redex runs spec-meta-redex, the AL semantics written in PLT Redex,
//! as the base, on a `RedexEngine`. Every payload crossing this boundary is
//! JSON text held in a `text` value (`make::text`, `get::text`), in the format
//! that `json` and `serve` share with spec-meta-k, except that a specification
//! is the s-expression text of a `spec` term in a JSON string (`sexp`).
//! `load` passes the AL specification above to the base as `{"load": sexp}`,
//! and the base returns C, its handle of the loaded specification;
//! `encode_script` encodes a patched AL script as `{"eval-al": sexp}`;
//! `lower`, `raise`, `host`, and `reply` are `serve`'s.

mod sexp;

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

/// The protocol of spec-meta-redex toward the AL specification above it.
pub(in crate::metacirc) struct MetaRedexProtocol;

impl MetaRedexProtocol {
    /// The engine's call that loads a specification, `load(request)`, returning C.
    pub(in crate::metacirc) const LOAD: &str = serve::LOAD;
    /// The engine's call that serves a request under C, `serve(C, request)`.
    pub(in crate::metacirc) const SERVE: &str = serve::SERVE;
    /// The host call of every request of the process.
    pub(in crate::metacirc) const HOST_EVAL: &str = serve::HOST_EVAL;
}

impl Protocol for MetaRedexProtocol {
    type Cache = Cache;

    fn new_cache(&self, enabled: bool) -> Cache {
        Cache::new(enabled)
    }

    /// Loads the AL specification of `above` into spec-meta-redex, returning C.
    ///
    /// C is the value the base returns for `{"load": sexp}`, kept for the whole program.
    fn load(
        &self,
        below: &mut dyn Below,
        above: &PreparedLevel,
    ) -> Result<Value, InterpreterError> {
        let PreparedLevel::Al { spec_al, .. } = above else {
            let message = "spec-meta-redex loads only a level passed to it as AL";
            return Err(ExternError::diagnostic_message(message).into());
        };
        let request = spec_request(request::LOAD, spec_al);
        let value_request =
            make::text(below.arena(), request, Span::default()).map_err(ExternError::from)?;
        let call = Call::Rel { name: Self::LOAD, values: Values::Borrowed(&[value_request]) };
        let values = below.call(call)?.into_rel();
        Ok(*value::get::one(&values).map_err(ExternError::from)?)
    }

    /// Encodes a patched AL script as `{"eval-al": sexp}`,
    /// which `RedexEngine::call_program` sends to spec-meta-redex's `Eval_AL`.
    fn encode_script(&self, arena: &mut Arena, script: Script) -> MetacircResult<Value> {
        let Script::Al(spec_al) = script else {
            return Err(error::entry_invalid("spec-meta-redex runs only an AL script"));
        };
        let request = spec_request(request::EVAL_AL, &spec_al);
        make::text(arena, request, Span::default())
            .map_err(|cause| error::entry_invalid(cause.to_string()))
    }

    /// Wraps `call` of the AL level above as `serve(C, request)` on spec-meta-redex.
    fn lower(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        value_ctx: Value,
        call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        serve::lower(arena, cache, value_ctx, call)
    }

    /// Unwraps the JSON answer of spec-meta-redex into the output of `call`.
    fn raise(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        output: Output,
        call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        serve::raise(arena, cache, output, call)
    }

    /// Decodes a host request of spec-meta-redex for the level above.
    fn host(
        &self,
        arena: &mut Arena,
        cache: &mut Cache,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError> {
        serve::host(arena, cache, name, values)
    }

    /// Encodes the outcome of a host call as the JSON text answering the request.
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

impl MetaRedexProtocol {
    /// The request `RedexEngine::call_program` sends for `program`.
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

    /// The response telling spec-meta-redex that a host call failed with `report`.
    pub(in crate::metacirc) fn error_response(report: &Report) -> String {
        serve::error_response(report)
    }

    /// The request that ends spec-meta-redex's serve loop.
    pub(in crate::metacirc) fn exit_request() -> String {
        serve::exit_request()
    }
}

/// `{key: sexp}`, a call of the serve loop carrying a specification.
fn spec_request(key: &str, spec_al: &al::Spec) -> String {
    let mut writer = Writer::new();
    writer.object(|writer| {
        writer.key(key);
        writer.text(&sexp::script(spec_al));
    });
    writer.finish()
}
