//! spec-meta-redex's protocol: an AL level above a base written in PLT Redex
//!
//! ST-in-Redex runs spec-meta-redex, the AL semantics written in PLT Redex,
//! as the base, on a `RedexEngine`. Every payload crossing this boundary is
//! JSON text held in a `text` value (`make::text`, `get::text`), in the format
//! that `json` and `serve` share with spec-meta-k, except that a specification
//! is the s-expression text of a `spec` term in a JSON string.
//! `load` passes the AL specification above to the base, making C;
//! `encode_script` encodes a patched AL script as `{"eval-al": spec}`;
//! `lower`, `raise`, `host`, and `reply` are `serve`'s.
//! Each function is a slot that the ST-in-Redex implementation fills.

use super::{Below, Call, HostCall, Output, Protocol, Script, json::Cache};
use crate::{
    lang::data::{arena::Arena, value::flat::Value},
    metacirc::{Result as MetacircResult, error, tower::PreparedLevel},
    runner::{Eval, ExternError, InterpreterError, SideEffect},
};

// == Protocol

/// The protocol of spec-meta-redex toward the AL specification above it.
pub(in crate::metacirc) struct MetaRedexProtocol;

impl Protocol for MetaRedexProtocol {
    type Cache = Cache;

    fn new_cache(&self, enabled: bool) -> Cache {
        Cache::new(enabled)
    }

    /// Loads the AL specification of `above` into spec-meta-redex, returning C.
    ///
    /// C is the value the base returns for `{"load": spec}`, kept for the whole program.
    fn load(
        &self,
        _below: &mut dyn Below,
        _above: &PreparedLevel,
    ) -> Result<Value, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("MetaRedexProtocol::load")))
    }

    /// Encodes a patched AL script as `{"eval-al": spec}`,
    /// which `RedexEngine::call_program` sends to spec-meta-redex's `Eval_AL`.
    fn encode_script(&self, _arena: &mut Arena, _script: Script) -> MetacircResult<Value> {
        Err(error::unimplemented("MetaRedexProtocol::encode_script"))
    }

    /// Wraps `call` of the AL level above as `serve(C, request)` on spec-meta-redex.
    fn lower(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _value_ctx: Value,
        _call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("MetaRedexProtocol::lower")))
    }

    /// Unwraps the JSON answer of spec-meta-redex into the output of `call`.
    fn raise(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _output: Output,
        _call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented("MetaRedexProtocol::raise")))
    }

    /// Decodes a host request of spec-meta-redex for the level above.
    fn host(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _name: &str,
        _values: &[Value],
    ) -> Result<HostCall, ExternError> {
        Err(error::unimplemented("MetaRedexProtocol::host").into())
    }

    /// Encodes the outcome of a host call as the JSON text answering the request.
    fn reply(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _call: &HostCall,
        _eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError> {
        let error = error::unimplemented("MetaRedexProtocol::reply").into();
        Eval::new(Err(error), SideEffect::Pure)
    }
}
