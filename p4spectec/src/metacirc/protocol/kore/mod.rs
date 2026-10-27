//! spec-k's protocol: a level written in K above spec-k
//!
//! spec-k, the K semantics written in P4-SpecTec, interprets KORE.
//! A single-level K-in-ST tower enters spec-k through `Krun` directly
//! (`Tower::eval_krun`), and spec-k makes no host calls,
//! so this boundary carries nothing yet.
//! Stacking a level over a K definition, such as ST-in-K-in-ST, would use it:
//! `load` prepares the definition once (`Prepare`, a split of spec-k's `Start`),
//! `lower` runs one request as a K term to the end (`Run`),
//! and each depth keeps the configuration of the K level it runs.
//! That needs `PreparedLevel::Kore` and per-depth K state in `TowerInterpState`;
//! neither exists until stacking is implemented.

use super::{Below, Call, HostCall, Output, Protocol, Script};
use crate::{
    lang::data::{arena::Arena, value::flat::Value},
    metacirc::{Result as MetacircResult, error, tower::PreparedLevel},
    runner::{Eval, ExternError, InterpreterError, SideEffect},
};

/// What a level stacked over spec-k needs, which is not implemented.
const STACKING: &str = "stacking a level over a K definition";

// == Protocol

/// The protocol of spec-k toward the K definition above it.
pub(in crate::metacirc) struct KoreProtocol;

/// The codec state of a boundary above spec-k.
pub(in crate::metacirc) struct Cache;

impl Cache {
    /// Clears the memoized conversions between programs.
    pub(in crate::metacirc) fn clear(&mut self) {}
}

impl Protocol for KoreProtocol {
    type Cache = Cache;

    fn new_cache(&self, _enabled: bool) -> Cache {
        Cache
    }

    /// Prepares the K definition above once, as C (`Prepare`).
    fn load(
        &self,
        _below: &mut dyn Below,
        _above: &PreparedLevel,
    ) -> Result<Value, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented(STACKING)))
    }

    /// Encodes a patched script of the level above; a K definition takes no script.
    fn encode_script(&self, _arena: &mut Arena, _script: Script) -> MetacircResult<Value> {
        Err(error::unimplemented(STACKING))
    }

    /// Runs `call`, a request of the K level above, as a K term under C (`Run`).
    fn lower(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _value_ctx: Value,
        _call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented(STACKING)))
    }

    /// Reads the final configuration of a `Run` as the output of `call`.
    fn raise(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _output: Output,
        _call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        Err(InterpreterError::Fatal(error::unimplemented(STACKING)))
    }

    /// Rejects a host call: spec-k declares no extern relations or functions.
    fn host(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        name: &str,
        _values: &[Value],
    ) -> Result<HostCall, ExternError> {
        Err(error::extern_unsupported(name).into())
    }

    /// Never runs, since `host` accepts no call.
    fn reply(
        &self,
        _arena: &mut Arena,
        _cache: &mut Cache,
        _call: &HostCall,
        _eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError> {
        let error = error::extern_unsupported("a reply above spec-k").into();
        Eval::new(Err(error), SideEffect::Pure)
    }
}
