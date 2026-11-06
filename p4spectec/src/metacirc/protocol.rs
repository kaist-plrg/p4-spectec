//! How one level's specification interprets the level above it
//!
//! The boundary between levels k and k+1 belongs to level k's `Protocol`.
//! Going down, `lower` turns a call of a level-(k+1) relation or function
//! into a call on level k, and `raise` turns its result back;
//! `load` makes C, level k+1's specification loaded into level k,
//! and `encode_entry` encodes the inputs of level k's entry into level k+1,
//! a configured level or the script above the top (`Entered`).
//! Going up, `host` decodes a host call that level k's specification made
//! for level k+1, and `reply` encodes its outcome.
//! `ProtocolKind` stores the protocols, each in the module of the backend
//! whose specification interprets the level above:
//! `ProtocolStSt` is spec-meta's (`st`),
//! `ProtocolStK` is spec-meta-k's, the AL semantics written in K (`k`),
//! `ProtocolStRedex` is spec-meta-redex's, the AL semantics written in PLT Redex (`redex`),
//! and `ProtocolKSt` is spec-k's, the K semantics written in P4-SpecTec (`st`).

pub(super) use super::k::ProtocolStK;
pub(super) use super::redex::ProtocolStRedex;
pub(super) use super::st::{ProtocolKSt, ProtocolStSt};

use super::{
    Result as MetacircResult, k::CacheStK, redex::CacheStRedex, st::CacheKSt, tower::PreparedLevel,
};
use crate::{
    interface::meta::shared::Cache,
    lang::{
        al::ast as al,
        data::{arena::Arena, value::flat::Value},
        il::ast::{self as il, Id, Typ},
    },
    runner::{Eval, ExternError, InterpreterError},
};
use smallvec::SmallVec;
use std::{borrow::Cow, ops::Deref};

// == Protocols

/// The protocol of a level's specification toward the level above it.
///
/// A protocol keeps no state of its own; `cache` is its boundary's,
/// of the kind `new_cache` makes.
pub(in crate::metacirc) trait Protocol {
    /// The codec state of one boundary, shared by every reentry depth.
    type Cache;

    /// Makes the empty codec state of a boundary, memoizing if `enabled`.
    fn new_cache(&self, enabled: bool) -> Self::Cache;

    /// Loads the specification of `above` into the level below, returning C.
    fn load(&self, below: &mut dyn Below, above: &PreparedLevel)
    -> Result<Value, InterpreterError>;

    /// Encodes the inputs of this level's entry relation,
    /// which enters `name` of `above` on `values`,
    /// the inputs of `name` encoded one level down.
    fn encode_entry(
        &self,
        arena: &mut Arena,
        above: Entered<'_>,
        name: &str,
        values: &[Value],
    ) -> MetacircResult<Vec<Value>>;

    /// Turns `call` of the level above into a call on this level, given C.
    fn lower(
        &self,
        arena: &mut Arena,
        cache: &mut Self::Cache,
        value_ctx: Value,
        call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError>;

    /// Turns the output of a lowered call back into the output of `call`.
    fn raise(
        &self,
        arena: &mut Arena,
        cache: &mut Self::Cache,
        output: Output,
        call: &Call<'_>,
    ) -> Result<Output, InterpreterError>;

    /// Decodes host call `name` of this level's specification for the level above.
    fn host(
        &self,
        arena: &mut Arena,
        cache: &mut Self::Cache,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError>;

    /// Encodes the outcome of `call` as the host call's outputs on this level.
    fn reply(
        &self,
        arena: &mut Arena,
        cache: &mut Self::Cache,
        call: &HostCall,
        eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError>;
}

/// A stored protocol.
pub(in crate::metacirc) enum ProtocolKind {
    /// spec-meta interprets the level above as AL.
    StSt(ProtocolStSt),
    /// spec-meta-k, run by a `EngineK`, interprets the level above as AL.
    StK(ProtocolStK),
    /// spec-meta-redex, run by a `EngineRedex`, interprets the level above as AL.
    StRedex(ProtocolStRedex),
    /// spec-k interprets the level above as KORE.
    KSt(ProtocolKSt),
}

/// The codec state of a boundary, of the kind its `ProtocolKind` keeps.
pub(in crate::metacirc) enum CodecCache {
    /// spec-meta's AL encodings and decodings, boxed as the largest.
    StSt(Box<Cache>),
    /// spec-meta-k's JSON codec state.
    StK(CacheStK),
    /// spec-meta-redex's JSON codec state.
    StRedex(CacheStRedex),
    /// spec-k's KORE codec state.
    KSt(CacheKSt),
}

impl CodecCache {
    /// Clears the conversions, keeping their capacity.
    pub(in crate::metacirc) fn clear(&mut self) {
        match self {
            Self::StSt(cache) => cache.clear(),
            Self::StK(cache) => cache.clear(),
            Self::StRedex(cache) => cache.clear(),
            Self::KSt(cache) => cache.clear(),
        }
    }

    /// The response a pure host call of spec-meta-k or spec-meta-redex
    /// gave to `request` before.
    pub(in crate::metacirc) fn host_response(&self, request: &str) -> Option<String> {
        match self {
            Self::StK(cache) => cache.response(request).map(str::to_owned),
            Self::StRedex(cache) => cache.response(request).map(str::to_owned),
            Self::StSt(_) | Self::KSt(_) => None,
        }
    }

    /// Caches the response of a pure host call of spec-meta-k or spec-meta-redex,
    /// if it succeeded.
    pub(in crate::metacirc) fn cache_host_response(&mut self, request: String, response: &str) {
        match self {
            Self::StK(cache) if super::k::is_ok(response) => {
                cache.cache_response(request, response);
            }
            Self::StRedex(cache) if super::redex::is_ok(response) => {
                cache.cache_response(request, response);
            }
            _ => {}
        }
    }
}

impl Protocol for ProtocolKind {
    type Cache = CodecCache;

    fn new_cache(&self, enabled: bool) -> CodecCache {
        match self {
            Self::StSt(protocol) => CodecCache::StSt(Box::new(protocol.new_cache(enabled))),
            Self::StK(protocol) => CodecCache::StK(protocol.new_cache(enabled)),
            Self::StRedex(protocol) => CodecCache::StRedex(protocol.new_cache(enabled)),
            Self::KSt(protocol) => CodecCache::KSt(protocol.new_cache(enabled)),
        }
    }

    fn load(
        &self,
        below: &mut dyn Below,
        above: &PreparedLevel,
    ) -> Result<Value, InterpreterError> {
        match self {
            Self::StSt(protocol) => protocol.load(below, above),
            Self::StK(protocol) => protocol.load(below, above),
            Self::StRedex(protocol) => protocol.load(below, above),
            Self::KSt(protocol) => protocol.load(below, above),
        }
    }

    fn encode_entry(
        &self,
        arena: &mut Arena,
        above: Entered<'_>,
        name: &str,
        values: &[Value],
    ) -> MetacircResult<Vec<Value>> {
        match self {
            Self::StSt(protocol) => protocol.encode_entry(arena, above, name, values),
            Self::StK(protocol) => protocol.encode_entry(arena, above, name, values),
            Self::StRedex(protocol) => protocol.encode_entry(arena, above, name, values),
            Self::KSt(protocol) => protocol.encode_entry(arena, above, name, values),
        }
    }

    fn lower(
        &self,
        arena: &mut Arena,
        cache: &mut CodecCache,
        value_ctx: Value,
        call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        match (self, cache) {
            (Self::StSt(protocol), CodecCache::StSt(cache)) => {
                protocol.lower(arena, cache, value_ctx, call)
            }
            (Self::StK(protocol), CodecCache::StK(cache)) => {
                protocol.lower(arena, cache, value_ctx, call)
            }
            (Self::StRedex(protocol), CodecCache::StRedex(cache)) => {
                protocol.lower(arena, cache, value_ctx, call)
            }
            (Self::KSt(protocol), CodecCache::KSt(cache)) => {
                protocol.lower(arena, cache, value_ctx, call)
            }
            _ => unreachable!("a boundary keeps the cache its protocol made"),
        }
    }

    fn raise(
        &self,
        arena: &mut Arena,
        cache: &mut CodecCache,
        output: Output,
        call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        match (self, cache) {
            (Self::StSt(protocol), CodecCache::StSt(cache)) => {
                protocol.raise(arena, cache, output, call)
            }
            (Self::StK(protocol), CodecCache::StK(cache)) => {
                protocol.raise(arena, cache, output, call)
            }
            (Self::StRedex(protocol), CodecCache::StRedex(cache)) => {
                protocol.raise(arena, cache, output, call)
            }
            (Self::KSt(protocol), CodecCache::KSt(cache)) => {
                protocol.raise(arena, cache, output, call)
            }
            _ => unreachable!("a boundary keeps the cache its protocol made"),
        }
    }

    fn host(
        &self,
        arena: &mut Arena,
        cache: &mut CodecCache,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError> {
        match (self, cache) {
            (Self::StSt(protocol), CodecCache::StSt(cache)) => {
                protocol.host(arena, cache, name, values)
            }
            (Self::StK(protocol), CodecCache::StK(cache)) => {
                protocol.host(arena, cache, name, values)
            }
            (Self::StRedex(protocol), CodecCache::StRedex(cache)) => {
                protocol.host(arena, cache, name, values)
            }
            (Self::KSt(protocol), CodecCache::KSt(cache)) => {
                protocol.host(arena, cache, name, values)
            }
            _ => unreachable!("a boundary keeps the cache its protocol made"),
        }
    }

    fn reply(
        &self,
        arena: &mut Arena,
        cache: &mut CodecCache,
        call: &HostCall,
        eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError> {
        match (self, cache) {
            (Self::StSt(protocol), CodecCache::StSt(cache)) => {
                protocol.reply(arena, cache, call, eval)
            }
            (Self::StK(protocol), CodecCache::StK(cache)) => {
                protocol.reply(arena, cache, call, eval)
            }
            (Self::StRedex(protocol), CodecCache::StRedex(cache)) => {
                protocol.reply(arena, cache, call, eval)
            }
            (Self::KSt(protocol), CodecCache::KSt(cache)) => {
                protocol.reply(arena, cache, call, eval)
            }
            _ => unreachable!("a boundary keeps the cache its protocol made"),
        }
    }
}

/// A specification entered from the level below, in the form that level accepts:
/// a configured level above the base, or the script above the top.
#[derive(Clone, Copy)]
pub(in crate::metacirc) enum Entered<'a> {
    Al(&'a al::Spec),
    Il(&'a il::Spec),
}

/// Calls on the level below a boundary, which `Protocol::load` runs.
pub(in crate::metacirc) trait Below {
    fn arena(&mut self) -> &mut Arena;

    fn call(&mut self, call: Call<'_>) -> Result<Output, InterpreterError>;
}

// == Calls across a boundary

/// A relation or function call on one level.
pub(in crate::metacirc) enum Call<'a> {
    Rel { name: &'a str, values: Values<'a> },
    Func { name: &'a str, targs: Cow<'a, [Typ]>, values: Values<'a> },
}

/// The values of a `Call`: the caller's, or the few a protocol lowers them to.
pub(in crate::metacirc) enum Values<'a> {
    Borrowed(&'a [Value]),
    Lowered(SmallVec<[Value; 4]>),
}

impl Call<'_> {
    /// Returns the name of the called relation or function.
    pub(in crate::metacirc) fn name(&self) -> &str {
        match self {
            Self::Rel { name, .. } | Self::Func { name, .. } => name,
        }
    }
}

impl Deref for Values<'_> {
    type Target = [Value];

    fn deref(&self) -> &[Value] {
        match self {
            Self::Borrowed(values) => values,
            Self::Lowered(values) => values,
        }
    }
}

/// The outcome of a `Call`, of the same kind.
pub(in crate::metacirc) enum Output {
    Rel(Vec<Value>),
    Func(Value),
}

impl Output {
    /// Returns the outputs of a relation call.
    pub(in crate::metacirc) fn into_rel(self) -> Vec<Value> {
        match self {
            Self::Rel(values) => values,
            Self::Func(_) => unreachable!("a relation call outputs a list"),
        }
    }

    /// Returns the result of a function call.
    pub(in crate::metacirc) fn into_func(self) -> Value {
        match self {
            Self::Func(value) => value,
            Self::Rel(_) => unreachable!("a function call outputs one value"),
        }
    }
}

/// A host call that a level's specification made, decoded for the level above.
pub(in crate::metacirc) enum HostCall {
    Builtin { id: Id, targs: Vec<Typ>, values: Vec<Value> },
    ExternRel { id: Id, values: Vec<Value> },
    ExternFunc { id: Id, targs: Vec<Typ>, values: Vec<Value> },
}
