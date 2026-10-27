//! How one level's specification interprets the level above it
//!
//! The boundary between levels k and k+1 belongs to level k's `Protocol`.
//! Going down, `lower` turns a call of a level-(k+1) relation or function
//! into a call on level k, and `raise` turns its result back;
//! `load` makes C, level k+1's specification loaded into level k,
//! and `encode_script` encodes level k+1 patched with a program entry.
//! Going up, `host` decodes a host call that level k's specification made
//! for level k+1, and `reply` encodes its outcome.
//! `ProtocolKind` stores the protocols; `meta` is spec-meta's,
//! `meta_k` is spec-meta-k's, the AL semantics written in K,
//! and `kore` is spec-k's, the K semantics written in P4-SpecTec.

mod kore;
mod meta;
mod meta_k;

pub(super) use kore::KoreProtocol;
pub(super) use meta::MetaProtocol;
pub(super) use meta_k::MetaKProtocol;

use super::{Result as MetacircResult, tower::PreparedLevel};
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

    /// Encodes `script`, the level above patched with a program entry,
    /// as the input of this level's entry relation.
    fn encode_script(&self, arena: &mut Arena, script: Script) -> MetacircResult<Value>;

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
    Meta(MetaProtocol),
    /// spec-meta-k, run by a `KEngine`, interprets the level above as AL.
    MetaK(MetaKProtocol),
    /// spec-k interprets the level above as KORE.
    Kore(KoreProtocol),
}

/// The codec state of a boundary, of the kind its `ProtocolKind` keeps.
pub(in crate::metacirc) enum CodecCache {
    /// spec-meta's AL encodings and decodings.
    Meta(Cache),
    /// spec-meta-k's JSON codec state.
    MetaK(meta_k::Cache),
    /// spec-k's KORE codec state.
    Kore(kore::Cache),
}

impl CodecCache {
    /// Clears the conversions, keeping their capacity.
    pub(in crate::metacirc) fn clear(&mut self) {
        match self {
            Self::Meta(cache) => cache.clear(),
            Self::MetaK(cache) => cache.clear(),
            Self::Kore(cache) => cache.clear(),
        }
    }
}

impl Protocol for ProtocolKind {
    type Cache = CodecCache;

    fn new_cache(&self, enabled: bool) -> CodecCache {
        match self {
            Self::Meta(protocol) => CodecCache::Meta(protocol.new_cache(enabled)),
            Self::MetaK(protocol) => CodecCache::MetaK(protocol.new_cache(enabled)),
            Self::Kore(protocol) => CodecCache::Kore(protocol.new_cache(enabled)),
        }
    }

    fn load(
        &self,
        below: &mut dyn Below,
        above: &PreparedLevel,
    ) -> Result<Value, InterpreterError> {
        match self {
            Self::Meta(protocol) => protocol.load(below, above),
            Self::MetaK(protocol) => protocol.load(below, above),
            Self::Kore(protocol) => protocol.load(below, above),
        }
    }

    fn encode_script(&self, arena: &mut Arena, script: Script) -> MetacircResult<Value> {
        match self {
            Self::Meta(protocol) => protocol.encode_script(arena, script),
            Self::MetaK(protocol) => protocol.encode_script(arena, script),
            Self::Kore(protocol) => protocol.encode_script(arena, script),
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
            (Self::Meta(protocol), CodecCache::Meta(cache)) => {
                protocol.lower(arena, cache, value_ctx, call)
            }
            (Self::MetaK(protocol), CodecCache::MetaK(cache)) => {
                protocol.lower(arena, cache, value_ctx, call)
            }
            (Self::Kore(protocol), CodecCache::Kore(cache)) => {
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
            (Self::Meta(protocol), CodecCache::Meta(cache)) => {
                protocol.raise(arena, cache, output, call)
            }
            (Self::MetaK(protocol), CodecCache::MetaK(cache)) => {
                protocol.raise(arena, cache, output, call)
            }
            (Self::Kore(protocol), CodecCache::Kore(cache)) => {
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
            (Self::Meta(protocol), CodecCache::Meta(cache)) => {
                protocol.host(arena, cache, name, values)
            }
            (Self::MetaK(protocol), CodecCache::MetaK(cache)) => {
                protocol.host(arena, cache, name, values)
            }
            (Self::Kore(protocol), CodecCache::Kore(cache)) => {
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
            (Self::Meta(protocol), CodecCache::Meta(cache)) => {
                protocol.reply(arena, cache, call, eval)
            }
            (Self::MetaK(protocol), CodecCache::MetaK(cache)) => {
                protocol.reply(arena, cache, call, eval)
            }
            (Self::Kore(protocol), CodecCache::Kore(cache)) => {
                protocol.reply(arena, cache, call, eval)
            }
            _ => unreachable!("a boundary keeps the cache its protocol made"),
        }
    }
}

/// A level's specification patched with a program entry, in the form it was prepared in.
pub(in crate::metacirc) enum Script {
    Al(al::Spec),
    Il(il::Spec),
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
