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

use super::{Result as MetacircResult, tower::PreparedLevel};
use crate::{
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
/// A protocol owns the codec state of its boundary, shared by every reentry
/// depth. Loading C and encoding the entry need no state, so they are
/// associated functions: loading runs on the level below, while the tower
/// that stores this protocol is borrowed for that call.
pub(in crate::metacirc) trait Protocol {
    /// Makes the protocol of a boundary, memoizing its conversions if `cache`.
    fn new(cache: bool) -> Self;

    /// Loads the specification of `above` into the level below, returning C.
    fn load(below: &mut dyn Below, above: &PreparedLevel) -> Result<Value, InterpreterError>;

    /// Encodes the inputs of this level's entry relation,
    /// which enters `name` of `above` on `values`,
    /// the inputs of `name` encoded one level down.
    fn encode_entry(
        arena: &mut Arena,
        above: Entered<'_>,
        name: &str,
        values: &[Value],
    ) -> MetacircResult<Vec<Value>>;

    /// Turns `call` of the level above into a call on this level, given C.
    fn lower(
        &mut self,
        arena: &mut Arena,
        value_ctx: Value,
        call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError>;

    /// Turns the output of a lowered call back into the output of `call`.
    fn raise(
        &mut self,
        arena: &mut Arena,
        output: Output,
        call: &Call<'_>,
    ) -> Result<Output, InterpreterError>;

    /// Decodes host call `name` of this level's specification for the level above.
    fn host(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError>;

    /// Encodes the outcome of `call` as the host call's outputs on this level.
    fn reply(
        &mut self,
        arena: &mut Arena,
        call: &HostCall,
        eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError>;

    /// Clears the codec state between programs, keeping its capacity.
    fn clear(&mut self);
}

/// `Protocol::load` of a stored protocol, taken out before the tower is lent to it.
pub(in crate::metacirc) type Load =
    fn(&mut dyn Below, &PreparedLevel) -> Result<Value, InterpreterError>;

/// The protocol of one boundary, with its codec state.
pub(in crate::metacirc) enum ProtocolKind {
    /// spec-meta interprets the level above as AL or IL; boxed as the largest.
    StSt(Box<ProtocolStSt>),
    /// spec-meta-k, run by an `EngineK`, interprets the level above as AL.
    StK(ProtocolStK),
    /// spec-meta-redex, run by an `EngineRedex`, interprets the level above as AL.
    StRedex(ProtocolStRedex),
    /// spec-k interprets the level above as KORE.
    KSt(ProtocolKSt),
}

impl ProtocolKind {
    /// Returns how this boundary loads C, without borrowing it.
    pub(in crate::metacirc) fn load(&self) -> Load {
        match self {
            Self::StSt(_) => ProtocolStSt::load,
            Self::StK(_) => ProtocolStK::load,
            Self::StRedex(_) => ProtocolStRedex::load,
            Self::KSt(_) => ProtocolKSt::load,
        }
    }

    /// Encodes the inputs of the entry below this boundary (`Protocol::encode_entry`).
    pub(in crate::metacirc) fn encode_entry(
        &self,
        arena: &mut Arena,
        above: Entered<'_>,
        name: &str,
        values: &[Value],
    ) -> MetacircResult<Vec<Value>> {
        match self {
            Self::StSt(_) => ProtocolStSt::encode_entry(arena, above, name, values),
            Self::StK(_) => ProtocolStK::encode_entry(arena, above, name, values),
            Self::StRedex(_) => ProtocolStRedex::encode_entry(arena, above, name, values),
            Self::KSt(_) => ProtocolKSt::encode_entry(arena, above, name, values),
        }
    }

    pub(in crate::metacirc) fn lower(
        &mut self,
        arena: &mut Arena,
        value_ctx: Value,
        call: &Call<'_>,
    ) -> Result<Call<'static>, InterpreterError> {
        match self {
            Self::StSt(protocol) => protocol.lower(arena, value_ctx, call),
            Self::StK(protocol) => protocol.lower(arena, value_ctx, call),
            Self::StRedex(protocol) => protocol.lower(arena, value_ctx, call),
            Self::KSt(protocol) => protocol.lower(arena, value_ctx, call),
        }
    }

    pub(in crate::metacirc) fn raise(
        &mut self,
        arena: &mut Arena,
        output: Output,
        call: &Call<'_>,
    ) -> Result<Output, InterpreterError> {
        match self {
            Self::StSt(protocol) => protocol.raise(arena, output, call),
            Self::StK(protocol) => protocol.raise(arena, output, call),
            Self::StRedex(protocol) => protocol.raise(arena, output, call),
            Self::KSt(protocol) => protocol.raise(arena, output, call),
        }
    }

    pub(in crate::metacirc) fn host(
        &mut self,
        arena: &mut Arena,
        name: &str,
        values: &[Value],
    ) -> Result<HostCall, ExternError> {
        match self {
            Self::StSt(protocol) => protocol.host(arena, name, values),
            Self::StK(protocol) => protocol.host(arena, name, values),
            Self::StRedex(protocol) => protocol.host(arena, name, values),
            Self::KSt(protocol) => protocol.host(arena, name, values),
        }
    }

    pub(in crate::metacirc) fn reply(
        &mut self,
        arena: &mut Arena,
        call: &HostCall,
        eval: Eval<Output>,
    ) -> Eval<Vec<Value>, ExternError> {
        match self {
            Self::StSt(protocol) => protocol.reply(arena, call, eval),
            Self::StK(protocol) => protocol.reply(arena, call, eval),
            Self::StRedex(protocol) => protocol.reply(arena, call, eval),
            Self::KSt(protocol) => protocol.reply(arena, call, eval),
        }
    }

    pub(in crate::metacirc) fn clear(&mut self) {
        match self {
            Self::StSt(protocol) => protocol.clear(),
            Self::StK(protocol) => protocol.clear(),
            Self::StRedex(protocol) => protocol.clear(),
            Self::KSt(protocol) => protocol.clear(),
        }
    }

    /// The response a pure host call of spec-meta-k or spec-meta-redex
    /// gave to `request` before.
    pub(in crate::metacirc) fn host_response(&self, request: &str) -> Option<String> {
        match self {
            Self::StK(protocol) => protocol.host_response(request),
            Self::StRedex(protocol) => protocol.host_response(request),
            Self::StSt(_) | Self::KSt(_) => None,
        }
    }

    /// Caches the response of a pure host call of spec-meta-k or spec-meta-redex,
    /// if it succeeded.
    pub(in crate::metacirc) fn cache_host_response(&mut self, request: String, response: &str) {
        match self {
            Self::StK(protocol) => protocol.cache_host_response(request, response),
            Self::StRedex(protocol) => protocol.cache_host_response(request, response),
            Self::StSt(_) | Self::KSt(_) => {}
        }
    }
}

/// A specification entered from the level below, in the form that level accepts:
/// a configured level above the base, or the program above the top,
/// an AL or IL script or a K definition.
#[derive(Clone, Copy)]
pub(in crate::metacirc) enum Entered<'a> {
    Al(&'a al::Spec),
    Il(&'a il::Spec),
    /// A K definition, as spec-k's `kdefinition` value.
    Kore(Value),
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
