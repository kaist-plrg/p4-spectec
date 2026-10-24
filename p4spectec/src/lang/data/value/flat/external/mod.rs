//! Value encoding to and from JSON, arena-relative or arena-independent
//!
//! Relative mode saves slot 7 as the number 7;
//! independent mode saves its contents.
//! The caller supplies the matching arena, lifetime, and encoding
//! for relative data.
//! Independent payloads retain the owned value format without arena indices.
//! A case body is written as its filled notation in both modes,
//! so mixop handles never appear in a payload.
//! An extern value is written as its type name and its payload,
//! which its definition's codec encodes in the same mode;
//! decoding reads it through the definition the receiving runner registered.

use std::rc::Rc;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_state::{DeserializeState, SerializeState};

use crate::util::json::json;

use crate::lang::{
    common::source::Span,
    data::{
        arena::Arena, encoding::Encoding, intern::Interned, typ::TypKind,
        value::extern_value::TypDefs,
    },
};

use super::super::flat;

mod case;
mod field;

// = Configuration

/// The source arena and the representation of handles in JSON.
///
/// Relative handles are written as indices,
/// but a case body's notation is still expanded from the arena's mixops.
pub enum EncodeContext<'arena> {
    /// Write handles as indices.
    ArenaRelative(&'arena Arena),
    /// Resolve handles through this arena.
    ArenaIndependent(&'arena Arena),
}

impl<'arena> EncodeContext<'arena> {
    /// Selects the representation for this source arena.
    pub fn new(arena: &'arena Arena, encoding: Encoding) -> Self {
        match encoding {
            Encoding::ArenaRelative => Self::ArenaRelative(arena),
            Encoding::ArenaIndependent => Self::ArenaIndependent(arena),
        }
    }

    /// The arena handles are read from.
    pub(in crate::lang::data::value) fn arena(&self) -> &'arena Arena {
        match self {
            Self::ArenaRelative(arena) | Self::ArenaIndependent(arena) => arena,
        }
    }
}

/// The target arena, the representation of handles in JSON,
/// and the definitions extern values are decoded through.
///
/// Relative handles are read as indices,
/// but a case body's notation is still interned into the arena's mixops.
pub struct DecodeContext<'arena> {
    arena: &'arena mut Arena,
    encoding: Encoding,
    /// The receiving runner's definitions, issued by `arena`.
    typdefs: &'arena TypDefs,
}

impl<'arena> DecodeContext<'arena> {
    /// Selects the representation for this target arena and its definitions.
    pub fn new(arena: &'arena mut Arena, encoding: Encoding, typdefs: &'arena TypDefs) -> Self {
        Self { arena, encoding, typdefs }
    }

    /// The arena contents are interned into.
    pub(in crate::lang::data::value) fn arena_mut(&mut self) -> &mut Arena {
        self.arena
    }

    /// The definitions extern values are decoded through.
    pub(in crate::lang::data::value) fn typdefs(&self) -> &'arena TypDefs {
        self.typdefs
    }
}

// = Encode

// - Entry points

/// Encodes full contents, including annotations, without arena indices.
pub fn encode<T>(arena: &Arena, data: &T) -> Result<json, serde_json::Error>
where
    T: for<'arena> SerializeState<EncodeContext<'arena>> + ?Sized,
{
    encode_with(arena, Encoding::ArenaIndependent, data)
}

/// Encodes handles or flat bodies using the selected mode.
pub fn encode_with<T>(
    arena: &Arena,
    encoding: Encoding,
    data: &T,
) -> Result<json, serde_json::Error>
where
    T: for<'arena> SerializeState<EncodeContext<'arena>> + ?Sized,
{
    let ctx = EncodeContext::new(arena, encoding);
    match encoding {
        // Relative payloads are shallow; a stack-growing serializer suffices
        Encoding::ArenaRelative => data
            .serialize_state(serde_stacker::Serializer::new(serde_json::value::Serializer), &ctx),
        // Serialization recurses through independently encoded child values
        Encoding::ArenaIndependent => stacker::grow(32 * 1024 * 1024, || {
            data.serialize_state(serde_json::value::Serializer, &ctx)
        }),
    }
}

// - Interned values

impl SerializeState<EncodeContext<'_>> for Interned<flat::ValueKind> {
    fn serialize_state<S: Serializer>(
        &self,
        serializer: S,
        ctx: &EncodeContext<'_>,
    ) -> Result<S::Ok, S::Error> {
        match ctx {
            // Relative: the index; independent: recursively serialize the body
            EncodeContext::ArenaRelative(_) => self.index().serialize(serializer),
            EncodeContext::ArenaIndependent(arena) => {
                arena.value.kind(*self).serialize_state(serializer, ctx)
            }
        }
    }
}

impl SerializeState<EncodeContext<'_>> for Interned<Rc<TypKind>> {
    fn serialize_state<S: Serializer>(
        &self,
        serializer: S,
        ctx: &EncodeContext<'_>,
    ) -> Result<S::Ok, S::Error> {
        match ctx {
            EncodeContext::ArenaRelative(_) => self.index().serialize(serializer),
            EncodeContext::ArenaIndependent(arena) => arena.value.typ(*self).serialize(serializer),
        }
    }
}

impl SerializeState<EncodeContext<'_>> for Interned<Span> {
    fn serialize_state<S: Serializer>(
        &self,
        serializer: S,
        ctx: &EncodeContext<'_>,
    ) -> Result<S::Ok, S::Error> {
        match ctx {
            EncodeContext::ArenaRelative(_) => self.index().serialize(serializer),
            EncodeContext::ArenaIndependent(arena) => arena.value.span(*self).serialize(serializer),
        }
    }
}

// = Decode

// - Entry points

/// Decodes handles or flat bodies, interning contents in independent mode.
///
/// Extern values are read through the definitions in `typdefs`.
pub fn decode_with<'de, T>(
    arena: &'de mut Arena,
    encoding: Encoding,
    typdefs: &'de TypDefs,
    json: &'de json,
) -> Result<T, serde_json::Error>
where
    T: DeserializeState<'de, DecodeContext<'de>>,
{
    let mut ctx = DecodeContext::new(arena, encoding, typdefs);
    match encoding {
        // Relative payloads are shallow; a stack-growing deserializer suffices
        Encoding::ArenaRelative => {
            T::deserialize_state(&mut ctx, serde_stacker::Deserializer::new(json))
        }
        // Independent contents recurse deeply; grow the stack up front
        Encoding::ArenaIndependent => {
            stacker::grow(32 * 1024 * 1024, || T::deserialize_state(&mut ctx, json))
        }
    }
}

// - Interned values

impl<'de> DeserializeState<'de, DecodeContext<'_>> for Interned<flat::ValueKind> {
    fn deserialize_state<D: Deserializer<'de>>(
        ctx: &mut DecodeContext<'_>,
        deserializer: D,
    ) -> Result<Self, D::Error> {
        match ctx.encoding {
            // Relative: trust the index; independent: intern decoded children and body
            Encoding::ArenaRelative => u32::deserialize(deserializer).map(Self::from_index),
            Encoding::ArenaIndependent => {
                let kind = flat::ValueKind::deserialize_state(ctx, deserializer)?;
                let arena = ctx.arena_mut();
                arena
                    .value
                    .intern_kind(&arena.mixop, kind)
                    .map_err(::serde::de::Error::custom)
            }
        }
    }
}

impl<'de> DeserializeState<'de, DecodeContext<'_>> for Interned<Rc<TypKind>> {
    fn deserialize_state<D: Deserializer<'de>>(
        ctx: &mut DecodeContext<'_>,
        deserializer: D,
    ) -> Result<Self, D::Error> {
        match ctx.encoding {
            Encoding::ArenaRelative => u32::deserialize(deserializer).map(Self::from_index),
            Encoding::ArenaIndependent => {
                let typ = TypKind::deserialize(deserializer)?.into();
                ctx.arena
                    .value
                    .intern_typ(typ)
                    .map_err(::serde::de::Error::custom)
            }
        }
    }
}

impl<'de> DeserializeState<'de, DecodeContext<'_>> for Interned<Span> {
    fn deserialize_state<D: Deserializer<'de>>(
        ctx: &mut DecodeContext<'_>,
        deserializer: D,
    ) -> Result<Self, D::Error> {
        match ctx.encoding {
            Encoding::ArenaRelative => u32::deserialize(deserializer).map(Self::from_index),
            Encoding::ArenaIndependent => {
                let span = Span::deserialize(deserializer)?;
                ctx.arena
                    .value
                    .intern_span(span)
                    .map_err(::serde::de::Error::custom)
            }
        }
    }
}
