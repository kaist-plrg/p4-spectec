//! Value encoding to and from JSON, arena-relative or arena-independent
//!
//! Relative mode saves slot 7 as the number 7;
//! independent mode saves its contents.
//! The caller supplies the matching arena, lifetime, and encoding
//! for relative data.
//! Independent payloads are value trees (`tree`) that any arena can intern.
//! A case body is written as its filled notation in both modes,
//! so mixop handles never appear in a payload.

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_state::{DeserializeState, SerializeState};

use crate::util::json::json;

use crate::lang::{
    common::source::Span,
    data::{arena::Arena, encoding::Encoding, intern::Interned, typ::TypKind},
};

use super::super::{flat, tree};

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
    fn arena(&self) -> &'arena Arena {
        match self {
            Self::ArenaRelative(arena) | Self::ArenaIndependent(arena) => arena,
        }
    }
}

/// The target arena and the representation of handles in JSON.
///
/// Relative handles are read as indices,
/// but a case body's notation is still interned into the arena's mixops.
pub enum DecodeContext<'arena> {
    /// Read handles as indices.
    ArenaRelative(&'arena mut Arena),
    /// Intern contents into this arena.
    ArenaIndependent(&'arena mut Arena),
}

impl<'arena> DecodeContext<'arena> {
    /// Selects the representation for this target arena.
    pub fn new(arena: &'arena mut Arena, encoding: Encoding) -> Self {
        match encoding {
            Encoding::ArenaRelative => Self::ArenaRelative(arena),
            Encoding::ArenaIndependent => Self::ArenaIndependent(arena),
        }
    }

    /// The arena contents are interned into.
    fn arena_mut(&mut self) -> &mut Arena {
        match self {
            Self::ArenaRelative(arena) | Self::ArenaIndependent(arena) => arena,
        }
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
        // Conversion, serde, and destruction all recurse through the contents
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
            // Relative: the index; independent: the body as a tree
            EncodeContext::ArenaRelative(_) => self.index().serialize(serializer),
            EncodeContext::ArenaIndependent(arena) => arena
                .value
                .kind(*self)
                .into_tree(arena)
                .serialize(serializer),
        }
    }
}

impl SerializeState<EncodeContext<'_>> for Interned<TypKind> {
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
pub fn decode_with<'de, T>(
    arena: &'de mut Arena,
    encoding: Encoding,
    json: &'de json,
) -> Result<T, serde_json::Error>
where
    T: DeserializeState<'de, DecodeContext<'de>>,
{
    let mut ctx = DecodeContext::new(arena, encoding);
    match encoding {
        // Relative payloads are shallow; a stack-growing deserializer suffices
        Encoding::ArenaRelative => {
            T::deserialize_state(&mut ctx, serde_stacker::Deserializer::new(json))
        }
        // Independent trees recurse deeply; grow the stack up front
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
        match ctx {
            // Relative: trust the index; independent: intern the tree
            DecodeContext::ArenaRelative(_) => u32::deserialize(deserializer).map(Self::from_index),
            DecodeContext::ArenaIndependent(arena) => {
                let kind = tree::ValueKind::deserialize(deserializer)?
                    .into_flat(arena)
                    .map_err(::serde::de::Error::custom)?;
                arena
                    .value
                    .intern_kind(&arena.mixop, kind)
                    .map_err(::serde::de::Error::custom)
            }
        }
    }
}

impl<'de> DeserializeState<'de, DecodeContext<'_>> for Interned<TypKind> {
    fn deserialize_state<D: Deserializer<'de>>(
        ctx: &mut DecodeContext<'_>,
        deserializer: D,
    ) -> Result<Self, D::Error> {
        match ctx {
            DecodeContext::ArenaRelative(_) => u32::deserialize(deserializer).map(Self::from_index),
            DecodeContext::ArenaIndependent(arena) => {
                let typ = TypKind::deserialize(deserializer)?.into();
                arena
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
        match ctx {
            DecodeContext::ArenaRelative(_) => u32::deserialize(deserializer).map(Self::from_index),
            DecodeContext::ArenaIndependent(arena) => {
                let span = Span::deserialize(deserializer)?;
                arena
                    .value
                    .intern_span(span)
                    .map_err(::serde::de::Error::custom)
            }
        }
    }
}
