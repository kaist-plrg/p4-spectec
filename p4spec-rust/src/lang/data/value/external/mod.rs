//! Value encoding to and from JSON, arena-relative or arena-independent
//!
//! Relative mode saves slot 7 as the number 7;
//! independent mode saves its contents.
//! The caller supplies the matching arena, lifetime, and encoding
//! for relative data.
//! Independent payloads are trees (`indep`) that any arena can intern.

pub mod indep;

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_state::{DeserializeState, SerializeState};

use crate::util::json::json;

use crate::lang::{common::source::Span, data::typ::TypKind};

use super::{Interned, ValueArena, ValueKind};

// = Configuration

/// Relative payloads belong to one live arena;
/// independent payloads carry contents.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum Encoding {
    /// Handles as indices; readable only with the same arena.
    #[default]
    ArenaRelative,
    /// Full contents; readable anywhere.
    ArenaIndependent,
}

impl std::str::FromStr for Encoding {
    type Err = String;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        match text {
            "arena-relative" => Ok(Self::ArenaRelative),
            "arena-independent" => Ok(Self::ArenaIndependent),
            _ => Err("expected arena-relative or arena-independent".to_owned()),
        }
    }
}

impl std::fmt::Display for Encoding {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.write_str(match self {
            Self::ArenaRelative => "arena-relative",
            Self::ArenaIndependent => "arena-independent",
        })
    }
}

/// What an encoder needs: nothing for relative, the arena for independent.
pub enum EncodeContext<'arena> {
    /// Write handles as indices.
    ArenaRelative,
    /// Resolve handles through this arena.
    ArenaIndependent(&'arena ValueArena),
}

impl<'arena> EncodeContext<'arena> {
    /// The context for an encoding.
    pub fn new(arena: &'arena ValueArena, encoding: Encoding) -> Self {
        match encoding {
            Encoding::ArenaRelative => Self::ArenaRelative,
            Encoding::ArenaIndependent => Self::ArenaIndependent(arena),
        }
    }
}

/// What a decoder needs: nothing for relative, a mutable arena for independent.
pub enum DecodeContext<'arena> {
    /// Read handles as indices.
    ArenaRelative,
    /// Intern contents into this arena.
    ArenaIndependent(&'arena mut ValueArena),
}

impl<'arena> DecodeContext<'arena> {
    /// The context for an encoding.
    pub fn new(arena: &'arena mut ValueArena, encoding: Encoding) -> Self {
        match encoding {
            Encoding::ArenaRelative => Self::ArenaRelative,
            Encoding::ArenaIndependent => Self::ArenaIndependent(arena),
        }
    }
}

// = Encode

// - Entry points

/// Keeps the arena-independent JSON and annotation contract.
pub fn encode<T>(arena: &ValueArena, data: &T) -> Result<json, serde_json::Error>
where
    T: for<'arena> SerializeState<EncodeContext<'arena>> + ?Sized,
{
    encode_with(arena, Encoding::ArenaIndependent, data)
}

/// Encodes with the chosen encoding.
pub fn encode_with<T>(
    arena: &ValueArena,
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

impl SerializeState<EncodeContext<'_>> for Interned<ValueKind> {
    fn serialize_state<S: Serializer>(
        &self,
        serializer: S,
        ctx: &EncodeContext<'_>,
    ) -> Result<S::Ok, S::Error> {
        match ctx {
            // Relative: the index; independent: the body as a tree
            EncodeContext::ArenaRelative => self.index().serialize(serializer),
            EncodeContext::ArenaIndependent(arena) => {
                indep::ValueKind::from_arena(arena, arena.values.get(*self)).serialize(serializer)
            }
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
            EncodeContext::ArenaRelative => self.index().serialize(serializer),
            EncodeContext::ArenaIndependent(arena) => arena.types.get(*self).serialize(serializer),
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
            EncodeContext::ArenaRelative => self.index().serialize(serializer),
            EncodeContext::ArenaIndependent(arena) => arena.spans.get(*self).serialize(serializer),
        }
    }
}

// = Decode

// - Entry points

/// Decodes with the chosen encoding, interning into `arena` when independent.
pub fn decode_with<'de, T>(
    arena: &'de mut ValueArena,
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

impl<'de> DeserializeState<'de, DecodeContext<'_>> for Interned<ValueKind> {
    fn deserialize_state<D: Deserializer<'de>>(
        ctx: &mut DecodeContext<'_>,
        deserializer: D,
    ) -> Result<Self, D::Error> {
        match ctx {
            // Relative: trust the index; independent: intern the tree
            DecodeContext::ArenaRelative => u32::deserialize(deserializer).map(Self::from_index),
            DecodeContext::ArenaIndependent(arena) => {
                let kind = indep::ValueKind::deserialize(deserializer)?
                    .into_arena(arena)
                    .map_err(::serde::de::Error::custom)?;
                arena
                    .values
                    .intern(kind)
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
            DecodeContext::ArenaRelative => u32::deserialize(deserializer).map(Self::from_index),
            DecodeContext::ArenaIndependent(arena) => {
                let typ = TypKind::deserialize(deserializer)?.into();
                arena.types.intern(typ).map_err(::serde::de::Error::custom)
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
            DecodeContext::ArenaRelative => u32::deserialize(deserializer).map(Self::from_index),
            DecodeContext::ArenaIndependent(arena) => {
                let span = Span::deserialize(deserializer)?;
                arena.spans.intern(span).map_err(::serde::de::Error::custom)
            }
        }
    }
}
