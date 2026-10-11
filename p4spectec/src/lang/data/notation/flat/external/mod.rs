//! Standalone mixop JSON with the same encoding modes as values
//!
//! Relative handles belong to the same live arena. Independent payloads
//! own their notation and atom spans and can be interned into any arena.

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_state::{DeserializeState, SerializeState};

use crate::util::json::json;

use crate::lang::data::encoding::Encoding;

use super::super::{MixopArena, flat, tree};

// = Configuration

/// The source arena and the representation of handles in JSON.
pub enum EncodeContext<'arena> {
    /// Write handles as indices.
    ArenaRelative(&'arena MixopArena),
    /// Resolve handles through this arena.
    ArenaIndependent(&'arena MixopArena),
}

impl<'arena> EncodeContext<'arena> {
    /// Selects the representation for this source arena.
    pub fn new(arena_mixop: &'arena MixopArena, encoding: Encoding) -> Self {
        match encoding {
            Encoding::ArenaRelative => Self::ArenaRelative(arena_mixop),
            Encoding::ArenaIndependent => Self::ArenaIndependent(arena_mixop),
        }
    }
}

/// The target arena and the representation of handles in JSON.
pub enum DecodeContext<'arena> {
    /// Read handles as indices.
    ArenaRelative(&'arena mut MixopArena),
    /// Intern contents into this arena.
    ArenaIndependent(&'arena mut MixopArena),
}

impl<'arena> DecodeContext<'arena> {
    /// Selects the representation for this target arena.
    pub fn new(arena_mixop: &'arena mut MixopArena, encoding: Encoding) -> Self {
        match encoding {
            Encoding::ArenaRelative => Self::ArenaRelative(arena_mixop),
            Encoding::ArenaIndependent => Self::ArenaIndependent(arena_mixop),
        }
    }
}

// = Encode

// - Entry points

/// Encodes full contents, including annotations, without arena indices.
pub fn encode<T>(arena_mixop: &MixopArena, data: &T) -> Result<json, serde_json::Error>
where
    T: for<'arena> SerializeState<EncodeContext<'arena>> + ?Sized,
{
    encode_with(arena_mixop, Encoding::ArenaIndependent, data)
}

/// Encodes handles or flat bodies using the selected mode.
pub fn encode_with<T>(
    arena_mixop: &MixopArena,
    encoding: Encoding,
    data: &T,
) -> Result<json, serde_json::Error>
where
    T: for<'arena> SerializeState<EncodeContext<'arena>> + ?Sized,
{
    let ctx = EncodeContext::new(arena_mixop, encoding);
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

// - Interned mixops

impl SerializeState<EncodeContext<'_>> for flat::Mixop {
    fn serialize_state<S: Serializer>(
        &self,
        serializer: S,
        ctx: &EncodeContext<'_>,
    ) -> Result<S::Ok, S::Error> {
        match ctx {
            EncodeContext::ArenaRelative(_) => self.index().serialize(serializer),
            EncodeContext::ArenaIndependent(arena_mixop) => {
                self.into_tree(arena_mixop).serialize(serializer)
            }
        }
    }
}

// = Decode

// - Entry points

/// Decodes handles or flat bodies, interning contents in independent mode.
pub fn decode_with<'de, T>(
    arena_mixop: &'de mut MixopArena,
    encoding: Encoding,
    json: &'de json,
) -> Result<T, serde_json::Error>
where
    T: DeserializeState<'de, DecodeContext<'de>>,
{
    let mut ctx = DecodeContext::new(arena_mixop, encoding);
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

// - Interned mixops

impl<'de> DeserializeState<'de, DecodeContext<'_>> for flat::Mixop {
    fn deserialize_state<D: Deserializer<'de>>(
        ctx: &mut DecodeContext<'_>,
        deserializer: D,
    ) -> Result<Self, D::Error> {
        match ctx {
            DecodeContext::ArenaRelative(_) => u32::deserialize(deserializer).map(Self::from_index),
            DecodeContext::ArenaIndependent(arena_mixop) => {
                let mixop_tree = tree::Mixop::deserialize(deserializer)?;
                mixop_tree
                    .into_flat(arena_mixop)
                    .map_err(serde::de::Error::custom)
            }
        }
    }
}
